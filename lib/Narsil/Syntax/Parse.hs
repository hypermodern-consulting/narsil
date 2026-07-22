{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                 // nix // parsing
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Back into the living room, and amazed, somehow, that he hadn't moved;
--    expecting him to jump up, hello, waving a few centimeters of trick wire.
--    She removed his shoes, looked inside, felt the lining. Nothing. 'Don't
--    do this to me.' And back into the bedroom. The narrow closet. Brushing
--    aside a clatter of cheap white plastic hangers, a limp shroud of
--    drycleaner's plastic. Dragging the stained bedslab over and standing on
--    it, her heels sinking into the foam, to slide her hands the length of a
--    pressboard shelf, and find, in the far corner, a hard little fold of
--    paper, rectangular and blue."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // nix // parse // extract
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Syntax.Parse (
  -- * Parsing
  parseNixFile,
  parseNixExpr,
  parseNix,

  -- * Extraction
  extractBashScripts,
  BashScript (..),
  Interpolation (..),

  -- * Low-level
  findShellScriptCalls,
  ShellScriptCall (..),
  extractString,
  extractPartsWithInterps,
)
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span (Span (..))
import Narsil.Syntax.Annotation (
  normalizeStaticKeys,
  toSpan,
  varNameText,
  pattern Layer,
  pattern LayerAnn,
 )
import Nix.Atoms (NAtom (..))
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Parser qualified

-- | A bash script extracted from a Nix file
data BashScript = BashScript
  { bsName :: !Text
  , bsContent :: !Text
  , bsInterpolations :: ![Interpolation]
  , bsSpan :: !Span
  }
  deriving (Eq, Show)

-- | An interpolation site in a bash string
data Interpolation = Interpolation
  { intExpr :: !Text
  , intIsStorePath :: !Bool
  , intSpan :: !Span
  }
  deriving (Eq, Show)

-- | A writeShellScript* call found in Nix
data ShellScriptCall = ShellScriptCall
  { sscFunction :: !Text
  , sscName :: !Text
  , sscBody :: !NExprLoc
  , sscSpan :: !Span
  }
  deriving (Show)

{- | Parse a Nix file and return the annotated AST.
n.b. catches StackOverflow from the parser (megaparsec recursion blows the stack
on adversarial input like deeply-nested parens); routes through Safety wrappers.
-}
parseNixFile :: FilePath -> IO (Either Text NExprLoc)
parseNixFile path = either (Left . Safety.renderSafetyError) Right <$> Safety.safeParseNixFile path

{- | Parse a Nix expression from text.
n.b. this is the safe variant — exception-handling lives in Safety.
-}
parseNixExpr :: Text -> Either Text NExprLoc
parseNixExpr src =
  either (Left . Safety.renderSafetyError) Right (Safety.safeAnalyze =<< parseNoIO src)
 where
  parseNoIO :: Text -> Either Safety.SafetyError NExprLoc
  parseNoIO s =
    either
      (Left . Safety.SafetyParseFailed . T.pack . show)
      (Right . normalizeStaticKeys)
      (Nix.Parser.parseNixTextLoc s)

-- | Parse a Nix expression from text with filepath for error context
parseNix :: FilePath -> Text -> Either Text NExprLoc
parseNix _path = parseNixExpr

-- | Extract all bash scripts from a Nix file
extractBashScripts :: FilePath -> IO (Either Text [BashScript])
extractBashScripts path = do
  result <- parseNixFile path
  pure (concatMap extractFromCall . findShellScriptCalls <$> result)

-- | Extract bash content from a shell script call
extractFromCall :: ShellScriptCall -> [BashScript]
extractFromCall ssc = maybe [] build (extractString (sscBody ssc))
 where
  build (content, interps, span') =
    [ BashScript
        { bsName = sscName ssc
        , bsContent = content
        , bsInterpolations = interps
        , bsSpan = span'
        }
    ]

-- | Extract string content and interpolations from an expression
extractString :: NExprLoc -> Maybe (Text, [Interpolation], Span)
extractString (LayerAnn srcSpan (NStr (DoubleQuoted parts))) = Just (withSpan srcSpan parts)
extractString (LayerAnn srcSpan (NStr (Indented _ parts))) = Just (withSpan srcSpan parts)
extractString _ = Nothing

-- | The content, interpolations, and span of a string's parts.
withSpan :: SrcSpan -> [Antiquoted Text NExprLoc] -> (Text, [Interpolation], Span)
withSpan srcSpan parts =
  let (content, interps) = extractPartsWithInterps parts
   in (content, interps, toSpan srcSpan Nothing)

-- | Generate a placeholder string for an interpolation site
injectPlaceholders :: Bool -> Int -> Text
injectPlaceholders isStore n
  | isStore = "/nix/store/__nix_compile_interp_" <> T.pack (show n) <> "__"
  | otherwise = "@__nix_compile_interp_" <> T.pack (show n) <> "__@"

-- | Extract interpolation data from an antiquoted expression
extractInterpolations :: Int -> NExprLoc -> (Text, Interpolation)
extractInterpolations n expr =
  let isStore = isStorePathExpr expr
   in ( injectPlaceholders isStore n
      , Interpolation
          { intExpr = prettyExpr expr
          , intIsStorePath = isStore
          , intSpan = exprSpan expr
          }
      )

{- | Extract text and interpolations from string parts.

We replace interpolations with stable placeholders so downstream bash analysis can:
  * treat "known store-path" interpolations as store paths (by prefixing /nix/store/)
  * treat "unknown" interpolations as explicit placeholders (by prefixing @...@)
-}
extractPartsWithInterps :: [Antiquoted Text NExprLoc] -> (Text, [Interpolation])
extractPartsWithInterps = go 0
 where
  go _ [] = ("", [])
  go n (part : rest) = combine part
   where
    (restText, restInterps) = go (bump part) rest
    combine (Plain txt) = (txt <> restText, restInterps)
    combine EscapedNewline = ("\n" <> restText, restInterps)
    combine (Antiquoted expr) =
      let (placeholder, interp) = extractInterpolations n expr
       in (placeholder <> restText, interp : restInterps)
    -- the interpolation counter advances only past an antiquotation
    bump (Antiquoted _) = n + 1
    bump _ = n

{- | Check if an expression looks like a store path access
e.g., ${pkgs.curl} or ${lib.getExe pkgs.ripgrep}
-}
isStorePathExpr :: NExprLoc -> Bool
isStorePathExpr (Layer (NSelect _ base (k :| _))) =
  isPackageBase base || keyTextIs "pkgs" k || keyTextIs "lib" k
isStorePathExpr (Layer (NApp func arg)) = isStorePathExpr func || isStorePathExpr arg
isStorePathExpr (Layer (NSym name)) = isLikelyPackageVar (varNameText name)
isStorePathExpr (Layer (NLiteralPath p)) = "/nix/store" `T.isPrefixOf` T.pack (show p)
isStorePathExpr _ = False

isPackageBase :: NExprLoc -> Bool
isPackageBase (Layer (NSym n)) = varNameText n `elem` ["pkgs", "lib"]
isPackageBase (Layer (NSelect _ b _)) = isPackageBase b
isPackageBase _ = False

keyTextIs :: Text -> NKeyName r -> Bool
keyTextIs name (StaticKey k) = varNameText k == name
keyTextIs _ (DynamicKey _) = False

isLikelyPackageVar :: Text -> Bool
isLikelyPackageVar name =
  T.isPrefixOf "pkgs" name
    || T.isPrefixOf "lib" name
    || T.isSuffixOf "Pkg" name
    || T.isSuffixOf "Package" name
    || name == "narsil"
    || name == "nix-compile"

-- | Get a simple text representation of an expression
prettyExpr :: NExprLoc -> Text
prettyExpr (Layer (NSym name)) = varNameText name
prettyExpr (Layer (NSelect _ base (attr :| rest))) =
  prettyExpr base <> "." <> T.intercalate "." (map keyText (attr : rest))
prettyExpr (Layer (NApp func arg)) = prettyExpr func <> " " <> prettyExpr arg
prettyExpr (Layer (NConstant (NInt n))) = T.pack (show n)
prettyExpr (Layer (NConstant (NFloat f))) = T.pack (show f)
prettyExpr (Layer (NConstant (NBool b))) = if b then "true" else "false"
prettyExpr (Layer (NConstant NNull)) = "null"
prettyExpr (Layer (NStr _)) = "<string>"
prettyExpr (Layer (NList _)) = "<list>"
prettyExpr (Layer (NSet _ _)) = "<attrset>"
prettyExpr (Layer (NLiteralPath p)) = T.pack (show p)
prettyExpr (Layer (NEnvPath p)) = "<" <> T.pack (show p) <> ">"
prettyExpr _ = "<expr>"

-- | Get the source span of an expression
exprSpan :: NExprLoc -> Span
exprSpan (LayerAnn srcSpan _) = toSpan srcSpan Nothing

-- | Find all writeShellScript* calls in an expression
findShellScriptCalls :: NExprLoc -> [ShellScriptCall]
findShellScriptCalls = walkExpression

-- | Walk the Nix AST, collecting shell script calls
walkExpression :: NExprLoc -> [ShellScriptCall]
walkExpression expr = maybe (walkSubExprs expr) pure (extractScriptCall expr)

-- | Walk sub-expressions of a node
walkSubExprs :: NExprLoc -> [ShellScriptCall]
walkSubExprs (Layer (NList xs)) = concatMap walkExpression xs
walkSubExprs (Layer (NSet _ bindings)) = concatMap walkBinding bindings
walkSubExprs (Layer (NLet bindings body)) = concatMap walkBinding bindings ++ walkExpression body
walkSubExprs (Layer (NIf cond t f)) = walkExpression cond ++ walkExpression t ++ walkExpression f
walkSubExprs (Layer (NWith scope body)) = walkExpression scope ++ walkExpression body
walkSubExprs (Layer (NAssert cond body)) = walkExpression cond ++ walkExpression body
walkSubExprs (Layer (NAbs _ body)) = walkExpression body
walkSubExprs (Layer (NApp f x)) = walkExpression f ++ walkExpression x
walkSubExprs (Layer (NSelect alt base _)) = walkExpression base ++ maybe [] walkExpression alt
walkSubExprs (Layer (NHasAttr base _)) = walkExpression base
walkSubExprs (Layer (NUnary _ x)) = walkExpression x
walkSubExprs (Layer (NBinary _ x y)) = walkExpression x ++ walkExpression y
-- leaves and holes carry no sub-expressions to walk
walkSubExprs _ = []

-- | Process a binding node
walkBinding :: Binding NExprLoc -> [ShellScriptCall]
walkBinding (NamedVar _ expr _) = walkExpression expr
walkBinding Inherit{} = []

-- | Check if a function name is a writeShellScript variant
isShellScriptFunction :: Text -> Bool
isShellScriptFunction name =
  name == "writeShellScript"
    || name == "writeShellScriptBin"
    || name == "writeScript"
    || name == "writeScriptBin"
    || name == "writeShellApplication"

-- | Unwrap nested applications to find the function name and all arguments
unwrapApp :: NExprLoc -> [NExprLoc] -> Maybe (Text, [NExprLoc])
unwrapApp (Layer (NApp func arg)) args = unwrapApp func (arg : args)
unwrapApp (Layer (NSym name)) args = Just (varNameText name, args)
unwrapApp (Layer (NSelect _ _ (attr :| rest))) args = Just (keyText (last (attr : rest)), args)
unwrapApp _ _ = Nothing

-- | Extract key name text from a key node
keyText :: NKeyName NExprLoc -> Text
keyText (StaticKey k) = varNameText k
keyText (DynamicKey _) = ""

-- | Extract name and text from a record: { name = "foo"; text = ''body''; }
extractFromRecord :: NExprLoc -> Maybe (Text, NExprLoc)
extractFromRecord (Layer (NSet _ bindings)) = liftA2 (,) nameVal textVal
 where
  nameVal = findBinding "name" bindings >>= extractStringLit
  textVal = findBinding "text" bindings
extractFromRecord _ = Nothing

-- | Find a binding by name in a binding list
findBinding :: Text -> [Binding NExprLoc] -> Maybe NExprLoc
findBinding name = foldr check Nothing
 where
  check (NamedVar (StaticKey k :| []) expr _) acc
    | varNameText k == name = Just expr
    | otherwise = acc
  check _ acc = acc

-- | Extract a string literal from an expression
extractStringLit :: NExprLoc -> Maybe Text
extractStringLit (Layer (NStr (DoubleQuoted [Plain t]))) = Just t
extractStringLit (Layer (NStr (Indented _ [Plain t]))) = Just t
extractStringLit _ = Nothing

-- | Extract a ShellScriptCall from an expression node if it matches
extractScriptCall :: NExprLoc -> Maybe ShellScriptCall
extractScriptCall expr@(LayerAnn srcSpan e)
  | NApp{} <- e = processApp (unwrapApp expr [])
  | otherwise = Nothing
 where
  processApp (Just (name, args))
    | not (isShellScriptFunction name) = Nothing
    | name `elem` positionalFuncs = extractPositional name args
    | name == "writeShellApplication" = extractShellApp name args
    | otherwise = Nothing
  processApp Nothing = Nothing

  extractPositional name args
    | [nameArg, bodyArg] <- args =
        fmap
          (\n -> ShellScriptCall name n bodyArg (toSpan srcSpan Nothing))
          (extractStringLit nameArg)
    | otherwise = Nothing

  extractShellApp name args
    | [recordArg] <- args = fmap (build name) (extractFromRecord recordArg)
    | otherwise = Nothing

  build name (n, body) =
    ShellScriptCall
      { sscFunction = name
      , sscName = n
      , sscBody = body
      , sscSpan = toSpan srcSpan Nothing
      }

  positionalFuncs =
    [ "writeShellScript"
    , "writeShellScriptBin"
    , "writeScript"
    , "writeScriptBin"
    ]
