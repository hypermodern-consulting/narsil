{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // layout // import
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He saw the edges of the thing, the way it fit into the world."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The import walker: a pure traversal that finds `import ./path` (and
--   `builtins.import`, `import ./path (args)`) call sites in a parsed
--   expression and resolves each to a filesystem path. No IO, no inference —
--   just the AST. This is the leaf the graph builder ('Narsil.Layout.Graph')
--   stands on to discover edges.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Import (
  -- * The edge
  Import (..),

  -- * Extraction
  findImports,
  checkImportBuiltin,
)
where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Span (Span)
import Narsil.Syntax.Annotation (srcSpanToSpan, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types hiding (Binding)
import Nix.Expr.Types qualified as Nix
import Nix.Expr.Types.Annotated
import Nix.Utils qualified as NixPath
import System.FilePath (normalise, (</>))

-- | one resolved `import` edge: where it points, how it was written, its args, its span
data Import = Import
  { impPath :: !FilePath
  , impRawPath :: !Text
  , impArgs :: !(Maybe NExprLoc)
  , impSpan :: !Span
  }
  deriving (Show)

-- ── import finding: walk AST for `import ./path` calls ───────────

-- | walk an entire expression tree looking for import calls
findImports :: FilePath -> NExprLoc -> [Import]
findImports baseDir = walkExpr
 where
  walkExpr :: NExprLoc -> [Import]
  walkExpr (LayerAnn srcSpan (NApp func arg)) = processApplication baseDir srcSpan func arg walkExpr
  walkExpr (Layer (NLet bindings body)) = concatMap walkBinding bindings ++ walkExpr body
  walkExpr (Layer (NSet _ bindings)) = concatMap walkBinding bindings
  walkExpr (Layer (NIf cond thenBranch elseBranch)) =
    walkExpr cond ++ walkExpr thenBranch ++ walkExpr elseBranch
  walkExpr (Layer (NWith scope body)) = walkExpr scope ++ walkExpr body
  walkExpr (Layer (NAssert cond body)) = walkExpr cond ++ walkExpr body
  walkExpr (Layer (NAbs _ body)) = walkExpr body
  walkExpr (Layer (NList elements)) = concatMap walkExpr elements
  walkExpr (Layer (NSelect _ base _)) = walkExpr base
  walkExpr (Layer (NBinary _ left right)) = walkExpr left ++ walkExpr right
  walkExpr (Layer (NUnary _ operand)) = walkExpr operand
  walkExpr _ = []

  walkBinding :: Nix.Binding NExprLoc -> [Import]
  walkBinding (Nix.NamedVar _ expr _) = walkExpr expr
  walkBinding (Nix.Inherit (Just scope) _ _) = walkExpr scope
  walkBinding (Nix.Inherit Nothing _ _) = []

-- ── import application analysis ──────────────────────────────────

{- | given an application node, determine if it's an import and extract its parts
handles: import ./path, builtins.import ./path, import ./path (arg)
-}
processApplication ::
  FilePath -> SrcSpan -> NExprLoc -> NExprLoc -> (NExprLoc -> [Import]) -> [Import]
processApplication baseDir srcSpan func arg continue
  | Just (rawPath, Nothing) <- unwrapImportExpression func =
      makeImport baseDir rawPath (Just arg) srcSpan ++ continue arg
  | Just (rawPath, Just inner) <- unwrapImportExpression func =
      makeImport baseDir rawPath (Just arg) srcSpan ++ continue inner ++ continue arg
  | Just () <- checkImportBuiltin func = makeImport baseDir (extractImportPath arg) Nothing srcSpan
  | otherwise = continue func ++ continue arg

-- | check if an expression is literally the `import` builtin (or builtins.import)
checkImportBuiltin :: NExprLoc -> Maybe ()
checkImportBuiltin (Layer (NSym name))
  | nixVarNameText name == "import" = Just ()
checkImportBuiltin (Layer (NSelect _ _ (attr :| rest)))
  | nixVarNameText (nixKeyName (last (attr : rest))) == "import" = Just ()
checkImportBuiltin _ = Nothing

{- | try to unwrap a nested import expression: import (./path + args)
returns (path, maybe inner-arg-expr)
-}
unwrapImportExpression :: NExprLoc -> Maybe (Text, Maybe NExprLoc)
unwrapImportExpression (Layer (NApp func pathExpr)) = unwrapImportHelper func pathExpr
unwrapImportExpression _ = Nothing

-- | helper to unwrap import at the head of a chain of applications
unwrapImportHelper :: NExprLoc -> NExprLoc -> Maybe (Text, Maybe NExprLoc)
unwrapImportHelper func pathExpr
  | Layer (NSym name) <- func
  , nixVarNameText name == "import" =
      Just (extractImportPath pathExpr, Nothing)
  | Just (path, Nothing) <- unwrapImportExpression func
  , not (T.null path) =
      Just (path, Just pathExpr)
  | Just () <- checkImportBuiltin func = Just (extractImportPath pathExpr, Nothing)
  | otherwise = Nothing

-- | extract the file path text from an import argument expression
extractImportPath :: NExprLoc -> Text
extractImportPath (Layer (NLiteralPath (NixPath.Path p))) = T.pack p
extractImportPath (Layer (NStr (DoubleQuoted [Plain t]))) = t
extractImportPath (Layer (NStr (Indented _ [Plain t]))) = t
extractImportPath _ = ""

-- ── key & name helpers ───────────────────────────────────────────

nixKeyName :: NKeyName r -> VarName
nixKeyName (StaticKey key) = key
nixKeyName (DynamicKey _) = VarName ""

nixVarNameText :: VarName -> Text
nixVarNameText = coerce

-- | construct an Import record from a raw path string and source location
makeImport :: FilePath -> Text -> Maybe NExprLoc -> SrcSpan -> [Import]
makeImport baseDirectory rawPath arguments srcSpan
  | T.null rawPath = []
  | otherwise =
      let resolvedPath = resolveImportPath baseDirectory (T.unpack rawPath)
       in [ Import
              { impPath = resolvedPath
              , impRawPath = rawPath
              , impArgs = arguments
              , impSpan = srcSpanToSpan srcSpan
              }
          ]

-- | resolve a relative or absolute import path against the base directory
resolveImportPath :: FilePath -> FilePath -> FilePath
resolveImportPath _ path@('/' : _) = path
resolveImportPath baseDir path = normalise (baseDir </> path)
