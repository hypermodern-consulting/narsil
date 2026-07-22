{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                         // safety
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He was good as new. How good was that?"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // single source of safety
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Safety (
  -- * Constants
  maxRecursionDepth,

  -- * Errors
  SafetyError (..),
  DepthError (..),
  renderSafetyError,

  -- * Depth analysis
  analyzeDepth,
  analyzeDepthWith,

  -- * Exception-safe wrappers
  safeParseNixText,
  safeParseNixFile,
  safeReadFile,
  safeIO,
  safeIOWith,

  -- * Combined analysis
  safeAnalyze,
)
where

import Control.Exception (Exception, SomeException, evaluate, fromException, try)
import Control.Exception qualified as Exc
import Data.Fix (Fix (..))
import Data.Functor.Compose (Compose (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.Syntax.Annotation (normalizeStaticKeys)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated (AnnUnit (..), NExprLoc)
import Nix.Parser (parseNixFileLoc, parseNixTextLoc)
import Nix.Utils (Path (..))
import System.IO.Error (isDoesNotExistError)

-- ── single-source depth constant ──────────────────────────────────
-- Every guard in the codebase reads from here. Do not duplicate.

-- | the single AST-depth limit every guard in the codebase reads from.
maxRecursionDepth :: Int
maxRecursionDepth = 200

-- ── errors ────────────────────────────────────────────────────────

-- | a depth-limit breach: the offending depth and the constructor tag where it tripped.
data DepthError = DepthError
  { deDepth :: !Int
  , deContext :: !Text
  }
  deriving (Eq, Show)

-- | any failure the safety layer converts an exception or limit breach into.
data SafetyError
  = SafetyDepthExceeded !DepthError
  | SafetyParseFailed !Text
  | SafetyStackOverflow
  | SafetyInternalException !Text
  | SafetyIOError !Text
  deriving (Eq, Show)

instance Exception SafetyError

-- | render a 'SafetyError' as a one-line human-readable message.
renderSafetyError :: SafetyError -> Text
renderSafetyError (SafetyDepthExceeded (DepthError d ctx)) =
  "depth limit exceeded ("
    <> T.pack (show d)
    <> " > "
    <> T.pack (show maxRecursionDepth)
    <> ") at "
    <> ctx
renderSafetyError (SafetyParseFailed t) = "parse error: " <> t
renderSafetyError SafetyStackOverflow = "stack overflow — input too deeply nested for parser"
renderSafetyError (SafetyInternalException t) = "internal exception: " <> t
renderSafetyError (SafetyIOError t) = "I/O error: " <> t

-- ── depth analysis ────────────────────────────────────────────────
-- Walks EVERY Fix unwrap. Cannot be bypassed by NWith/NStr/NSynHole.
-- Strictly counts depth on every recursion regardless of constructor.

-- | check an AST against 'maxRecursionDepth', failing on the first node that exceeds it.
analyzeDepth :: NExprLoc -> Either DepthError ()
analyzeDepth = analyzeDepthWith maxRecursionDepth

-- | 'analyzeDepth' with an explicit depth limit; walks every 'Fix' unwrap, unbypassable.
analyzeDepthWith :: Int -> NExprLoc -> Either DepthError ()
analyzeDepthWith limit = go 0
 where
  go :: Int -> NExprLoc -> Either DepthError ()
  go !d (Fix (Compose (AnnUnit _ expr)))
    | d > limit = Left (DepthError d (constructorTag expr))
    | otherwise = walk (d + 1) expr

  walk :: Int -> NExprF NExprLoc -> Either DepthError ()
  walk _ (NConstant _) = Right ()
  walk _ (NSym _) = Right ()
  walk _ (NLiteralPath _) = Right ()
  walk _ (NEnvPath _) = Right ()
  walk _ (NSynHole _) = Right ()
  walk d (NStr (DoubleQuoted parts)) = mapM_ (goAnti d) parts
  walk d (NStr (Indented _ parts)) = mapM_ (goAnti d) parts
  walk d (NList xs) = mapM_ (go d) xs
  walk d (NSet _ bs) = mapM_ (goBinding d) bs
  walk d (NLet bs body) = mapM_ (goBinding d) bs >> go d body
  walk d (NIf c t e) = go d c >> go d t >> go d e
  walk d (NWith s b) = go d s >> go d b
  walk d (NAssert c b) = go d c >> go d b
  walk d (NAbs p b) = goParams d p >> go d b
  walk d (NApp f a) = go d f >> go d a
  walk d (NSelect alt b path) = go d b >> mapM_ (go d) alt >> goPath d path
  walk d (NHasAttr b path) = go d b >> goPath d path
  walk d (NUnary _ x) = go d x
  walk d (NBinary _ x y) = go d x >> go d y

  goAnti d (Antiquoted e) = go d e
  goAnti _ _ = Right ()

  goBinding d (NamedVar path e _) = goPath d path >> go d e
  goBinding d (Inherit ms _ _) = maybe (Right ()) (go d) ms

  goPath d path = goPath' d (toList' path)
  goPath' d ks = mapM_ (goKey d) ks
  goKey d (DynamicKey (Antiquoted e)) = go d e
  goKey _ _ = Right ()

  goParams _ (Param _) = Right ()
  goParams d (ParamSet _ _ items) =
    mapM_ (\(_, mDef) -> maybe (Right ()) (go d) mDef) items

  toList' (k :| ks) = k : ks

  constructorTag (NSet _ _) = "attrset"
  constructorTag (NList _) = "list"
  constructorTag (NApp _ _) = "application"
  constructorTag (NLet _ _) = "let"
  constructorTag (NWith _ _) = "with"
  constructorTag (NIf _ _ _) = "if"
  constructorTag (NStr (DoubleQuoted _)) = "string interpolation"
  constructorTag (NStr (Indented _ _)) = "indented-string interpolation"
  constructorTag (NAbs _ _) = "lambda"
  constructorTag (NBinary _ _ _) = "binary operator"
  constructorTag (NUnary _ _) = "unary operator"
  constructorTag (NSelect _ _ _) = "attribute select"
  constructorTag (NHasAttr _ _) = "attribute test"
  constructorTag (NAssert _ _) = "assertion"
  constructorTag (NSynHole _) = "syntax hole"
  constructorTag (NConstant _) = "constant"
  constructorTag (NSym _) = "symbol"
  constructorTag (NLiteralPath _) = "path"
  constructorTag (NEnvPath _) = "env path"

-- ── exception-safe wrappers ───────────────────────────────────────
-- Every parse/IO call goes through one of these. They catch StackOverflow
-- and other async exceptions that try @IOException misses.

-- | catch every exception including StackOverflow; return SafetyError.
safeIO :: IO a -> IO (Either SafetyError a)
safeIO = safeIOWith mempty

-- | like 'safeIO' but prefixes a context string onto any I/O or internal error message.
safeIOWith :: Text -> IO a -> IO (Either SafetyError a)
safeIOWith prefix action = do
  result <- try (action >>= evaluate)
  pure $ either onErr Right result
 where
  onErr (e :: SomeException) = Left (classify prefix e)
  classify pfx e
    | Just Exc.StackOverflow <- fromException e = SafetyStackOverflow
    | Just (ioe :: IOError) <- fromException e
    , isDoesNotExistError ioe =
        SafetyIOError (pfx <> T.pack (show ioe))
    | Just (ioe :: IOError) <- fromException e =
        SafetyIOError (pfx <> T.pack (show ioe))
    | otherwise = SafetyInternalException (pfx <> T.pack (show e))

-- | safely read a UTF-8 file; converts every failure mode to SafetyError.
safeReadFile :: FilePath -> IO (Either SafetyError Text)
safeReadFile path = safeIOWith (T.pack path <> ": ") (TIO.readFile path)

-- | safely parse a Nix text buffer; catches stack overflows from megaparsec.
safeParseNixText :: Text -> IO (Either SafetyError NExprLoc)
safeParseNixText src = do
  r <- safeIO (evaluate (parseNixTextLoc src))
  pure $
    either
      Left
      (either (Left . SafetyParseFailed . T.pack . show) (Right . normalizeStaticKeys))
      r

-- | safely parse a Nix file; catches stack overflows, missing files, parse errors.
safeParseNixFile :: FilePath -> IO (Either SafetyError NExprLoc)
safeParseNixFile path = do
  r <- safeIO (parseNixFileLoc (Path path))
  pure $
    either
      Left
      (either (Left . SafetyParseFailed . T.pack . show) (Right . normalizeStaticKeys))
      r

-- ── combined: parse + depth ──────────────────────────────────────

{- | full safety pipeline: parse, then check depth, then return AST.
Every public entry point should funnel through this (or 'analyzeDepth' if the
AST is already in hand).
-}
safeAnalyze :: NExprLoc -> Either SafetyError NExprLoc
safeAnalyze expr = either (Left . SafetyDepthExceeded) (const (Right expr)) (analyzeDepth expr)
