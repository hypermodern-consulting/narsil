{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                   // nix // infer
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Machine dreams hold a special vertigo."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The @infer@ command: run the inference engine ('Narsil.Inference.Nix')
--   over a source file and render it with inline @# :: <type>@ annotations.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Inference.Nix.Annotate (
  -- * Type-annotation injection (the @infer@ command)
  annotateFile,
  annotateFileWithEnv,
  annotateExpr,

  -- * Low-level
  annotateSource,
  annotateText,
  stripAnnotations,
)
where

import Data.List (sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix (
  Binding (..),
  InferResult (..),
  TypeEnv,
  builtinEnv,
  inferExprWithEnv,
 )
import Narsil.Inference.Nix.Type (prettyType)
import Narsil.Syntax.Parse (parseNix)
import Nix.Expr.Types.Annotated (NExprLoc)

-- | Annotate a file with inferred types using the default (no-import) env.
annotateFile :: FilePath -> IO (Either Text Text)
annotateFile = annotateFileWithEnv builtinEnv

{- | Annotate a file using a pre-built TypeEnv (e.g. from cross-module inference).
n.b. D2 from review-2: the @infer@ command must not throw away cross-module
knowledge by inferring with the empty env.
-}
annotateFileWithEnv :: TypeEnv -> FilePath -> IO (Either Text Text)
annotateFileWithEnv env path = do
  readResult <- Safety.safeReadFile path
  pure (either (Left . Safety.renderSafetyError) (annotateText env path) readResult)

-- | parse and annotate an in-memory expression source string with the default env.
annotateExpr :: Text -> Either Text Text
annotateExpr = annotateText builtinEnv "<input>"

{- | Strip any prior @# :: …@ annotations, parse the cleaned source, and re-render
it with fresh annotations. Stripping first is what makes @infer@ IDEMPOTENT: a
second run replaces the comments rather than stacking a new layer on the old.
-}
annotateText :: TypeEnv -> FilePath -> Text -> Either Text Text
annotateText env path src =
  let clean = stripAnnotations src
   in either Left (annotateExprWithEnv env clean) (parseNix path clean)

{- | Remove previously-injected @# :: …@ annotation lines (a line whose first
non-space content is the @# ::@ marker). Ordinary comments are untouched.
-}
stripAnnotations :: Text -> Text
stripAnnotations = T.unlines . filter (not . isAnnotation) . T.lines
 where
  isAnnotation line = "# ::" `T.isPrefixOf` T.stripStart line

annotateExprWithEnv :: TypeEnv -> Text -> NExprLoc -> Either Text Text
annotateExprWithEnv env src expr =
  either onDepth (const inferred) (Safety.analyzeDepth expr)
 where
  onDepth de = Left (Safety.renderSafetyError (Safety.SafetyDepthExceeded de))
  inferred = either Left fromBindings (inferExprWithEnv env expr)
  fromBindings (_, bindings) = Right $ annotateSource src (InferResult bindings [])

{- | inject @# :: \<type\>@ comment lines into source text from an inference result,
applying annotations bottom-up so earlier line offsets stay valid.
-}
annotateSource :: Text -> InferResult -> Text
annotateSource src InferResult{..} =
  let
    bindingAnns = map mkBindingAnn irBindings
    -- one annotation per source line: keep the leftmost (outermost) binding, so
    -- inline / nested bindings that share a line don't stack above it.
    perLine = Map.elems (Map.fromListWith leftmost [(locLine (annLoc a), a) | a <- bindingAnns])
    anns = sortBy (flip (comparing annLoc)) perLine
   in
    foldl' (flip applyAnn) src anns
 where
  leftmost a b = if locCol (annLoc a) <= locCol (annLoc b) then a else b

data Ann = Ann
  { annLoc :: !Loc
  , annText :: !Text
  }
  deriving (Eq, Show)

mkBindingAnn :: Binding -> Ann
mkBindingAnn Binding{..} =
  Ann
    { annLoc = spanStart bindSpan
    , annText = "# :: " <> prettyType bindType
    }

applyAnn :: Ann -> Text -> Text
applyAnn Ann{..} src =
  let lines_ = T.lines src
      (before, after) = splitAt (locLine annLoc - 1) lines_
      indent = getIndent (headSafe after)
   in T.unlines $ before ++ [indent <> annText] ++ after

getIndent :: Maybe Text -> Text
getIndent Nothing = ""
getIndent (Just t) = T.takeWhile (== ' ') t

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x : _) = Just x
