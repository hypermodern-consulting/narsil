{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                    // nixpkgs // options // index
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The eye at the top of every tower."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The NixOS OPTIONS universe, indexed SYNTACTICALLY: every `mkOption`
--   declaration under `nixos/modules/**`, extracted by the same reified-type
--   walker the module ontology uses ('Module.declaredOptionsWithSpans') — no
--   evaluation, exact source spans, offline. Powers `config.…` completion,
--   and go-to-declaration into nixpkgs itself.
--
--   Deliberately-missed residue: options GENERATED at eval time (renames,
--   function-built submodules). An eval-backed layer can top this up later;
--   the syntactic tier is the always-available floor, in the same doctrine
--   as the package index.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.OptionsIndex (
  OptionsIndex (..),
  OptionEntry (..),
  buildOptionsIndex,
  childrenAt,
  lookupExact,
)
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.QSemN (newQSemN, signalQSemN, waitQSemN)
import Control.Exception (bracket_)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span (Span)
import Narsil.Inference.Nix.Module qualified as Module
import Narsil.Inference.Nix.Type qualified as NT

-- | one declared option: its dotted path, pretty type, declaring file + span.
data OptionEntry = OptionEntry
  { oePath :: ![Text]
  , oeType :: !Text
  , oeFile :: !FilePath
  , oeSpan :: !Span
  }
  deriving (Eq, Show)

-- | the indexed options universe for one nixpkgs root.
data OptionsIndex = OptionsIndex
  { oiRoot :: !FilePath
  , oiEntries :: ![OptionEntry]
  }
  deriving (Eq, Show)

{- | Walk @<root>/nixos/modules@ recursively, parse every @.nix@ file, and
extract its option declarations. Parse failures and non-modules contribute
nothing; the walk is capability-bounded.
-}
buildOptionsIndex :: FilePath -> IO OptionsIndex
buildOptionsIndex root = do
  let modulesDir = root </> "nixos" </> "modules"
  present <- doesDirectoryExist modulesDir
  files <- if present then nixFilesUnder modulesDir else pure []
  sem <- newQSemN =<< getNumCapabilities
  entries <-
    forConcurrently files $ \f ->
      bracket_ (waitQSemN sem 1) (signalQSemN sem 1) (fileEntries f)
  pure (OptionsIndex root (concat entries))

fileEntries :: FilePath -> IO [OptionEntry]
fileEntries f = do
  parsed <- Safety.safeParseNixFile f
  pure (either (const []) fromExpr parsed)
 where
  fromExpr expr =
    [ OptionEntry path (NT.prettyType t) f sp
    | (path, t, sp) <- Module.declaredOptionsWithSpans expr
    ]

nixFilesUnder :: FilePath -> IO [FilePath]
nixFilesUnder dir = do
  names <- listDirectory dir
  concat <$> mapM entry names
 where
  entry name = do
    let p = dir </> name
    isDir <- doesDirectoryExist p
    if isDir
      then nixFilesUnder p
      else pure [p | takeExtension p == ".nix"]

{- | The NEXT path segments available under @prefix@ whose name starts with
@partial@ — completion's question. A leaf child carries its type; an interior
namespace carries 'Nothing'.
-}
childrenAt :: OptionsIndex -> [Text] -> Text -> [(Text, Maybe Text)]
childrenAt idx prefix partial =
  nub
    (mapMaybe child (oiEntries idx))
 where
  child e = case stripPfx prefix (oePath e) of -- CASE-OK: shape dispatch
    Just (next : rest)
      | partial `T.isPrefixOf` next ->
          Just (next, if null rest then Just (oeType e) else Nothing)
    _ -> Nothing

-- | the entry declared at EXACTLY this path, if any.
lookupExact :: OptionsIndex -> [Text] -> Maybe OptionEntry
lookupExact idx path =
  listToMaybe [e | e <- oiEntries idx, oePath e == path]

stripPfx :: [Text] -> [Text] -> Maybe [Text]
stripPfx [] xs = Just xs
stripPfx (p : ps) (x : xs) | p == x = stripPfx ps xs
stripPfx _ _ = Nothing
