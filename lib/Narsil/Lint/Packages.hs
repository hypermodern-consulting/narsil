-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                              // Narsil.Lint.Packages // check
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "The black Honda hovered twenty meters above the octagonal deck of the
--    derelict oil rig."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                            // Nix // package directory lint rules
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE OverloadedStrings #-}

module Narsil.Lint.Packages (
  PackageViolationCode (..),
  PackageViolation (..),
  checkPackageDirs,
)
where

import Control.Exception (IOException, try)
import Control.Monad (filterM)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import System.Directory (doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

import Narsil.Layout.ModuleKind (detectKindFromFile, isPackage)

-- | package-directory lint codes; @P001@ is "package dir missing default.nix".
data PackageViolationCode
  = P001
  deriving (Show, Eq)

-- | one package-directory violation: its code, the offending directory, and a message.
data PackageViolation = PackageViolation
  { pvCode :: !PackageViolationCode
  , pvPath :: !FilePath
  , pvMessage :: !Text
  }
  deriving (Show, Eq)

-- | given a set of .nix files, flag every package directory that lacks a @default.nix@.
checkPackageDirs :: [FilePath] -> IO [PackageViolation]
checkPackageDirs nixFiles = do
  let dirs = nub (map takeDirectory nixFiles)
  packageDirs <- filterM isPackageDir dirs
  catMaybes <$> mapM checkDefaultNix packageDirs

isPackageDir :: FilePath -> IO Bool
isPackageDir directory = do
  listResult <- try (listDirectory directory)
  either onError onListing listResult
 where
  onError (_ :: IOException) = pure False
  onListing entries =
    anyM
      (\file -> isPackageModule (directory </> file))
      (filter ((== ".nix") . takeExtension) entries)

isPackageModule :: FilePath -> IO Bool
isPackageModule path = do
  moduleKind <- detectKindFromFile path
  pure (isPackage moduleKind)

checkDefaultNix :: FilePath -> IO (Maybe PackageViolation)
checkDefaultNix directory = do
  fileExists <- doesFileExist (directory </> "default.nix")
  pure $ missingDefaultViolation directory fileExists
 where
  missingDefaultViolation _ True = Nothing
  missingDefaultViolation directoryPath False =
    Just
      PackageViolation
        { pvCode = P001
        , pvPath = directoryPath
        , pvMessage = "Package directory missing default.nix"
        }

anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM predicate (element : rest) = do
  result <- predicate element
  if result then pure True else anyM predicate rest
