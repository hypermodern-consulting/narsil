-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                              // app // Narsil // CLI // Types
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE OverloadedStrings #-}

module Narsil.CLI.Types (
  TCResult (..),
  CICounts (..),
  emptyCICounts,
  okMarker,
  crossMarker,
  unsupMarker,
)
where

import Data.Text (Text)

-- ── check result ────────────────────────────────────────────────────

-- | Outcome of checking one file: passed, failed, or skipped (e.g. ignored).
data TCResult = TCOk | TCFail | TCSkip
  deriving (Eq, Show)

-- ── CI aggregate counts ─────────────────────────────────────────────

{- | Aggregate tallies across all CI phases — files scanned plus per-category
pass/fail/skip and violation counts — summed into the final summary.
-}
data CICounts = CICounts
  { ciFilesScanned :: !Int
  , ciTypePass :: !Int
  , ciTypeFail :: !Int
  , ciTypeSkip :: !Int
  , ciLintViolations :: !Int
  , ciPackageViolations :: !Int
  , ciBashViolations :: !Int
  , ciGraphFailures :: !Int
  , ciLayoutViolations :: !Int
  }

-- | A 'CICounts' with every field zeroed — the starting accumulator.
emptyCICounts :: CICounts
emptyCICounts = CICounts 0 0 0 0 0 0 0 0 0

-- ── status markers ──────────────────────────────────────────────────

-- | Status marker for a passing file: @[OK]@.
okMarker :: Text
okMarker = "[OK]"

-- | Status marker for a failing file: @[XX]@.
crossMarker :: Text
crossMarker = "[XX]"

-- | Status marker for a file with an unsupported construct: @[UNC]@.
unsupMarker :: Text
unsupMarker = "[UNC]"
