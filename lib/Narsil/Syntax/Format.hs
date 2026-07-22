{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // syntax // format
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A year here and he still dreamed of cyberspace, hope fading nightly."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   The reformatter is backed by DEEP-VENDORED nixfmt (RFC 166) under
--   @vendor/nixfmt@ — first-class code in our tree, building under our flags
--   (MPL-2.0; see vendor/nixfmt/LICENSE). This gives byte-for-byte parity with
--   the upstream `nixfmt` binary; we own the vendored source and diverge by
--   editing @vendor/nixfmt/Nixfmt/Pretty.hs@ etc.
--
--   nixfmt re-parses the source with its own parser (its layout depends on
--   comment/trivia attached to tokens, which hnix discards), so the already-parsed
--   'NExprLoc' is unused here — the caller still parses with hnix first for the
--   safety/depth gate. Defaults match `nixfmt -`: width 100, indent 2, non-strict.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Syntax.Format (
  formatNix,
  formatNixFile,
)
where

import Data.Text (Text)
import Nix.Expr.Types.Annotated (NExprLoc)
import Nixfmt qualified
import Nixfmt.Predoc (layout)

{- | Format source via vendored nixfmt with the RFC-166 CLI defaults (100-col
width, 2-space indent, non-strict). On the (unreachable) event that nixfmt's
parser rejects source the caller already parsed with hnix, fall back to the
input verbatim.
-}
nixfmtFormat :: FilePath -> Text -> Text
nixfmtFormat path srcTxt =
  either (const srcTxt) id (Nixfmt.format (layout 100 2 False) path srcTxt)

-- | reformat in-memory Nix source via vendored nixfmt; the parsed AST is ignored.
formatNix :: Text -> NExprLoc -> Text
formatNix srcTxt _expr = nixfmtFormat "<nix>" srcTxt

-- | reformat Nix source via vendored nixfmt, using @path@ for error context only.
formatNixFile :: Text -> FilePath -> NExprLoc -> Text
formatNixFile srcTxt path _expr = nixfmtFormat path srcTxt
