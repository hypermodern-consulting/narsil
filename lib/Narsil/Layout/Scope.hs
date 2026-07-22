-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // nix // compile // scope
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Machine dreams hold a special vertigo. Turner lay down on a
--    virgin slab of green temperfoam in the makeshift dorm and
--    jacked Mitchell's dossier."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The scope-graph façade. The work lives in four leaves: the type vocabulary
--   ('Narsil.Layout.Scope.Types', also the JSON projection), construction
--   from a Nix AST ('…Scope.Build'), name resolution + queries
--   ('…Scope.Resolve'), and the Dhall projection ('…Scope.Dhall'). This module
--   re-exports their public surface unchanged, so callers import one name.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Layout.Scope (
  module Narsil.Layout.Scope.Types,
  module Narsil.Layout.Scope.Build,
  module Narsil.Layout.Scope.Resolve,
  toJSON,
  toDhall,
)
where

import Data.Aeson (toJSON)
import Narsil.Layout.Scope.Build
import Narsil.Layout.Scope.Dhall (toDhall)
import Narsil.Layout.Scope.Resolve
import Narsil.Layout.Scope.Types
