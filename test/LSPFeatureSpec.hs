{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                       // tests // lsp // features
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He never saw the whole of it, only the traffic: requests arriving,
--    answers dispatched, the board never going dark."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   Pure-compute contract tests for the LSP language features, derived from an
--   ad-hoc audit of the running server. Each feature has a
--   REGRESSION GUARD pinning behaviour that is correct today, plus — where the
--   audit found a gap — a TRIPWIRE encoding the behaviour we WANT. A tripwire
--   inverts its assertion: it is green while the bug lives and flips red the
--   instant the underlying code is fixed, which is the cue to delete the
--   'tripwire' wrapper and keep the bare assertion. Mirrors the long-standing
--   'expectFailure' idiom in "Props.hs".
--
--   The five audited gaps are CLOSED — every former tripwire is a
--   promoted guard: type errors surface as diagnostics at configured
--   severity, `let` files outline, hints survive type errors (partial
--   inference), completion offers locals, and the declaration site
--   navigates.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module LSPFeatureSpec (lspFeatureTests) where

import Data.Either (isLeft)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types (
  CodeAction (..),
  CompletionItem (..),
  Diagnostic (..),
  DiagnosticSeverity (..),
  Position (..),
  Range (..),
  WorkspaceEdit (..),
  filePathToUri,
 )
import Narsil.Core.Config (Config (..), RuleOverride (..), Severity (..), defaultConfig)
import Narsil.Inference.Nix (builtinEnv, inferExprWithEnv)
import Narsil.LSP.Handlers qualified as Handlers
import Narsil.LSP.Handlers.Diagnostics (diagnosticsForExpr)
import Narsil.LSP.Handlers.Features (
  completionsForExpr,
  findRef,
  inlayHintsForExpr,
  violationAction,
 )
import Narsil.LSP.Handlers.Project qualified as Project
import Narsil.LSP.Handlers.Symbols (collectTopBindingSymbols)
import Narsil.Layout.Scope qualified as Scope
import Nix.Expr.Types.Annotated (NExprLoc)
import Nix.Parser (parseNixTextLoc)

-- ── harness ────────────────────────────────────────────────────────

{- | A correct-contract assertion that holds TODAY — a regression guard. Keep it
green: if it ever flips, the feature regressed.
-}
holds :: Bool -> IO Bool
holds = pure

-- | Parse test Nix source; a parse failure is a test bug, so bottom out loudly.
parse :: Text -> NExprLoc
parse src = either (\e -> error ("LSPFeatureSpec parse: " <> show e)) id (parseNixTextLoc src)

-- | A range spanning any reasonable test buffer (inlay hints are range-filtered).
wholeBuffer :: Range
wholeBuffer = Range (Position 0 0) (Position 1000 0)

-- | The label of a completion item (record pattern disambiguates the field).
completionLabel :: CompletionItem -> Text
completionLabel CompletionItem{_label = l} = l

-- ── diagnostics ────────────────────────────────────────────────────

{- | GUARD: a lint violation (`with`) does surface as a diagnostic — the
diagnostics layer works for the rules it covers.
-}
testDiagReportsLint :: IO Bool
testDiagReportsLint =
  holds (not (null (diagnosticsForExpr defaultConfig "<buffer>" (parse "with { a = 1; }; a"))))

{- | GUARD: the inference engine DOES detect `1 + "s"` as a type error (returns
Left). The information exists; the tripwire below is that it never reaches the
editor as a diagnostic.
-}
testEngineDetectsTypeError :: IO Bool
testEngineDetectsTypeError =
  holds (isLeft (inferExprWithEnv builtinEnv (parse "let y = \"s\"; in 1 + y")))

{- | GUARD (promoted tripwire): a type error IS published as a diagnostic —
'diagnosticsForExpr' runs inference at the config's @type-check-failure@
severity, so the engine's verdict reaches the editor as squiggles.
-}
testDiagSurfacesTypeError :: IO Bool
testDiagSurfacesTypeError =
  holds
    (not (null (diagnosticsForExpr defaultConfig "<buffer>" (parse "let y = \"s\"; in 1 + y"))))

{- | GUARD: the editor honors the config's @type-check-failure@ severity —
the `nixpkgs` profile's lax remap publishes a WARNING squiggle, and an
explicit Off publishes nothing. The CLI and the editor share one judgment.
-}
testDiagTypeSeverityRemap :: IO Bool
testDiagTypeSeverityRemap =
  holds (warnsUnderNixpkgs && silentUnderOff)
 where
  broken = parse "let y = \"s\"; in 1 + y"
  typeDiagsOf cfg =
    [ d
    | d <- diagnosticsForExpr cfg "<buffer>" broken
    , Diagnostic{_message = m} <- [d]
    , "type: " `T.isPrefixOf` m
    ]
  sevOf Diagnostic{_severity = sv} = sv
  warnsUnderNixpkgs =
    map sevOf (typeDiagsOf defaultConfig{configProfile = "nixpkgs"})
      == [Just DiagnosticSeverity_Warning]
  silentUnderOff =
    null
      ( typeDiagsOf
          defaultConfig
            { configOverrides = [RuleOverride "type-check-failure" SevOff Nothing]
            }
      )

{- | GUARD: the `off` profile silences the whole editor for a project — its
ignore-the-world glob applies to buffer paths through 'fullLint', matching
the CLI walker exactly.
-}
testOffProfileSilent :: IO Bool
testOffProfileSilent =
  holds
    ( null
        ( Handlers.fullLint
            defaultConfig{configProfile = "off"}
            builtinEnv
            "pkgs/thing/default.nix"
            "with builtins; let y = \"s\"; in 1 + y"
        )
    )

-- ── document symbols ───────────────────────────────────────────────

-- | GUARD: a top-level attrset yields one document symbol per binding.
testSymbolsAttrset :: IO Bool
testSymbolsAttrset =
  holds (length (collectTopBindingSymbols (parse "{ foo = 1; bar = 2; }")) == 2)

{- | GUARD (promoted tripwire): a `let … in` file outlines its let bindings —
the common file shape has a real outline.
-}
testSymbolsLetIn :: IO Bool
testSymbolsLetIn =
  holds (length (collectTopBindingSymbols (parse "let x = 1; y = 2; in x + y")) == 2)

-- ── inlay hints ────────────────────────────────────────────────────

-- | GUARD: a clean file gets a type inlay hint per let binding.
testInlayClean :: IO Bool
testInlayClean =
  holds (not (null (inlayHintsForExpr builtinEnv (parse "let x = 1; y = 2; in x") wholeBuffer)))

{- | GUARD (promoted tripwire): one type error does not erase the hints for
the well-typed bindings around it — partial inference results survive
('inferExprBindingsPartial').
-}
testInlaySurvivesTypeError :: IO Bool
testInlaySurvivesTypeError =
  holds
    ( not
        ( null
            ( inlayHintsForExpr
                builtinEnv
                (parse "let good = 1; bad = 1 + \"s\"; in good")
                wholeBuffer
            )
        )
    )

-- ── completion ─────────────────────────────────────────────────────

letSrc :: Text
letSrc = "let myLocal = 1; in myLocal"

-- | GUARD: completion offers builtins (cursor after "in ", empty prefix).
testCompletionBuiltins :: IO Bool
testCompletionBuiltins =
  holds (any ((== "map") . completionLabel) (completionsForExpr builtinEnv letSrc letExpr 0 21))
 where
  letExpr = parse letSrc

{- | GUARD (promoted tripwire): completion offers in-scope local bindings
alongside builtins.
-}
testCompletionIncludesLocal :: IO Bool
testCompletionIncludesLocal =
  holds (any ((== "myLocal") . completionLabel) (completionsForExpr builtinEnv letSrc letExpr 0 21))
 where
  letExpr = parse letSrc

{- | GUARD: completion is PREFIX-filtered from the buffer text — at
@…in myL|ocal@ the local (and any @myL…@) survive, @map@ does not.
-}
testCompletionPrefixFilters :: IO Bool
testCompletionPrefixFilters =
  holds
    (any ((== "myLocal") . completionLabel) items && not (any ((== "map") . completionLabel) items))
 where
  items = completionsForExpr builtinEnv letSrc (parse letSrc) 0 24

{- | GUARD: completion is POSITION-scoped — an inner lambda's formal is not
offered at a cursor outside the lambda.
-}
testCompletionScopedToCursor :: IO Bool
testCompletionScopedToCursor =
  holds (has "outer" atEnd && not (has "inner" atEnd) && has "inner" inside)
 where
  src = "let outer = (inner: inner) 5; in outer"
  expr = parse src
  has n = any ((== n) . completionLabel)
  atEnd = completionsForExpr builtinEnv src expr 0 37
  inside = completionsForExpr builtinEnv src expr 0 25

{- | GUARD: a non-lisp-case diagnostic carries a COMPLETE rename quickfix —
declaration plus every reference, as a ready-to-apply 'WorkspaceEdit'.
-}
testCodeActionRenameEdit :: IO Bool
testCodeActionRenameEdit =
  holds
    ( case actions of
        [CodeAction{_title = t, _edit = Just WorkspaceEdit{_changes = Just m}}] ->
          t == "Rename to `my-thing`" && any ((>= 2) . length) (Map.elems m)
        _ -> False
    )
 where
  src = "let myThing = 1; in myThing"
  expr = parse src
  sg = Scope.fromNixExpr Nothing expr
  strictCfg = defaultConfig{configProfile = "strict"}
  n13 d = "ALEPH-N013" `T.isInfixOf` (let Diagnostic{_message = m} = d in m)
  diags = filter n13 (diagnosticsForExpr strictCfg "<buffer>" expr)
  actions = concatMap (violationAction (filePathToUri "/b.nix") (Just sg)) diags

-- ── navigation (definition / references) ───────────────────────────

-- | Scope graph + first declaration / first reference of `let x = 1; in x + x`.
navFixture :: (Scope.ScopeGraph, Maybe Scope.Declaration, Maybe Scope.Reference)
navFixture = (sg, listToMaybe decls, listToMaybe refs)
 where
  sg = Scope.fromNixExpr Nothing (parse "let x = 1; in x + x")
  decls = concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg))
  refs = concatMap Scope.scopeReferences (Map.elems (Scope.sgScopes sg))

declPos :: Scope.Declaration -> (Int, Int)
declPos d =
  ( Scope.posLine (Scope.spanStart (Scope.declSpan d))
  , Scope.posCol (Scope.spanStart (Scope.declSpan d))
  )

refPos :: Scope.Reference -> (Int, Int)
refPos r =
  ( Scope.posLine (Scope.spanStart (Scope.refSpan r))
  , Scope.posCol (Scope.spanStart (Scope.refSpan r))
  )

-- | GUARD: the cursor on a USE of `x` resolves to a reference (nav works there).
testFindRefAtUse :: IO Bool
testFindRefAtUse =
  case navFixture of
    (sg, _, Just r) -> holds (isJust (findRef (refPos r) sg))
    _ -> holds False

{- | GUARD: given the declaration, the graph enumerates both references — the
data backing references/rename is correct; only the cursor-on-declaration entry
point is missing (the tripwire below).
-}
testFindReferencesEnumerates :: IO Bool
testFindReferencesEnumerates =
  case navFixture of
    (sg, Just d, _) -> holds (length (Scope.findReferences sg d) == 2)
    _ -> holds False

{- | GUARD (promoted tripwire): the cursor ON a declaration is navigable —
'findRef' resolves the binding site as a self-reference, so references /
rename work from the place users most often invoke them.
-}
testFindRefAtDecl :: IO Bool
testFindRefAtDecl =
  case navFixture of
    (sg, Just d, _) -> holds (isJust (findRef (declPos d) sg))
    _ -> holds False

{- | GUARD: the cross-module scope-graph builder is non-blocking — when no
module graph has been built for the project (cold cache, or a loose file with no
flake root) it answers from the current file ALONE rather than blocking on a
synchronous build or returning an empty graph. So within-file go-to-def works
instantly and needs no project root. (Pins the cold-window de-block of
'buildCrossScopeGraphWith'; the cross-file result arrives on a later request.)
-}
testNavSingleFileFallback :: IO Bool
testNavSingleFileFallback = do
  let uri = filePathToUri "/nonexistent-no-flake-root-xyz/foo.nix"
  sg <- Project.buildCrossScopeGraphWith uri (Just (parse "let x = 1; in x + x"))
  holds (not (null (concatMap Scope.scopeDeclarations (Map.elems (Scope.sgScopes sg)))))

-- ── runner ─────────────────────────────────────────────────────────

-- | All LSP feature contract tests (all guards — the tripwires are promoted).
lspFeatureTests :: [(String, IO Bool)]
lspFeatureTests =
  [ ("lsp_diag_reports_lint", testDiagReportsLint)
  , ("lsp_diag_engine_detects_type_error", testEngineDetectsTypeError)
  , ("lsp_diag_surfaces_type_error", testDiagSurfacesTypeError)
  , ("lsp_diag_type_severity_remap", testDiagTypeSeverityRemap)
  , ("lsp_diag_off_profile_silent", testOffProfileSilent)
  , ("lsp_symbols_attrset", testSymbolsAttrset)
  , ("lsp_symbols_letin", testSymbolsLetIn)
  , ("lsp_inlay_clean", testInlayClean)
  , ("lsp_inlay_survives_type_error", testInlaySurvivesTypeError)
  , ("lsp_completion_builtins", testCompletionBuiltins)
  , ("lsp_completion_includes_local", testCompletionIncludesLocal)
  , ("lsp_completion_prefix_filters", testCompletionPrefixFilters)
  , ("lsp_completion_scoped_to_cursor", testCompletionScopedToCursor)
  , ("lsp_codeaction_rename_edit", testCodeActionRenameEdit)
  , ("lsp_nav_findref_at_use", testFindRefAtUse)
  , ("lsp_nav_findreferences_enumerates", testFindReferencesEnumerates)
  , ("lsp_nav_findref_at_decl", testFindRefAtDecl)
  , ("lsp_nav_single_file_fallback_nonblocking", testNavSingleFileFallback)
  ]
