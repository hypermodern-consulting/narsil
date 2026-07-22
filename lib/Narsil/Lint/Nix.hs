-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                    // Narsil.Lint.Nix // lint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "It was such an easy thing, death. He saw that now: It just happened."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                              // Nix // banned construct detection
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Narsil.Lint.Nix (
  NixViolation (..),
  ViolationType (..),
  findNixViolations,
  checkLetBindingName,
  suggestLispCase,
  formatNixViolations,
  nixViolationDiagnostic,
)
where

import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Katip (Severity (ErrorS))
import Narsil.Core.Diagnostic (Diagnostic (..))
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Syntax.Annotation (srcSpanToSpan, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import Nix.Utils qualified as Nix

{- | A lint violation as a unified 'Diagnostic': the rule code, a one-line
summary, the span, and the suggestion line as @= help:@. The verbose
explanation from 'formatNixNote' is condensed to its final (suggestion) line.
-}
nixViolationDiagnostic :: NixViolation -> Diagnostic
nixViolationDiagnostic v =
  Diagnostic
    { diagSeverity = ErrorS
    , diagCode = if T.null code then Nothing else Just code
    , diagSpan = Just (nvSpan v)
    , diagSummary = desc
    , diagHelp = lastLine (formatNixNote (nvType v))
    , diagSnippet = Nothing
    }
 where
  full = formatNixErrorCode (nvType v)
  (codePart, rest) = T.breakOn ": " full
  (code, desc) = if T.null rest then ("", full) else (codePart, T.drop 2 rest)
  -- the last non-blank line of the note (the suggestion), or none
  lastLine note = take 1 (reverse (filter (not . T.null) (map T.strip (T.lines note))))

{- | a banned Nix construct: @with@, @rec@, @substituteAll@, the raw derivation
builders, @writeShellScript@, or an over-long inline string (with its length).
-}
data ViolationType
  = VWith
  | VRec
  | VSubstituteAll
  | VRawMkDerivation
  | VRawRunCommand
  | VRawWriteShellApplication
  | VWriteShellScript
  | VLongInlineString !Int
  | -- | a non-lisp-case name in an AUTHOR-OWNED binding position (carries the name)
    VNonLispCase !Text
  deriving (Eq, Show)

-- | one Nix-idiom violation: its kind, source span, and a short context label.
data NixViolation = NixViolation
  { nvType :: !ViolationType
  , nvSpan :: !Span
  , nvContext :: !Text
  }
  deriving (Eq, Show)

maxInlineStringLength :: Int
maxInlineStringLength = 120

-- ── entry point ────────────────────────────────────────────────────

-- | walk an expression and collect every banned-construct violation.
findNixViolations :: NExprLoc -> [NixViolation]
findNixViolations = traverseNixExpr

-- ── tree walk ──────────────────────────────────────────────────────
-- The recursive descent spine. Each node emits its local violations,
-- then recurses into all sub-expressions (and bindings where present).
-- n.b. the : vs ++ ordering matters for diagnostic stability — local
-- violations appear first so the user sees the direct problem before
-- any cascading sub-expression issues.

traverseNixExpr :: NExprLoc -> [NixViolation]
-- the two banned binders carry a local violation, then recurse
traverseNixExpr (LayerAnn srcSpan (NWith scope body)) =
  nixViolation VWith srcSpan ("with " <> prettyExpr scope <> ";")
    : traverseNixExpr scope
    ++ traverseNixExpr body
traverseNixExpr (LayerAnn srcSpan (NSet Recursive bindings)) =
  nixViolation VRec srcSpan "rec { ... }"
    : concatMap traverseNixBinding bindings
-- application: check for a banned call at the head, then recurse both sides
traverseNixExpr (LayerAnn srcSpan (NApp function argument)) =
  checkBannedAppCall srcSpan function ++ traverseNixExpr function ++ traverseNixExpr argument
-- a double-quoted string may also be too long; indented strings only recurse
traverseNixExpr (LayerAnn srcSpan (NStr (DoubleQuoted parts))) =
  checkInlineStringLength srcSpan parts ++ concatMap nixPartExprs parts
traverseNixExpr (Layer (NStr (Indented _ parts))) = concatMap nixPartExprs parts
-- everything else: no local violation, just recurse into sub-expressions
traverseNixExpr (Layer (NSet NonRecursive bindings)) = concatMap traverseNixBinding bindings
traverseNixExpr (Layer (NList xs)) = concatMap traverseNixExpr xs
-- let bindings are the AUTHOR-OWNED naming position: attr keys mirror
-- external schemas (buildInputs, perSystem) and lambda formals are
-- caller-dictated, but a `let` name is chosen by the file's author — the
-- one place lisp-case is enforceable without flooding on interop
traverseNixExpr (Layer (NLet bindings body)) =
  concatMap checkLetBindingName bindings
    ++ concatMap traverseNixBinding bindings
    ++ traverseNixExpr body
traverseNixExpr (Layer (NIf c t f)) = traverseNixExpr c ++ traverseNixExpr t ++ traverseNixExpr f
traverseNixExpr (Layer (NAssert c b)) = traverseNixExpr c ++ traverseNixExpr b
traverseNixExpr (Layer (NAbs _ b)) = traverseNixExpr b
traverseNixExpr (Layer (NSelect alt b _)) = traverseNixExpr b ++ maybe [] traverseNixExpr alt
traverseNixExpr (Layer (NHasAttr b _)) = traverseNixExpr b
traverseNixExpr (Layer (NUnary _ x)) = traverseNixExpr x
traverseNixExpr (Layer (NBinary _ x y)) = traverseNixExpr x ++ traverseNixExpr y
traverseNixExpr _ = []

-- ── binding traversal ──────────────────────────────────────────────
-- Extract sub-expressions from both named var bindings and inherit
-- clauses. Inherit without a scope is a no-op (just pulls from scope).

traverseNixBinding :: Binding NExprLoc -> [NixViolation]
traverseNixBinding (NamedVar _ expr _) = traverseNixExpr expr
traverseNixBinding (Inherit (Just scope) _ _) = traverseNixExpr scope
traverseNixBinding (Inherit Nothing _ _) = []

-- ── naming (author-owned positions only) ───────────────────────────

{- | Flag a non-lisp-case name in a @let@ binding (single static key only;
inherit names come from elsewhere by definition, dotted paths are attr
structure). Lisp-case: a lowercase letter, then lowercase\/digits\/dashes,
with idiomatic trailing primes (@x'@) allowed.
-}
checkLetBindingName :: Binding NExprLoc -> [NixViolation]
checkLetBindingName (NamedVar (StaticKey key :| []) _ pos)
  | not (isLispCase name) =
      [ NixViolation
          { nvType = VNonLispCase name
          , nvSpan = posToSpan pos
          , nvContext = "let " <> name <> " = …;"
          }
      ]
 where
  name = coerce key
checkLetBindingName _ = []

isLispCase :: Text -> Bool
isLispCase name = ok (T.unpack (T.dropWhileEnd (== '\'') name))
 where
  ok [] = False
  ok (c : rest) = lower c && all body rest
  lower c = c >= 'a' && c <= 'z'
  body c = lower c || (c >= '0' && c <= '9') || c == '-'

posToSpan :: NSourcePos -> Span
posToSpan (NSourcePos path l c) =
  Span
    (Loc (unPos (coerce l)) (unPos (coerce c)))
    (Loc (unPos (coerce l)) (unPos (coerce c)))
    (Just (coerce path))

-- ── banned function calls ──────────────────────────────────────────
-- Detect calls to functions that are banned at the project level.
-- These each have a distinct ViolationType so the formatter can emit
-- specific remediation guidance.

checkBannedAppCall :: SrcSpan -> NExprLoc -> [NixViolation]
checkBannedAppCall srcSpan f = maybe [] banned (leafSym f)
 where
  banned name
    | name == "substituteAll" = [nixViolation VSubstituteAll srcSpan "substituteAll ..."]
    | name == "mkDerivation" = [nixViolation VRawMkDerivation srcSpan "mkDerivation { ... }"]
    | name == "runCommand" = [nixViolation VRawRunCommand srcSpan "runCommand ..."]
    | name == "writeShellApplication" =
        [nixViolation VRawWriteShellApplication srcSpan "writeShellApplication { ... }"]
    | name == "writeShellScript" || name == "writeShellScriptBin" =
        [nixViolation VWriteShellScript srcSpan (name <> " ...")]
    | otherwise = []

-- ── inline string length ───────────────────────────────────────────
-- Long inline strings clutter source files and should be extracted to
-- separate files. The threshold is defined by `maxInlineStringLength`.

checkInlineStringLength :: SrcSpan -> [Antiquoted Text NExprLoc] -> [NixViolation]
checkInlineStringLength srcSpan parts
  | stringLength > maxInlineStringLength =
      [ nixViolation
          (VLongInlineString stringLength)
          srcSpan
          ("inline string of length " <> T.pack (show stringLength))
      ]
  | otherwise = []
 where
  stringLength = sum (map nixPartLength parts)

-- ── leaf-symbol extraction ─────────────────────────────────────────
-- Resolve an expression to its "leaf name" — either a bare symbol or
-- the final segment of a select chain (e.g., `lib.mkDerivation` -> mkDerivation).
-- !? this doesn't handle `with`-imported names or recursive attr lookups

leafSym :: NExprLoc -> Maybe Text
leafSym (Layer (NSym name)) = Just (coerce name)
leafSym (Layer (NSelect _ _base (StaticKey key :| _))) = Just (coerce key)
leafSym _ = Nothing

-- ── string part helpers ────────────────────────────────────────────

nixPartLength :: Antiquoted Text NExprLoc -> Int
nixPartLength (Plain text) = T.length text
nixPartLength _ = 0

nixPartExprs :: Antiquoted Text NExprLoc -> [NixViolation]
nixPartExprs (Antiquoted expr) = traverseNixExpr expr
nixPartExprs _ = []

-- ── pretty-printing for context ────────────────────────────────────
-- Produces a short, human-readable summary of a sub-expression for
-- embedding in violation context messages. Not intended to be valid
-- Nix — just enough for a developer to locate the problem.

prettyExpr :: NExprLoc -> Text
prettyExpr (Layer (NSym name)) = coerce name
prettyExpr (Layer (NSelect _ base _)) = prettyExpr base <> ".‥"
prettyExpr _ = "‥"

-- | a lisp-case rendering of a camelCase\/snake_case name, for the help line.
suggestLispCase :: Text -> Text
suggestLispCase = T.replace "_" "-" . T.concatMap dashLower
 where
  dashLower c
    | c >= 'A' && c <= 'Z' = T.pack ['-', toEnum (fromEnum c + 32)]
    | otherwise = T.singleton c

-- ── violation construction ─────────────────────────────────────────

nixViolation :: ViolationType -> SrcSpan -> Text -> NixViolation
nixViolation typ srcSpan ctx =
  NixViolation
    { nvType = typ
    , nvSpan = srcSpanToSpan srcSpan
    , nvContext = ctx
    }

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                              // output formatting
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

-- | render a list of Nix violations as human-readable text blocks.
formatNixViolations :: [NixViolation] -> Text
formatNixViolations = T.unlines . map formatOneNixViolation

formatOneNixViolation :: NixViolation -> Text
formatOneNixViolation v =
  T.unlines
    [ formatNixLoc (nvSpan v) <> ": " <> formatNixErrorCode (nvType v)
    , "  " <> nvContext v
    , ""
    , formatNixNote (nvType v)
    ]

formatNixLoc :: Span -> Text
formatNixLoc span' = maybe loc (\f -> T.pack f <> ":" <> loc) (spanFile span')
 where
  loc = T.pack (show (locLine (spanStart span'))) <> ":" <> T.pack (show (locCol (spanStart span')))

-- ── error codes ────────────────────────────────────────────────────
-- Each violation type maps to an ALEPH-Nxxx code that is stable across
-- releases. Used by CI to suppress known issues and by editors to
-- provide quickfix links.

formatNixErrorCode :: ViolationType -> Text
formatNixErrorCode VWith = "ALEPH-N001: `with` expression"
formatNixErrorCode VRec = "ALEPH-N002: `rec` attrset"
formatNixErrorCode VSubstituteAll = "ALEPH-N005: `substituteAll`"
formatNixErrorCode VRawMkDerivation = "ALEPH-N006: raw `mkDerivation`"
formatNixErrorCode VRawRunCommand = "ALEPH-N007: raw `runCommand`"
formatNixErrorCode VRawWriteShellApplication = "ALEPH-N008: raw `writeShellApplication`"
formatNixErrorCode VWriteShellScript = "ALEPH-N011: `writeShellScript`"
formatNixErrorCode (VLongInlineString n) =
  "ALEPH-N012: long inline string (" <> T.pack (show n) <> " chars)"
formatNixErrorCode (VNonLispCase name) = "ALEPH-N013: non-lisp-case binding `" <> name <> "`"

-- ── remediation notes ──────────────────────────────────────────────
-- These are the full-text explanations shown to the user after the
-- one-line error header. Each note explains why the construct is
-- banned and what to use instead.

formatNixNote :: ViolationType -> Text
formatNixNote VWith =
  T.unlines
    [ "  `with` is banned because it:"
    , "    - Obscures where names come from"
    , "    - Breaks tooling (go-to-definition, autocomplete)"
    , "    - Creates shadowing hazards"
    , "    - Makes type inference unsound"
    , ""
    , "  Use `inherit (expr) name1 name2;` instead."
    ]
formatNixNote VRec =
  T.unlines
    [ "  `rec` is banned because it:"
    , "    - Enables infinite loops (non-termination)"
    , "    - Complicates static analysis"
    , "    - Makes evaluation order-dependent"
    , "    - Breaks referential transparency"
    , ""
    , "  Use `let` bindings or explicit function arguments instead."
    ]
formatNixNote VSubstituteAll =
  T.unlines
    [ "  `substituteAll` is banned because it:"
    , "    - Copies all derivation dependencies into the store"
    , "    - Is needlessly expensive for single-variable substitution"
    , "    - Should be replaced with the simpler `substitute` approach"
    , ""
    , "  Use `substituteInPlace` or `substitute` with explicit values instead."
    ]
formatNixNote VRawMkDerivation =
  T.unlines
    [ "  Raw `mkDerivation` is banned because it:"
    , "    - Bypasses language-specific wrappers"
    , "    - Misses important build phases and hooks"
    , ""
    , "  Use a language-specific wrapper (stdenv.mkDerivation, buildPythonPackage, etc.)."
    ]
formatNixNote VRawRunCommand =
  T.unlines
    [ "  Raw `runCommand` is banned because it:"
    , "    - Creates derivations without proper package metadata"
    , "    - Bypasses build system conventions"
    , ""
    , "  Use `runCommandWith` or a proper derivation wrapper instead."
    ]
formatNixNote VRawWriteShellApplication =
  T.unlines
    [ "  Raw `writeShellApplication` is banned because it:"
    , "    - Should be declared via the module system"
    , "    - Bypasses shell script linting and type checking"
    , ""
    , "  Use `aleph.shell.writeShellApplication` or the nix-compile wrapper instead."
    ]
formatNixNote VWriteShellScript =
  T.unlines
    [ "  `writeShellScript` is banned because it:"
    , "    - Lacks runtime metadata (name, runtime inputs, description)"
    , "    - Bypasses the module system for shell applications"
    , ""
    , "  Use `writeShellApplication` which requires explicit metadata."
    ]
formatNixNote (VNonLispCase name) =
  T.unlines
    [ "  `" <> name <> "` is not lisp-case. Author-chosen names (let bindings)"
    , "    use lowercase-with-dashes in straylight code; attribute keys and"
    , "    lambda formals mirror their callers and are not checked."
    , ""
    , "  Rename to something like `" <> suggestLispCase name <> "`."
    ]
formatNixNote (VLongInlineString n) =
  T.unlines
    [ "  Inline strings longer than "
        <> T.pack (show maxInlineStringLength)
        <> " characters are banned"
    , "    because they:"
    , "    - Clutter source files"
    , "    - Are hard to review and maintain"
    , "    - Should be extracted to separate files"
    , ""
    , "  Current string length: " <> T.pack (show n) <> " characters."
    , ""
    , "  Use a file reference (e.g., `builtins.readFile ./data.txt`) instead."
    ]
