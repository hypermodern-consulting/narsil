{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                      // straylint
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--     "He'd waited in the booth, watching the door, the gun a dead weight."
--
--                                                                                     — Neuromancer
--

{- | straylint — our own Haskell linter, on GHC's real parser.

We don't believe the existing tooling is good enough for how we write: fourmolu,
hlint, brittany each stumble on the Template Haskell we can't avoid. So this is
the seed of our own analyzer — the nix-compile of Haskell — built on
@ghc-lib-parser@ (which *is* GHC's frontend, and therefore parses TH correctly).

v1 enforces exactly one rule, the house @case@ ban (see doc/HOUSE_STYLE.md): a
@case@ or @\\case@ is demoted in favour of function-clause equations and pattern
guards. The escape hatch is a @CASE-OK@ marker on the offending line, for the
rare local match where locality genuinely beats flatness — counted, visible, few.

  straylint [--strict] FILE...

@--strict@ exits non-zero on any finding (the gate mode); without it, straylint
reports and exits 0. The 'Rule' type is the extension point: more rules land as
more list entries, each a pure function over the parsed module.
-}
module Main (main) where

import Data.Either (lefts, rights)
import Data.Generics (listify)
import Data.List (intercalate, isInfixOf, sortOn)
import Data.Maybe (listToMaybe)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Session (
  DynFlags,
  Language (GHC2021),
  defaultDynFlags,
  languageExtensions,
  xopt_set,
 )
import GHC.Hs (GhcPs, HsModule)
import GHC.Hs.Expr (HsExpr (HsCase, HsLam), HsLamVariant (LamSingle), LHsExpr)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GHC.LanguageExtensions (Extension (..))
import GHC.Parser.Annotation (getLocA)
import GHC.Parser.Lexer (PState, ParseResult (PFailed, POk), getPsErrorMessages)
import GHC.Types.Error (errMsgSpan, getMessages)
import GHC.Types.SrcLoc (
  Located,
  SrcSpan (RealSrcSpan),
  srcSpanStartCol,
  srcSpanStartLine,
  unLoc,
 )
import Language.Haskell.GhclibParserEx.GHC.Parser (parseFile)
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                       // findings
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | One violation: where it is, which rule fired, and what to do about it.
data Finding = Finding
  { findingPath :: !FilePath
  , findingLine :: !Int
  , findingCol :: !Int
  , findingRule :: !String
  , findingMessage :: !String
  }

{- | A lint rule: a name and a pure check over a file's source lines and parsed
module. New rules are new values here — that is the whole extension surface.
-}
data Rule = Rule
  { ruleName :: !String
  , ruleCheck :: FilePath -> [String] -> Located (HsModule GhcPs) -> [Finding]
  }

-- | The rule set. One, today.
rules :: [Rule]
rules = [caseBan]

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                   // the case ban
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Flag every @case@ and @\\case@ / @\\cases@ expression, except those whose
source line carries a @CASE-OK@ marker (the sanctioned, visible escape).
-}
caseBan :: Rule
caseBan = Rule "case-ban" check
 where
  check path sourceLines modul =
    [ Finding
        path
        line
        col
        "case-ban"
        ( caseExprKind expr
            <> " — prefer equations or guards (HOUSE_STYLE.md);"
            <> " mark a justified survivor with CASE-OK"
        )
    | located <- listify isCaseLike modul
    , let expr = unLoc located
    , Just (line, col) <- [spanStart (getLocA located)]
    , not (lineAllows sourceLines line)
    ]

  lineAllows sourceLines line = maybe False (isInfixOf "CASE-OK") (sourceLines `at` (line - 1))

{- | Is this expression a @case@ or a lambda-case (@\\case@ / @\\cases@)? Matches
the annotated 'LHsExpr' the AST actually carries (not a plain 'Located').
-}
isCaseLike :: LHsExpr GhcPs -> Bool
isCaseLike located = exprIsCaseLike (unLoc located)

exprIsCaseLike :: HsExpr GhcPs -> Bool
exprIsCaseLike HsCase{} = True
exprIsCaseLike (HsLam _ variant _) = variant /= LamSingle
exprIsCaseLike _ = False

-- | A human label for the offending construct.
caseExprKind :: HsExpr GhcPs -> String
caseExprKind HsCase{} = "`case`"
caseExprKind HsLam{} = "`\\case`"
caseExprKind _ = "`case`"

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                        // parsing
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

{- | Parse a module with a permissive flag set. We turn on the GHC2021 language
edition (our @default-language@) plus every extension the tree actually uses, so
parsing never fails for want of a flag — straylint only cares about syntax, so
over-enabling is free.
-}
parseModuleText :: FilePath -> String -> Either String (Located (HsModule GhcPs))
parseModuleText path source = result (parseFile path baseDynFlags source)
 where
  result (POk _ modul) = Right modul
  result (PFailed pst) = Left ("parse failed at " <> renderParseErrorLoc pst)

-- | the location of the first parse error in a failed 'PState', as @line:col@.
renderParseErrorLoc :: PState -> String
renderParseErrorLoc pst =
  maybe "unknown location" renderSpan (firstErrorSpan (getPsErrorMessages pst))
 where
  firstErrorSpan msgs = fmap errMsgSpan (listToMaybe (bagToList (getMessages msgs)))
  renderSpan (RealSrcSpan s _) = show (srcSpanStartLine s) <> ":" <> show (srcSpanStartCol s)
  renderSpan _ = "unknown location"

baseDynFlags :: DynFlags
baseDynFlags = foldl' xopt_set (defaultDynFlags fakeSettings) enabledExtensions
 where
  enabledExtensions = languageExtensions (Just GHC2021) <> extraExtensions
  extraExtensions =
    [ BangPatterns
    , DataKinds
    , DeriveAnyClass
    , DerivingStrategies
    , DuplicateRecordFields
    , GADTs
    , LambdaCase
    , MultiWayIf
    , NondecreasingIndentation
    , OverloadedStrings
    , PatternSynonyms
    , RecordWildCards
    , ScopedTypeVariables
    , StrictData
    , TemplateHaskell
    , TypeApplications
    ]

-- | The (line, column) where a span starts, if it is a real location.
spanStart :: SrcSpan -> Maybe (Int, Int)
spanStart (RealSrcSpan s _) = Just (srcSpanStartLine s, srcSpanStartCol s)
spanStart _ = Nothing

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
--                                                                                         // driver
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

main :: IO ()
main = do
  -- our sources carry UTF-8 banners; read them as UTF-8 regardless of locale.
  setLocaleEncoding utf8
  args <- getArgs
  let strict = "--strict" `elem` args
      files = filter (not . isFlag) args
  results <- traverse (lintFile rules) files
  let unparsed = lefts results
      findings = concat (rights results)
  mapM_ (putStrLn . renderFinding) (sortOn findingKey findings)
  reportSummary (length files) unparsed findings
  finish strict unparsed findings
 where
  isFlag argument = take 2 argument == "--"

{- | Lint one file: parse it, run every rule, collect findings, or report it as
unparsed. A parse failure is NOT a silent skip: under @--strict@ it fails the
gate ('finish'), because a file straylint cannot read is a file the case-ban is
not enforced on. The source is splice-neutralised first ('neutralizeSplices') so
TH does not trip the parser.
-}
lintFile :: [Rule] -> FilePath -> IO (Either FilePath [Finding])
lintFile activeRules path = do
  source <- readFile path
  either (onUnparsed source) (onParsed source) (parseModuleText path (neutralizeSplices source))
 where
  onUnparsed _ err = do
    hPutStrLn stderr ("straylint: " <> path <> ": " <> err)
    pure (Left path)
  onParsed source modul =
    pure (Right (concatMap (\rule -> ruleCheck rule path (lines source) modul) activeRules))

{- | Neutralise Template Haskell expression splices so the parser never trips on
them. straylint is a purely syntactic case/lambda check; it neither runs nor
typechecks splices, so rewriting @$(e)@ to @ (e)@ (dollar → space) is
information-preserving for our rules. We rewrite @$(@ to @( @ — open-paren in the
@$@'s column, space in the @(@'s — which is both COLUMN-preserving (one char for
one, so a reported line:col still indexes the original source) and, crucially,
LAYOUT-preserving: a splice that begins a do-statement keeps a non-space token in
its original column, so it is not re-read as a continuation of the previous line.
@$(e)@ becomes @( e)@, an ordinary parenthesised expression. This sidesteps a
ghc-lib-parser 9.12 lexer gap on @$ $(…)@ that the real compiler (GHC 9.10)
accepts. Safe because the tree has no top-level declaration splices or
quasi-quotes; were that to change this would need revisiting.
-}
neutralizeSplices :: String -> String
neutralizeSplices [] = []
neutralizeSplices ('$' : '(' : rest) = '(' : ' ' : neutralizeSplices rest
neutralizeSplices (c : rest) = c : neutralizeSplices rest

renderFinding :: Finding -> String
renderFinding finding =
  findingPath finding
    <> ":"
    <> show (findingLine finding)
    <> ":"
    <> show (findingCol finding)
    <> ": ["
    <> findingRule finding
    <> "] "
    <> findingMessage finding

findingKey :: Finding -> (FilePath, Int, Int)
findingKey finding = (findingPath finding, findingLine finding, findingCol finding)

reportSummary :: Int -> [FilePath] -> [Finding] -> IO ()
reportSummary fileCount unparsed findings =
  hPutStrLn
    stderr
    ( "straylint: "
        <> show (length findings)
        <> " finding(s), "
        <> show (length unparsed)
        <> " unparsed, across "
        <> show fileCount
        <> " file(s)  [rules: "
        <> intercalate ", " (map ruleName rules)
        <> "]"
    )

{- | Exit. Without @--strict@ straylint is informational and always exits 0. With
@--strict@ (the gate) it fails on any finding OR any unparsed file — an unparsed
file is a hole in the case-ban, not a free pass, so it must turn the gate red.
-}
finish :: Bool -> [FilePath] -> [Finding] -> IO ()
finish False _ _ = exitSuccess
finish True [] [] = exitSuccess
finish True _ _ = exitFailure

-- | Safe list indexing (no partial @!!@).
at :: [a] -> Int -> Maybe a
at xs index
  | index < 0 = Nothing
  | otherwise = atGo xs index
 where
  atGo [] _ = Nothing
  atGo (y : _) 0 = Just y
  atGo (_ : ys) n = atGo ys (n - 1)
