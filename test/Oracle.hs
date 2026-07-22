{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                   // tests // differential oracle
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
-- Ground-truth check for the Nix type checker: for each (closed) expression,
-- compare the inferred type against what `nix-instantiate --eval` actually
-- produces via `builtins.typeOf`. This is the soundness oracle the review
-- (REVIEW-3 #9) said was missing — the one property a type checker most needs:
-- "accept ⟹ the runtime type matches what we claimed".
--
-- Verdicts:
--   MISMATCH    checker claimed kind K, runtime is K'≠K          → FAILURE (unsound)
--   CHECKER-HANG inference didn't terminate within the timeout    → FAILURE
--   AGREE       checker kind == runtime kind                      → ok
--   AGREE-REJECT both checker and runtime reject the expression   → ok
--   TYPED-NOEVAL checker typed it but it didn't evaluate          → noted (e.g.
--                runtime error like `head []`; NOT a type error, so not a fail)
--   INCOMPLETE  checker rejected something that evaluates fine    → noted
--                (conservative checker; these are the RC1/RC2 gaps)
--   (skipped)   no concrete claim (TVar/TAny/TUnion) or parse fail
--
-- Ground truth comes from a FROZEN GOLDEN ('goldenTable') — each corpus entry's
-- runtime kind, captured once from `nix-instantiate`. So the suite does real
-- soundness work even with no nix on PATH (e.g. the sandboxed flake check): it
-- still flags the checker claiming a kind the runtime disagrees with. When nix
-- IS present it additionally re-runs the live differential and fails on any
-- drift between the golden and real nix, so the table cannot silently rot.
-- Refresh the golden after editing 'corpus': `… narsil-oracle -- --dump-golden`.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, unless)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Narsil.Core.Draw qualified as Draw
import Narsil.Core.Safety (safeParseNixFile)
import Narsil.Inference.Nix (builtinEnv, inferExpr, inferExprWithEnv)
import Narsil.Inference.Nix.Type (NixType (..))
import Narsil.Layout.Closure (closureEnv)
import Nix.Parser (parseNixTextLoc)
import System.Directory (createDirectoryIfMissing, createDirectoryLink, findExecutable)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

-- | timeout for both the checker and a single nix-instantiate call (microseconds)
timeoutMicros :: Int
timeoutMicros = 20 * 1000000

-- ── corpus: closed expressions spanning the type system + the review fixes ──
-- Every entry must be a CLOSED Nix expression (no free variables) so
-- nix-instantiate can evaluate it. The trailing comment is just a label.
corpus :: [String]
corpus =
  [ -- literals
    "42"
  , "-7"
  , "3.14"
  , "true"
  , "null"
  , "\"hello\""
  , "./some/path"
  , -- arithmetic (REVIEW-3 #7)
    "1 + 1"
  , "1 + 1.5"
  , "2 * 3 - 4"
  , "7 / 2"
  , "1.0 + 2"
  , -- string / path concat (REVIEW-3 #7)
    "\"a\" + \"b\""
  , "./x + \"y\""
  , -- comparison / equality (REVIEW-3 #3)
    "1 == null"
  , "1 == 2"
  , "\"a\" == \"b\""
  , "1 < 2"
  , "true && false"
  , "true || false"
  , -- collections
    "[ 1 2 3 ]"
  , "{ a = 1; b = true; }"
  , "[ ]"
  , -- selection, incl. nested (REVIEW-3 #1)
    "{ a = 1; }.a"
  , "let x = { a = { b = { c = 1; }; }; }; in x.a.b.c"
  , "{ a = 1; }.z or 99"
  , -- lambdas / application
    "(x: x) 5"
  , "(x: x + 1) 41"
  , "let f = x: y: x + y; in f 2 3"
  , "x: x"
  , -- polymorphic builtins (REVIEW-3 #4, #19). Note: only `map` is in Nix's
    -- GLOBAL scope; head/filter/foldl'/elemAt/length live under `builtins.`
    -- only (bare `head` is an undefined variable at eval — see REVIEW-3 #20).
    "map (x: x + 1) [ 1 2 3 ]"
  , "builtins.head [ 10 20 ]"
  , "builtins.length [ 1 2 ]"
  , "builtins.elemAt [ 10 20 ] 1"
  , "builtins.filter (x: x) [ true false ]"
  , "builtins.foldl' (a: b: a + b) 0 [ 1 2 3 ]"
  , -- row-polymorphic attribute builtins (RC1 stage 4)
    "builtins.attrNames { a = 1; b = 2; }"
  , "builtins.attrValues { a = 1; }"
  , "builtins.hasAttr \"a\" { a = 1; }"
  , -- bare non-global builtin: checker accepts, Nix rejects (undefined var).
    -- Documents the #20 scope discrepancy; shows up as 'incomplete'/typed-noeval.
    "head [ 1 2 ]"
  , -- review-5 missing-builtins additions (the nixpkgs-sweep FP class)
    "builtins.elem 2 [ 1 2 3 ]"
  , "builtins.any (x: x) [ true false ]"
  , "builtins.all (x: x) [ true false ]"
  , "builtins.sort (a: b: a < b) [ 2 1 ]"
  , "builtins.genList (i: i * 2) 3"
  , "builtins.concatStringsSep \",\" [ \"a\" \"b\" ]"
  , "builtins.toJSON { a = 1; }"
  , "builtins.typeOf 42"
  , "builtins.splitVersion \"1.2.3\""
  , "(builtins.parseDrvName \"hello-1.0\").version"
  , "builtins.compareVersions \"1.0\" \"2.0\""
  , "builtins.toFile \"n\" \"c\"" -- a store-path STRING, not a path
  , "builtins.currentSystem"
  , "builtins.storeDir"
  , "builtins.hashString \"sha256\" \"x\""
  , "builtins.getEnv \"HOME\""
  , -- other builtins
    "toString 5"
  , "builtins.stringLength \"abc\""
  , "if true then 1 else 2"
  , -- expressions that SHOULD type-error at runtime (checker should reject too)
    "1 + \"a\""
  , "1 + true"
  ]

{- | Map an inferred type to the runtime kind string `builtins.typeOf` reports,
or Nothing when the checker made no concrete claim (so nothing to assert).
-}
expectedKind :: NixType -> Maybe String
expectedKind = \case
  TInt -> Just "int"
  TFloat -> Just "float"
  TBool -> Just "bool"
  TString -> Just "string"
  TStrLit _ -> Just "string"
  TPath -> Just "path"
  TNull -> Just "null"
  TList _ -> Just "list"
  TRec _ _ -> Just "set"
  TFun _ _ -> Just "lambda"
  TDerivation -> Just "set"
  -- no concrete claim: don't assert
  TVar _ -> Nothing
  TUnion _ -> Nothing
  TAny -> Nothing

-- ═════════════════════════════════════════════════════════════════════════════════════════════════
-- tree corpus (M size): multi-file projects through the REAL closure pipeline
-- ═════════════════════════════════════════════════════════════════════════════════════════════════

-- | One entry of a fixture tree: a file with contents, or a directory symlink.
data TreeEntry
  = TFile FilePath String
  | -- | link path ↦ target, both relative to the tree root
    TDirLink FilePath FilePath

{- | A multi-file fixture: label (the golden key), entries, and the root file to
check — possibly THROUGH a symlink, since Nix's lexical @../@ resolution is one
of the behaviors under test. Every tree gets a @flake.nix@ project marker so the
closure's project bounding matches a real checkout.
-}
data Tree = Tree
  { treeLabel :: String
  , treeEntries :: [TreeEntry]
  , treeRoot :: FilePath
  }

{- | The file-level corpus. Where the expression corpus asserts the type SYSTEM,
these assert the type PIPELINE: import resolution, the cross-module closure, env
seeding, lexical paths. Several are regression trees — under bugs fixed in
review-4 they produced wrong claims (the old interception overreach typed
@dirOf ./dep.nix@ as the dep's type; the old canonicalize-first resolution typed
a DIFFERENT FILE than Nix reads when the access path crosses a symlink).
-}
treeCorpus :: [Tree]
treeCorpus =
  [ Tree
      "import-chain"
      [ TFile "dep.nix" "{ a = 1; b = \"x\"; }"
      , TFile "main.nix" "(import ./dep.nix).a"
      ]
      "main.nix"
  , Tree
      "parent-sibling-import"
      [ TFile "lib/dep.nix" "\"hello\""
      , TFile "app/main.nix" "import ../lib/dep.nix"
      ]
      "app/main.nix"
  , Tree
      "dir-default-nix"
      [ TFile "sub/default.nix" "{ v = true; }"
      , TFile "main.nix" "(import ./sub).v"
      ]
      "main.nix"
  , Tree
      "transitive-chain"
      [ TFile "d.nix" "3"
      , TFile "b.nix" "{ x = import ./d.nix; }"
      , TFile "main.nix" "(import ./b.nix).x"
      ]
      "main.nix"
  , Tree -- REGRESSION (review-4): head guard — dirOf of a seeded path is a path
      "import-head-guard"
      [ TFile "dep.nix" "\"hello\""
      , TFile "main.nix" "let x = import ./dep.nix; in dirOf ./dep.nix"
      ]
      "main.nix"
  , Tree -- REGRESSION (review-4): seeded dep types instantiate fresh (no capture)
      "module-shaped-import"
      [ TFile "dep.nix" "{ config, lib, pkgs, ... }: { val = config; }"
      , TFile "main.nix" "let m = import ./dep.nix; in m"
      ]
      "main.nix"
  , Tree -- REGRESSION (review-4): ../ resolves lexically from the ACCESS path
      "symlink-lexical-parent"
      [ TFile "a/dep.nix" "{ v = 1; }"
      , TFile "other/dep.nix" "{ v = \"s\"; }"
      , TFile "other/real/main.nix" "(import ../dep.nix).v"
      , TDirLink "a/link" "other/real"
      ]
      "a/link/main.nix"
  , Tree -- callPackage: the call site is the dep function's RESULT type
      "callpackage-result"
      [ TFile "pkg.nix" "{ x }: { val = \"s\"; }"
      , TFile
          "main.nix"
          ( "let callPackage = f: args: import f ({ x = 1; } // args); "
              <> "in { p = callPackage ./pkg.nix { }; }"
          )
      , TFile "root.nix" "((import ./main.nix).p).val"
      ]
      "root.nix"
  , Tree -- mutual imports: nix diverges (noeval); the checker must degrade, not hang
      "import-cycle"
      [ TFile "a.nix" "{ x = (import ./b.nix).y; }"
      , TFile "b.nix" "{ y = (import ./a.nix).x; }"
      ]
      "a.nix"
  ]

-- | Materialize a tree in a temp dir (with its project marker) and run the action on the root.
withTreeDir :: Tree -> (FilePath -> IO a) -> IO a
withTreeDir t act =
  withSystemTempDirectory ("oracle-" ++ treeLabel t) $ \dir -> do
    writeFile (dir </> "flake.nix") ""
    forM_ (treeEntries t) $ \case
      TFile p contents -> do
        createDirectoryIfMissing True (takeDirectory (dir </> p))
        writeFile (dir </> p) contents
      TDirLink link target -> do
        createDirectoryIfMissing True (takeDirectory (dir </> link))
        createDirectoryLink (dir </> target) (dir </> link)
    act (dir </> treeRoot t)

-- ── checker side ──
data CheckRes = ParseFail | CheckerHang | CheckerCrash | Rejected | Accepted NixType

runChecker :: String -> IO CheckRes
runChecker e = case parseNixTextLoc (T.pack e) of
  Left _ -> pure ParseFail
  Right ast ->
    -- force enough to surface a non-terminating inference as a HANG rather
    -- than letting it wedge the whole suite (this is what catches #19-class bugs)
    timeout timeoutMicros (evaluate (classify (inferExpr ast))) >>= \case
      Nothing -> pure CheckerHang
      Just r -> pure r
 where
  classify = \case
    Left _ -> Rejected
    Right (t, _) -> expectedKind t `seq` Accepted t

{- | Tree checker: the REAL file pipeline — parse the root, build its
cross-module closure env ('closureEnv': edge discovery, lexical resolution,
dependency inference), then infer against it. Crashes and hangs are their own
verdicts (both failures): a fixture tree must never wedge or kill the suite.
-}
runTreeChecker :: FilePath -> IO CheckRes
runTreeChecker rootFile = do
  outcome <- timeout timeoutMicros (try attempt)
  pure $ case outcome of
    Nothing -> CheckerHang
    Just (Left (_ :: SomeException)) -> CheckerCrash
    Just (Right r) -> r
 where
  attempt = do
    parsed <- safeParseNixFile rootFile
    case parsed of
      Left _ -> pure ParseFail
      Right ast -> do
        env <- closureEnv builtinEnv rootFile
        evaluate $ case inferExprWithEnv env ast of
          Left _ -> Rejected
          Right (t, _) -> expectedKind t `seq` Accepted t

-- ── oracle side ──

{- | runtime kind via `nix-instantiate --eval -E 'builtins.typeOf (EXPR)'`,
or Nothing if it errors / times out (did not evaluate to a value).
-}
nixTypeOf :: String -> IO (Maybe String)
nixTypeOf e = nixEvalKind ("builtins.typeOf (" ++ e ++ ")")

-- | runtime kind of a FILE's value: `builtins.typeOf (import /abs/root.nix)`.
nixTreeTypeOf :: FilePath -> IO (Maybe String)
nixTreeTypeOf rootFile = nixEvalKind ("builtins.typeOf (import " ++ rootFile ++ ")")

-- | shared `nix-instantiate --eval -E` runner for the two oracle sides.
nixEvalKind :: String -> IO (Maybe String)
nixEvalKind arg = do
  res <-
    timeout
      timeoutMicros
      ( try (readProcessWithExitCode "nix-instantiate" ["--eval", "-E", arg] "") ::
          IO (Either SomeException (ExitCode, String, String))
      )
  pure $ case res of
    Just (Right (ExitSuccess, out, _)) -> Just (cleanKind out)
    _ -> Nothing
 where
  -- output is e.g. "\"int\"\n"; strip quotes and whitespace
  cleanKind = filter (\c -> c /= '"' && not (isSpace c))

-- ── verdicts ──
data Verdict
  = Mismatch String String -- claimed, actual
  | CheckHang
  | CheckCrash
  | Agree String
  | AgreeReject
  | TypedNoEval String
  | Incomplete String -- runtime kind it evaluated to
  | Skipped String

isFailure :: Verdict -> Bool
isFailure (Mismatch _ _) = True
isFailure CheckHang = True
isFailure CheckCrash = True
isFailure _ = False

verdict :: CheckRes -> Maybe String -> Verdict
verdict CheckerHang _ = CheckHang
verdict CheckerCrash _ = CheckCrash
verdict ParseFail _ = Skipped "parse-fail"
verdict Rejected Nothing = AgreeReject
verdict Rejected (Just k) = Incomplete k
verdict (Accepted t) moracle = case (expectedKind t, moracle) of
  (Nothing, _) -> Skipped "no-concrete-claim"
  (Just k, Just k') | k == k' -> Agree k
  (Just k, Just k') -> Mismatch k k'
  (Just k, Nothing) -> TypedNoEval k

renderVerdict :: Verdict -> String
renderVerdict = \case
  Mismatch c a -> "MISMATCH  claimed=" ++ c ++ " runtime=" ++ a
  CheckHang -> "CHECKER-HANG"
  CheckCrash -> "CHECKER-CRASH"
  Agree k -> "AGREE     " ++ k
  AgreeReject -> "AGREE-REJECT (both reject)"
  TypedNoEval k -> "typed-but-noeval (" ++ k ++ ")"
  Incomplete k -> "INCOMPLETE (checker rejected; runtime=" ++ k ++ ")"
  Skipped why -> "skipped (" ++ why ++ ")"

{- | Frozen ground truth: each corpus entry paired with the runtime kind
'nix-instantiate' reports for it ('Just' a @builtins.typeOf@ string, or 'Nothing'
when it does not evaluate to a value — e.g. @head []@ or a runtime type error).

These kinds are STABLE (@builtins.typeOf 42@ is always @"int"@), so freezing them
lets the soundness check run hermetically — no nix in the loop — and still catch
the one thing that matters: the checker claiming a kind the runtime disagrees
with (MISMATCH). The live differential ('nixTypeOf') runs whenever nix IS present
and re-verifies this table, so it cannot silently rot.

Refresh after changing 'corpus' (needs nix on PATH):

    cabal run -v0 narsil-oracle -- --dump-golden
-}
goldenTable :: [(String, Maybe String)]
goldenTable =
  [ ("42", Just "int")
  , ("-7", Just "int")
  , ("3.14", Just "float")
  , ("true", Just "bool")
  , ("null", Just "null")
  , ("\"hello\"", Just "string")
  , ("./some/path", Just "path")
  , ("1 + 1", Just "int")
  , ("1 + 1.5", Just "float")
  , ("2 * 3 - 4", Just "int")
  , ("7 / 2", Just "int")
  , ("1.0 + 2", Just "float")
  , ("\"a\" + \"b\"", Just "string")
  , ("./x + \"y\"", Just "path")
  , ("1 == null", Just "bool")
  , ("1 == 2", Just "bool")
  , ("\"a\" == \"b\"", Just "bool")
  , ("1 < 2", Just "bool")
  , ("true && false", Just "bool")
  , ("true || false", Just "bool")
  , ("[ 1 2 3 ]", Just "list")
  , ("{ a = 1; b = true; }", Just "set")
  , ("[ ]", Just "list")
  , ("{ a = 1; }.a", Just "int")
  , ("let x = { a = { b = { c = 1; }; }; }; in x.a.b.c", Just "int")
  , ("{ a = 1; }.z or 99", Just "int")
  , ("(x: x) 5", Just "int")
  , ("(x: x + 1) 41", Just "int")
  , ("let f = x: y: x + y; in f 2 3", Just "int")
  , ("x: x", Just "lambda")
  , ("map (x: x + 1) [ 1 2 3 ]", Just "list")
  , ("builtins.head [ 10 20 ]", Just "int")
  , ("builtins.length [ 1 2 ]", Just "int")
  , ("builtins.elemAt [ 10 20 ] 1", Just "int")
  , ("builtins.filter (x: x) [ true false ]", Just "list")
  , ("builtins.foldl' (a: b: a + b) 0 [ 1 2 3 ]", Just "int")
  , ("builtins.attrNames { a = 1; b = 2; }", Just "list")
  , ("builtins.attrValues { a = 1; }", Just "list")
  , ("builtins.hasAttr \"a\" { a = 1; }", Just "bool")
  , ("builtins.elem 2 [ 1 2 3 ]", Just "bool")
  , ("builtins.any (x: x) [ true false ]", Just "bool")
  , ("builtins.all (x: x) [ true false ]", Just "bool")
  , ("builtins.sort (a: b: a < b) [ 2 1 ]", Just "list")
  , ("builtins.genList (i: i * 2) 3", Just "list")
  , ("builtins.concatStringsSep \",\" [ \"a\" \"b\" ]", Just "string")
  , ("builtins.toJSON { a = 1; }", Just "string")
  , ("builtins.typeOf 42", Just "string")
  , ("builtins.splitVersion \"1.2.3\"", Just "list")
  , ("(builtins.parseDrvName \"hello-1.0\").version", Just "string")
  , ("builtins.compareVersions \"1.0\" \"2.0\"", Just "int")
  , ("builtins.toFile \"n\" \"c\"", Just "string")
  , ("builtins.currentSystem", Just "string")
  , ("builtins.storeDir", Just "string")
  , ("builtins.hashString \"sha256\" \"x\"", Just "string")
  , ("builtins.getEnv \"HOME\"", Just "string")
  , ("head [ 1 2 ]", Nothing)
  , ("toString 5", Just "string")
  , ("builtins.stringLength \"abc\"", Just "int")
  , ("if true then 1 else 2", Just "int")
  , ("1 + \"a\"", Nothing)
  , ("1 + true", Nothing)
  ]

-- | Ground truth for an entry from the frozen table, if present.
goldenFor :: String -> Maybe (Maybe String)
goldenFor e = lookup e goldenTable

{- | Frozen ground truth for the tree corpus, keyed by 'treeLabel' — same
freezing discipline as 'goldenTable' (hermetic soundness check; live drift
re-verification when nix is present). Refresh with @-- --dump-golden@.
-}
treeGoldenTable :: [(String, Maybe String)]
treeGoldenTable =
  [ ("import-chain", Just "int")
  , ("parent-sibling-import", Just "string")
  , ("dir-default-nix", Just "bool")
  , ("transitive-chain", Just "int")
  , ("import-head-guard", Just "path")
  , ("module-shaped-import", Just "lambda")
  , ("symlink-lexical-parent", Just "int")
  , ("callpackage-result", Just "string")
  , ("import-cycle", Just "set")
  ]

-- | Ground truth for a tree from the frozen table, if present.
treeGoldenFor :: String -> Maybe (Maybe String)
treeGoldenFor l = lookup l treeGoldenTable

main :: IO ()
main = do
  args <- getArgs
  if "--dump-golden" `elem` args then dumpGolden else runOracle

{- | Regenerate 'goldenTable' from the live oracle and print it as Haskell source
to paste back in. Requires nix-instantiate on PATH (it is the source of truth).
-}
dumpGolden :: IO ()
dumpGolden = do
  mNix <- findExecutable "nix-instantiate"
  case mNix of
    Nothing -> hPutStrLn stderr "--dump-golden requires nix-instantiate on PATH" >> exitFailure
    Just _ -> do
      rows <- forM corpus $ \e -> do k <- nixTypeOf e; pure (e, k)
      putStrLn "goldenTable :: [(String, Maybe String)]"
      putStrLn ("goldenTable =\n  [ " ++ intercalate "\n  , " (map show rows) ++ "\n  ]")
      treeRows <- forM treeCorpus $ \t -> do
        k <- withTreeDir t nixTreeTypeOf
        pure (treeLabel t, k)
      putStrLn ""
      putStrLn "treeGoldenTable :: [(String, Maybe String)]"
      putStrLn ("treeGoldenTable =\n  [ " ++ intercalate "\n  , " (map show treeRows) ++ "\n  ]")

runOracle :: IO ()
runOracle = do
  putStrLn "narsil differential oracle (checker vs nix-instantiate)"
  TIO.putStrLn (Draw.rule Draw.Double 60)
  -- Every corpus entry must have a frozen ground truth; a missing one means the
  -- corpus grew without a `--dump-golden` refresh, which we fail on loudly.
  let missing = [e | e <- corpus, isNothing (goldenFor e)]
  unless (null missing) $ do
    putStrLn "oracle: FAILED — corpus entries missing from goldenTable (run -- --dump-golden):"
    mapM_ (\e -> putStrLn ("  " ++ e)) missing
    exitFailure

  let treeMissing = [treeLabel t | t <- treeCorpus, isNothing (treeGoldenFor (treeLabel t))]
  unless (null treeMissing) $ do
    putStrLn
      "oracle: FAILED — tree corpus entries missing from treeGoldenTable (run -- --dump-golden):"
    mapM_ (\l -> putStrLn ("  " ++ l)) treeMissing
    exitFailure

  -- Hermetic verdicts: the (pure) checker vs the frozen ground truth.
  exprVerdicts <- forM corpus $ \e -> do
    cr <- runChecker e
    let v = verdict cr (concatGolden (goldenFor e))
    putStrLn $ "  " ++ pad 52 e ++ renderVerdict v
    pure v

  -- Tree verdicts (M size): the real file pipeline vs its frozen ground truth.
  putStrLn ""
  putStrLn "tree corpus (file pipeline: closure, imports, symlinks, callPackage)"
  treeVerdicts <- forM treeCorpus $ \t -> do
    cr <- withTreeDir t runTreeChecker
    let v = verdict cr (concatGolden (treeGoldenFor (treeLabel t)))
    putStrLn $ "  " ++ pad 52 ("tree:" ++ treeLabel t) ++ renderVerdict v
    pure v

  let verdicts = exprVerdicts ++ treeVerdicts

  -- When nix IS present, re-verify the frozen tables against the live oracle so
  -- they cannot silently drift from real nix semantics.
  mNix <- findExecutable "nix-instantiate"
  drift <- maybe (pure []) (const driftReport) mNix

  let failures = filter isFailure verdicts
      nAgree = length [() | Agree _ <- verdicts]
      nReject = length [() | AgreeReject <- verdicts]
      nIncomplete = length [() | Incomplete _ <- verdicts]
      nNoEval = length [() | TypedNoEval _ <- verdicts]
  putStrLn ""
  putStrLn
    ( maybe
        "ground truth: frozen golden (no nix on PATH)"
        (const "ground truth: frozen golden + live nix drift-check")
        mNix
    )
  putStrLn $
    "agree="
      ++ show nAgree
      ++ " agree-reject="
      ++ show nReject
      ++ " incomplete="
      ++ show nIncomplete
      ++ " typed-noeval="
      ++ show nNoEval
      ++ " FAILURES="
      ++ show (length failures)
      ++ " drift="
      ++ show (length drift)
  putStrLn "note: 'incomplete' = conservative rejection (RC1/RC2 gap), not a failure."
  unless (null drift) $ do
    putStrLn
      "oracle: golden DRIFT — frozen kinds disagree with live nix (refresh with -- --dump-golden):"
    mapM_ (putStrLn . ("  " ++)) drift
  if null failures && null drift
    then putStrLn "oracle: OK (no soundness mismatches)" >> exitSuccess
    else
      putStrLn "oracle: FAILED (soundness mismatch, checker hang, or golden drift)"
        >> exitFailure
 where
  pad n s = take n (s ++ repeat ' ')
  -- a present-but-Nothing golden entry means "did not evaluate"; flatten the
  -- Maybe (Maybe String) lookup (absence already failed loudly above).
  concatGolden Nothing = Nothing
  concatGolden (Just g) = g

{- | Compare every frozen golden kind (expressions AND trees) against a fresh
live nix-instantiate run; return a human-readable line for each disagreement.
-}
driftReport :: IO [String]
driftReport = do
  exprDrift <- forM corpus $ \e -> do
    live <- nixTypeOf e
    pure (driftLine e (goldenFor e) live)
  treeDrift <- forM treeCorpus $ \t -> do
    live <- withTreeDir t nixTreeTypeOf
    pure (driftLine ("tree:" ++ treeLabel t) (treeGoldenFor (treeLabel t)) live)
  pure (catMaybes (exprDrift ++ treeDrift))
 where
  driftLine label mg live = case mg of
    Just g | g == live -> Nothing
    Just g -> Just (pad 52 label ++ "golden=" ++ showKind g ++ " live=" ++ showKind live)
    Nothing -> Nothing
  pad n s = take n (s ++ repeat ' ')
  showKind = fromMaybe "<noeval>"
