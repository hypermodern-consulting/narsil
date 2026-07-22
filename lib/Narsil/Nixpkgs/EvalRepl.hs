{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // nixpkgs // eval // repl
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "A nest of small bright machines, each one waiting, warm, for the
--    question only it could answer."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   An 'EvalBackend' backed by a pool of WARM @nix repl@ processes — REAL Nix
--   evaluation (names AND types, the full attr set), the interim engine until
--   the in-house compiler lands. @nix repl@ reads expressions off stdin and
--   stays alive, so each process forces the nixpkgs fixpoint ONCE (≈0.3s) and
--   then answers per-attr queries in milliseconds.
--
--   Protocol: write @builtins.toJSON (<query>)@ followed by a sentinel line;
--   read stdout until the sentinel; the result is toJSON's output (so nothing is
--   elided), un-nix-escaped and decoded as JSON. A query that errors prints to
--   stderr (discarded) and the sentinel still lands, so we see no result and
--   report failure — the caller falls back to the shape template.
--
--   This is a STOPGAP that introduces a runtime dependency on a working @nix@.
--   It degrades gracefully: if @nix@ is absent or a process dies, the op returns
--   'Left' and 'composeBackend' falls through.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Nixpkgs.EvalRepl (
  replBackend,

  -- * Protocol internals (exposed for testing)
  pathExpr,
  unNixString,
  stripAnsi,
  nixTypeOf,
)
where

import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, join, void)
import Data.Aeson (decodeStrict)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Narsil.Core.Config (getLspRuntime, lspMaxThreads)
import Narsil.Inference.Nix.Type (NixType (..))
import Narsil.Nixpkgs.Eval (EvalBackend (..), EvalError (..))
import Narsil.Nixpkgs.Index (nixpkgsRoot)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  createProcess,
  proc,
  terminateProcess,
 )
import System.Timeout (timeout)

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the backend
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

{- | The nix-repl-pool backend: real evaluation of @pkgs.<path>@ attribute names
and field types. Each op resolves the nixpkgs root from the index, borrows a warm
process, and runs one query. 'Left' on any failure (no nix, dead process,
timeout, eval error) — the caller composes this in front of the shape template.
-}
replBackend :: EvalBackend
replBackend =
  EvalBackend
    { backendName = "nix-repl-pool"
    , evalSpine = \idx path ->
        runQuery
          (nixpkgsRoot idx)
          ("builtins.attrNames (" <> pathExpr (nixpkgsRoot idx) path <> ")")
          decodeNames
    , evalFieldType = \idx path field ->
        runQuery
          (nixpkgsRoot idx)
          ("builtins.typeOf (" <> pathExpr (nixpkgsRoot idx) (path <> [field]) <> ")")
          decodeFieldType
    }

-- | Run one toJSON query against a warm process and decode it, or 'Left'.
runQuery :: FilePath -> Text -> (Text -> Maybe a) -> IO (Either EvalError a)
runQuery root inner decode = do
  mres <- withProc root (`query` inner)
  pure (toEither (join mres >>= decode))
 where
  toEither = maybe (Left (EvalFailed "nix repl: no result")) Right

{- | A nixpkgs attribute path as a Nix expression: @(import /root {})."a"."b"@.
Quoted attr access handles names with dots/dashes/plus; quotes/backslashes are
escaped defensively.
-}
pathExpr :: FilePath -> [Text] -> Text
pathExpr root path =
  "(import " <> T.pack root <> " {})" <> T.concat (map sel path)
 where
  sel seg = ".\"" <> escapeAttr seg <> "\""
  escapeAttr = T.replace "\"" "\\\"" . T.replace "\\" "\\\\"

-- | Decode toJSON's output for @attrNames@: a JSON array of strings.
decodeNames :: Text -> Maybe [Text]
decodeNames = decodeStrict . TE.encodeUtf8

-- | Decode toJSON's output for @typeOf@: a JSON string → 'NixType'.
decodeFieldType :: Text -> Maybe NixType
decodeFieldType t = nixTypeOf <$> decodeStrict (TE.encodeUtf8 t)

-- | Map a Nix @builtins.typeOf@ tag to a 'NixType'; coarse cases stay 'TAny'.
nixTypeOf :: Text -> NixType
nixTypeOf "int" = TInt
nixTypeOf "bool" = TBool
nixTypeOf "float" = TFloat
nixTypeOf "string" = TString
nixTypeOf "path" = TPath
nixTypeOf "null" = TNull
nixTypeOf "list" = TList TAny
nixTypeOf _ = TAny

-- ─────────────────────────────────────────────────────────────────────────────────────────────────
-- the warm process pool
-- ─────────────────────────────────────────────────────────────────────────────────────────────────

-- | One warm @nix repl@: its stdin, stdout, and handle.
data ReplProc = ReplProc
  { rpIn :: !Handle
  , rpOut :: !Handle
  , rpProc :: !ProcessHandle
  }

{-# NOINLINE replPools #-}

-- | An idle-process channel per nixpkgs root. CAF, like the other LSP caches.
replPools :: MVar (Map FilePath (TChan ReplProc))
replPools = unsafePerformIO (newMVar Map.empty)

{- | How many warm processes per root (the symbol-completion parallelism), from
the @max-threads@ LSP knob (see "Narsil.Core.Config"), floored at 1. Read when
a root's pool is first created — after the server has installed the project config.
-}
poolSize :: IO Int
poolSize = fromIntegral . max 1 . lspMaxThreads <$> getLspRuntime

-- | The sentinel that delimits one query's output (a Nix string literal).
sentinel :: Text
sentinel = "\"NCNIXSENTINEL\""

-- | Get-or-create the idle channel for a root, kicking off its warm processes.
getChan :: FilePath -> IO (TChan ReplProc)
getChan root = modifyMVar replPools $ \m ->
  maybe (create m) (\ch -> pure (m, ch)) (Map.lookup root m)
 where
  create m = do
    ch <- newTChanIO
    n <- poolSize
    forM_ [1 .. n] $ \_ -> void (async (spawnInto root ch))
    pure (Map.insert root ch m, ch)

-- | Start a warm process and hand it to the channel (or drop it on failure).
spawnInto :: FilePath -> TChan ReplProc -> IO ()
spawnInto _root ch = startProc >>= maybe (pure ()) (atomically . writeTChan ch)

{- | Borrow a warm process, run an action, and return it to the pool — unless the
action timed out or threw, in which case the (possibly poisoned) process is
killed and a fresh one is spawned to replace it. 'Nothing' if none is reachable.
-}
withProc :: FilePath -> (ReplProc -> IO a) -> IO (Maybe a)
withProc root act = do
  ch <- getChan root
  mrp <- timeout acquireTimeoutMicros (atomically (readTChan ch))
  maybe (pure Nothing) (use ch) mrp
 where
  use ch rp = do
    outcome <- timeout queryTimeoutMicros (try (act rp))
    finish ch rp outcome
  finish ch rp (Just (Right a)) = atomically (writeTChan ch rp) >> pure (Just a)
  finish ch rp (Just (Left (_ :: SomeException))) = replace ch rp >> pure Nothing
  finish ch rp Nothing = replace ch rp >> pure Nothing
  replace ch rp = killProc rp >> void (async (spawnInto root ch))
  acquireTimeoutMicros = 60_000_000
  queryTimeoutMicros = 30_000_000

-- | Spawn @nix repl@, configure buffering, and sync past its startup banner.
startProc :: IO (Maybe ReplProc)
startProc = do
  let cp =
        (proc "nix" ["repl", "--option", "allow-import-from-derivation", "false"])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = CreatePipe
          }
  spawned <- try (createProcess cp) :: IO (Either SomeException ProcHandles)
  either (const (pure Nothing)) setUp spawned
 where
  setUp (Just hin, Just hout, Just herr, ph) = do
    hSetBuffering hin LineBuffering
    hSetBuffering hout LineBuffering
    -- drain stderr (prompts, errors) so it can't fill and block the process.
    void (async (drain herr))
    let rp = ReplProc hin hout ph
    synced <- timeout 30_000_000 (sync rp)
    maybe (killProc rp >> pure Nothing) (const (pure (Just rp))) synced
  setUp _ = pure Nothing
  drain h = try (TIO.hGetContents h) >>= \(_ :: Either SomeException Text) -> pure ()
  -- send a bare sentinel and read until it echoes — past the banner, ready.
  sync rp = do
    TIO.hPutStr (rpIn rp) (sentinel <> "\n")
    hFlush (rpIn rp)
    void (readUntilSentinel (rpOut rp))

type ProcHandles = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

-- | Send @builtins.toJSON (inner)@ + the sentinel; return the decoded JSON text.
query :: ReplProc -> Text -> IO (Maybe Text)
query rp inner = do
  TIO.hPutStr (rpIn rp) ("builtins.toJSON (" <> inner <> ")\n" <> sentinel <> "\n")
  hFlush (rpIn rp)
  fmap unNixString <$> readUntilSentinel (rpOut rp)

{- | Read stdout lines until the sentinel echoes; return the last result-looking
line before it (a Nix string literal), if any.
-}
readUntilSentinel :: Handle -> IO (Maybe Text)
readUntilSentinel hout = go Nothing
 where
  go acc = do
    eline <- try (TIO.hGetLine hout) :: IO (Either SomeException Text)
    either (const (pure acc)) (step acc) eline
  step acc line =
    let s = T.strip (stripAnsi line)
     in if s == sentinel
          then pure acc
          else go (if isResult s then Just s else acc)
  isResult s = not (T.null s) && T.head s == '"'

{- | Un-nix-escape a printed string literal into its content: drop the surrounding
quotes, then @\\"@ → @"@ and @\\\\@ → @\\@. For our JSON payloads (string arrays,
type tags) that recovers valid JSON.
-}
unNixString :: Text -> Text
unNixString line =
  maybe line unescape (T.stripPrefix "\"" line >>= T.stripSuffix "\"")
 where
  unescape = T.replace "\\\"" "\"" . T.replace "\\\\" "\\"

-- | Strip ANSI SGR escape sequences (@ESC [ … m@) the repl colourises output with.
stripAnsi :: Text -> Text
stripAnsi = go
 where
  go t =
    let (before, rest) = T.breakOn "\ESC[" t
     in if T.null rest
          then before
          else before <> go (T.drop 1 (T.dropWhile (/= 'm') rest))

-- | Kill a process and close its handles, ignoring failures.
killProc :: ReplProc -> IO ()
killProc rp = do
  void (try (terminateProcess (rpProc rp)) :: IO (Either SomeException ()))
  void (try (hClose (rpIn rp)) :: IO (Either SomeException ()))
  void (try (hClose (rpOut rp)) :: IO (Either SomeException ()))
