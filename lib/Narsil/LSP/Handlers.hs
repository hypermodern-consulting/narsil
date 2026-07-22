-- yellow: lsp-types promoted 'Method_* symbols only (see HOUSE_STYLE)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // lsp // handlers
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He never saw the whole of it, only the traffic: requests arriving,
--    answers dispatched, the board never going dark."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The request registry: every LSP notification and request the editor
--   sends is matched here to its handler — lifecycle, diagnostics, hover,
--   definition, rename, references, completion, signature help, code
--   actions, document symbols, semantic tokens, inlay hints — then routed
--   to the pure compute in the sibling modules. The switchboard, plus the
--   VFS-read / safe-parse plumbing (lspSafeParse); the deciding lives next
--   door.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers (
  handlers,
  lintFile,
  fullLint,
  toNixDiag,
  nixCode,
  spToDiagnostic,
  NixViolation (..),
  ViolationType (..),
  findExprAt,
  inferExprAt,
  semanticLegend,
  enrichInferEnv,
  enrichInferEnvBatch,
)
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Exception qualified as Exc
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isAlphaNum)
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (inits, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)
import Narsil.Core.Config qualified as Cfg
import Narsil.Core.Profiles qualified as Profiles
import Narsil.Core.Safety qualified as Safety
import Narsil.Core.Span qualified as CSpan
import Narsil.Inference.Nix (builtinEnv)
import Narsil.Inference.Nix qualified as Infer
import Narsil.Inference.Nix.Environment (TypeEnv, withPkgsOracle)
import Narsil.Inference.Nix.Module qualified as Module
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.LSP.Handlers.Cursor (
  bindingValueByName,
  findExprAt,
  inferExprAt,
  inferExprAtWithEnv,
  selectAtCursor,
  selectPathAtCursor,
 )
import Narsil.LSP.Handlers.Diagnostics (
  NixViolation (..),
  ViolationType (..),
  diagnosticsForExprWith,
  moduleModeEnv,
  nixCode,
  spToDiagnostic,
  toNixDiag,
 )
import Narsil.LSP.Handlers.Features (
  PkgsCtx (..),
  attrCompletions,
  chainBeforeCursor,
  completionsForExpr,
  findRef,
  inferOptionAtPath,
  inlayHintsForExpr,
  memberCompletions,
  nixpkgsCompletionContext,
  noFile,
  parseErr,
  pkgNameCompletions,
  rangeOverlapsDiag,
  signatureAtCursor,
  toLspPos,
  violationActionIn,
 )
import Narsil.LSP.Handlers.Project (
  buildCrossEnv,
  buildCrossScopeGraphWith,
  findProjectRoot,
  getProjectCache,
  invalidateModuleGraphCache,
  latestNixpkgsIndex,
  lookupNixpkgsIndex,
  lookupOptionsIndex,
  resolveNixpkgsRoot,
  voidProjectDiags,
 )
import Narsil.LSP.Handlers.SemanticTokens (semanticLegend, semanticTokens)
import Narsil.LSP.Handlers.Symbols (collectTopBindingSymbols)
import Narsil.LSP.ProjectCache qualified as PC
import Narsil.Layout.Edge qualified as Edge
import Narsil.Layout.ModuleSystem qualified as MS
import Narsil.Layout.Scope qualified as Scope
import Narsil.Nixpkgs.Cache (
  CacheConfig (..),
  EvalCache,
  cachingBackend,
  defaultCachePath,
  flushCacheAsync,
  loadCacheFrom,
 )
import Narsil.Nixpkgs.Eval (EvalBackend (..), EvalError (..), composeBackend, shapeBackend)
import Narsil.Nixpkgs.EvalRepl (replBackend)
import Narsil.Nixpkgs.Index qualified as Nixpkgs
import Narsil.Nixpkgs.OptionsIndex qualified as Opts
import Narsil.Nixpkgs.Oracle (buildPkgsOracle, collectPkgsChainsAnn, pkgsAttrTypos)
import Narsil.Nixpkgs.Warm (WarmPool, enqueueDemand, newWarmPool, swapFocus)
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer)
import Narsil.Syntax.Parse qualified as NixParse
import Nix.Expr.Types (Binding (..), NExprF (..), NKeyName (..))
import Nix.Expr.Types.Annotated (NExprLoc)
import Nix.Expr.Types.Annotated qualified as NixAnn
import Nix.Parser (parseNixTextLoc)
import Nix.Utils qualified as NixU
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

{- | Parse text inside an LSP handler. Returns Nothing on parse failure,
depth overflow, or stack overflow — handlers respond gracefully instead of
crashing the whole server.
-}
lspSafeParse :: Text -> Maybe NExprLoc
lspSafeParse txt = unsafePerformIO $ do
  -- `evaluate` only forces to WHNF, so a bottom buried in the lazy hnix AST used
  -- to escape this `try` and detonate later when a handler (or `analyzeDepth`,
  -- which ran OUTSIDE the try) forced it. Run the depth walk — which traverses
  -- the whole tree — INSIDE the evaluated thunk so any such bottom is forced, and
  -- therefore caught, here.
  r <- try (Exc.evaluate (parseAndCheck txt))
  pure $ either (\(_ :: SomeException) -> Nothing) id r
 where
  parseAndCheck t = either (const Nothing) checkDepth (parseNixTextLoc t)
  checkDepth e = either (const Nothing) (const (Just e)) (Safety.analyzeDepth e)

{- | The eval backend for @pkgs.<pkg>.<symbol>@ completion: a content-addressed
cache wrapping the warm nix-repl pool (real names + types) in front of the
always-available shape template. The cache turns a repeat ~62ms eval into a map
lookup and survives across sessions on NVMe. When the in-house compiler lands it
composes in place of the repl pool — the cache and shape floor stay.
-}
nixpkgsBackend :: EvalBackend
nixpkgsBackend = cachingBackend nixpkgsEvalCache (composeBackend replBackend shapeBackend)

{-# NOINLINE nixpkgsEvalCache #-}

{- | The process-wide eval cache, loaded once from NVMe (or empty on first run)
using the quotas from the project config's @lsp@ block. A CAF like the other LSP
caches; forced lazily on the first symbol query — by which point 'initializedHandler'
has installed the config. Checkpointed back to disk on save (see
'documentSaveHandler').
-}
nixpkgsEvalCache :: EvalCache
nixpkgsEvalCache = unsafePerformIO $ do
  lsp <- Cfg.getLspRuntime
  defaultCachePath >>= loadCacheFrom (lspCacheConfig lsp)

-- | The cache quotas from the LSP knobs: MiB fields widened to bytes.
lspCacheConfig :: Cfg.LspConfig -> CacheConfig
lspCacheConfig lsp =
  CacheConfig
    { ccMaxMemoryBytes = mib (Cfg.lspMaxMemoryMB lsp)
    , ccMaxDiskBytes = mib (Cfg.lspMaxDiskMB lsp)
    }
 where
  mib n = fromIntegral n * 1024 * 1024

{-# NOINLINE warmPool #-}

{- | The process-wide background-warm pool: @max-threads@ workers that
pre-warm the eval cache for whatever the focus seeds. Forced lazily after the
config is installed. @wpExpand@ is a no-op for now — one-hop namespace speculation
waits on namespaces being indexed (their heads aren't yet content-addressable, so
warming their children wouldn't cache). The demand floor (visible @pkgs.<name>@
references) is the live win.
-}
warmPool :: WarmPool
warmPool = unsafePerformIO $ do
  n <- fromIntegral . Cfg.lspMaxThreads <$> Cfg.getLspRuntime
  newWarmPool n warmNixpkgsPath (\_ _ -> [])

{- | Warm one nixpkgs attribute path through the caching backend — a cache miss
evaluates (~62ms) and stores; a hit is a no-op. The index is built asynchronously
on didOpen, so a worker that pops a path before it lands briefly polls for it
(~0.4s build) rather than dropping the seed; it gives up after ~2s (no nixpkgs).
-}
warmNixpkgsPath :: [Text] -> IO (Either EvalError [Text])
warmNixpkgsPath path = go (20 :: Int)
 where
  go 0 = pure (Left Unsupported)
  go n =
    latestNixpkgsIndex
      >>= maybe (threadDelay 100_000 >> go (n - 1)) (\idx -> evalSpine nixpkgsBackend idx path)

{- | The distinct @pkgs.<chain>@ references in a document's text — the reachable
nixpkgs set of the current context, used to seed the warm frontier. A lightweight
text scan (survives the half-typed edits the parser rejects), boundary-checked so
@mypkgs.x@ does not match @pkgs@. Each dotted chain (@pkgs.python3Packages.requests@)
yields its non-empty prefixes (@["python3Packages"]@, @["python3Packages","requests"]@)
so symbol completion at every depth of the reference is a pre-warmed hit.
-}
pkgsReferences :: Text -> [[Text]]
pkgsReferences txt =
  nub
    [ prefix
    | (before, after) <- T.breakOnAll "pkgs." txt
    , leftOk before
    , let chain = readChain (T.drop 5 after)
    , not (null chain)
    , prefix <- drop 1 (inits chain)
    ]
 where
  readChain t =
    let (seg, rest) = T.span isPkgChar t
     in if T.null seg then [] else seg : afterDot rest
  afterDot rest = maybe [] dotMore (T.uncons rest)
  dotMore ('.', more) = readChain more
  dotMore _ = []
  leftOk before = maybe True (not . isPkgBoundary) (lastChar before)
  lastChar t = if T.null t then Nothing else Just (T.last t)
  isPkgBoundary ch = isPkgChar ch || ch == '.'
  isPkgChar ch = isAlphaNum ch || ch == '_' || ch == '\'' || ch == '-'

{- | Seed the inference env with precomputed @pkgs.<path>@ types for this file —
the eval/cache machinery enriching the type checker, so a nixpkgs
reference hovers as its real type, a bogus attribute is a real error, and a misuse
unifies away. Bounded to the file's references and time-boxed; with no index yet,
or on timeout, the env is returned unchanged. The warm pool (didOpen) has usually
pre-warmed exactly these paths, so the eval calls here are cache hits.
-}
enrichPkgsOracle :: Uri -> NExprLoc -> TypeEnv -> IO TypeEnv
enrichPkgsOracle uri expr env = do
  mIdx <- lookupNixpkgsIndex uri
  maybe (pure env) build mIdx
 where
  build idx = do
    mOracle <- timeout oracleBudgetMicros (buildPkgsOracle nixpkgsBackend idx expr)
    pure (maybe env (`withPkgsOracle` env) mOracle)
  oracleBudgetMicros = 4_000_000

{- | The inference env for a one-shot @infer@ of a file: the given base env plus a
SYNCHRONOUSLY-built pkgs oracle (resolve the nixpkgs root, build its index, eval the
file's @pkgs.<…>@ references). Time-boxed and best-effort — no nixpkgs, no working
@nix@, or a timeout leaves the base env untouched, so @infer@ always works. Unlike
'enrichPkgsOracle' (the hover path, which reads the async LSP index cache) this
builds the index eagerly, so it lands within a single CLI invocation.
-}
enrichInferEnv :: FilePath -> NExprLoc -> TypeEnv -> IO TypeEnv
enrichInferEnv file expr env = fromMaybe env <$> timeout enrichBudgetMicros build
 where
  build = do
    mRoot <- resolveNixpkgsRoot (filePathToUri file)
    mIdx <- traverse Nixpkgs.buildNixpkgsIndex mRoot
    maybe (pure env) withOracle mIdx
  withOracle idx = do
    oracle <- buildPkgsOracle nixpkgsBackend idx expr
    pure (withPkgsOracle oracle env)
  enrichBudgetMicros = 10_000_000

{- | The batch counterpart to 'enrichInferEnv' for @infer -r@ over a tree: resolve
the nixpkgs root and build its index ONCE (time-boxed), then hand back a per-file
enricher that rebuilds only the cheap per-expression @pkgs.<…>@ oracle. A recursive
sweep thus pays the index build a single time rather than on every file. Degrades to
the identity enricher when there is no nixpkgs / no working @nix@ / the build times
out, so @infer -r@ always proceeds.
-}
enrichInferEnvBatch :: FilePath -> IO (NExprLoc -> TypeEnv -> IO TypeEnv)
enrichInferEnvBatch root = do
  mIdx <- join <$> timeout enrichBudgetMicros build
  pure (maybe (\_ env -> pure env) perFile mIdx)
 where
  build = do
    mRoot <- resolveNixpkgsRoot (filePathToUri root)
    traverse Nixpkgs.buildNixpkgsIndex mRoot
  perFile idx expr env = do
    mOracle <- timeout oracleBudgetMicros (buildPkgsOracle nixpkgsBackend idx expr)
    pure (maybe env (`withPkgsOracle` env) mOracle)
  enrichBudgetMicros = 10_000_000
  oracleBudgetMicros = 2_000_000

{- | nixpkgs attribute-typo diagnostics for a file: build the oracle (time-boxed,
index-gated) and flag every @pkgs.<…>@ selection naming an attribute its known
closed record lacks — the "better error messages" win as real squiggles. Best
effort: no index yet, timeout, or no typos ⇒ []. Surfaced on didOpen/didSave, not
the per-keystroke didChange (which stays lint-fast).
-}
pkgsTypoDiagnostics :: Uri -> NExprLoc -> IO [Diagnostic]
pkgsTypoDiagnostics uri expr = do
  mIdx <- lookupNixpkgsIndex uri
  maybe (pure []) viaIdx mIdx
 where
  viaIdx idx = do
    mOracle <- timeout 2_000_000 (buildPkgsOracle nixpkgsBackend idx expr)
    pure (maybe [] toDiags mOracle)
  toDiags oracle =
    [spToDiagnostic msg sp | (sp, msg) <- pkgsAttrTypos oracle (collectPkgsChainsAnn expr)]

{- | The full request registry: maps every supported LSP notification/request
  method to its handler. Passed to the server as the static handler set.
-}
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initializedHandler
    , notificationHandler SMethod_TextDocumentDidOpen documentOpenHandler
    , notificationHandler SMethod_TextDocumentDidChange documentChangeHandler
    , notificationHandler SMethod_TextDocumentDidSave documentSaveHandler
    , notificationHandler SMethod_TextDocumentDidClose documentCloseHandler
    , requestHandler SMethod_TextDocumentHover hoverHandler
    , requestHandler SMethod_TextDocumentDefinition definitionHandler
    , requestHandler SMethod_TextDocumentRename renameHandler
    , requestHandler SMethod_TextDocumentReferences referencesHandler
    , requestHandler SMethod_TextDocumentCompletion completionHandler
    , requestHandler SMethod_TextDocumentSignatureHelp signatureHelpHandler
    , requestHandler SMethod_TextDocumentCodeAction codeActionHandler
    , requestHandler SMethod_TextDocumentDocumentSymbol documentSymbolHandler
    , requestHandler SMethod_TextDocumentSemanticTokensFull semanticTokensFullHandler
    , requestHandler SMethod_TextDocumentInlayHint inlayHintHandler
    , requestHandler SMethod_WorkspaceSymbol workspaceSymbolHandler
    ]

-- ═══════════════════════ lifecycle ═══════════════════════

initializedHandler :: TNotificationMessage 'Method_Initialized -> LspM () ()
initializedHandler _not = do
  -- Install the project's lsp knobs (eval-pool size, cache quotas) BEFORE the
  -- cache / warm pool are first forced. Absent or unparsable config → defaults.
  mroot <- getRootPath
  liftIO (installLspConfig mroot)
  -- Eagerly construct the project cache so its workers are running and
  -- ready to drain enqueued files as soon as the first didOpen lands.
  -- Cheap: just spawns N idle threads.
  _ <- liftIO getProjectCache
  sendNotification SMethod_WindowLogMessage $
    LogMessageParams MessageType_Info "narsil LSP — panopticon online"

{- | Load the project config (@.narsil.dhall@, or the legacy
@.nix-compile.dhall@) and install its @lsp@ knobs (defaults on miss).
-}
installLspConfig :: Maybe FilePath -> IO ()
installLspConfig mroot = do
  let root = fromMaybe "." mroot
  preferred <- doesFileExist (root </> ".narsil.dhall")
  let path = root </> (if preferred then ".narsil.dhall" else ".nix-compile.dhall")
  cfg <- either (const Cfg.defaultConfig) id <$> Cfg.loadConfig path
  Cfg.setLspRuntime (Cfg.configLsp cfg)
  Cfg.setLspProjectConfig cfg

documentOpenHandler :: TNotificationMessage 'Method_TextDocumentDidOpen -> LspM () ()
documentOpenHandler notif = do
  let TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ txt)) = notif
  -- Single-file lints (always available, never blocks) plus best-effort nixpkgs
  -- attribute-typo diagnostics (index-gated, time-boxed).
  _ <- liftIO (noteGoodParse uri txt)
  (cfg, env, path) <- diagCtx uri
  let baseDiags = fullLint cfg env path txt
  typos <- liftIO (maybe (pure []) (pkgsTypoDiagnostics uri) (lspSafeParse txt))
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri Nothing (baseDiags <> typos)
  -- BFS seed: the currently-open file is the highest priority. Workers will
  -- pick it up, expand to its imports, etc. This replaces voidProjectDiags
  -- as the "warm the cache" entry point.
  liftIO $ do
    maybe (pure ()) enqueue (uriToFilePath uri)
    -- Keep the existing flake-graph warm path for now; safe to call in
    -- parallel with the per-file cache.
    voidProjectDiags uri
    -- Warm the nixpkgs symbol index in the background so the first
    -- go-to-def on a `pkgs.<name>` resolves instantly.
    _ <- lookupNixpkgsIndex uri
    -- Focus = the nixpkgs packages this file references; the warm pool
    -- pre-evaluates their spines so the first hover/completion is a hit.
    swapFocus warmPool (pkgsReferences txt)
 where
  enqueue fp = do
    pc <- getProjectCache
    PC.enqueueFile pc fp

documentChangeHandler :: TNotificationMessage 'Method_TextDocumentDidChange -> LspM () ()
documentChangeHandler notif = do
  let TNotificationMessage _ _ params = notif
  let DidChangeTextDocumentParams
        { _textDocument = VersionedTextDocumentIdentifier{_uri = uri}
        , _contentChanges = cs
        } = params
  -- the VFS is the truth: the lsp library applies full AND incremental
  -- edits correctly; reading the raw change event treated an incremental
  -- fragment as the whole buffer
  mvf <- getVirtualFile (toNormalizedUri uri)
  let txt = maybe (firstChangeText cs) virtualFileText mvf
  _ <- liftIO (noteGoodParse uri txt)
  (cfg, env, path) <- diagCtx uri
  let diags = fullLint cfg env path txt
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri Nothing diags
  -- Re-seed the warm frontier to the edited file's nixpkgs references (a focus
  -- change is a single swap, never a restart — in-flight evals still land).
  liftIO (swapFocus warmPool (pkgsReferences txt))

firstChangeText :: [TextDocumentContentChangeEvent] -> Text
firstChangeText (TextDocumentContentChangeEvent change : _)
  | InL (TextDocumentContentChangePartial _ _ t) <- change = t
  | InR (TextDocumentContentChangeWholeDocument t) <- change = t
firstChangeText _ = ""

documentSaveHandler :: TNotificationMessage 'Method_TextDocumentDidSave -> LspM () ()
documentSaveHandler notif = do
  let TNotificationMessage _ _ (DidSaveTextDocumentParams (TextDocumentIdentifier uri) txt) = notif
  liftIO $ do
    invalidateModuleGraphCache uri
    maybe (pure ()) invalidate (uriToFilePath uri)
    -- Opportunistic, non-blocking checkpoint of the nixpkgs eval cache to NVMe.
    defaultCachePath >>= \p -> flushCacheAsync p nixpkgsEvalCache
  maybe (return ()) (publish uri) txt
 where
  -- Per-file invalidation: the saved file + its reverse-dep closure are
  -- marked Stale; the saved file is re-enqueued for immediate recompute;
  -- reverse-deps recompute lazily when something asks for them.
  invalidate fp = do
    pc <- getProjectCache
    PC.invalidateFile pc fp
  publish uri t = do
    (cfg, env, path) <- diagCtx uri
    let baseDiags = fullLint cfg env path t
    -- On save, enrich with nixpkgs attribute-typo diagnostics (the cache/warm pool
    -- have usually made the eval here cheap).
    typos <- liftIO (maybe (pure []) (pkgsTypoDiagnostics uri) (lspSafeParse t))
    sendNotification SMethod_TextDocumentPublishDiagnostics $
      PublishDiagnosticsParams uri Nothing (baseDiags <> typos)
    liftIO $ voidProjectDiags uri

documentCloseHandler :: TNotificationMessage 'Method_TextDocumentDidClose -> LspM () ()
documentCloseHandler notif = do
  let TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri)) = notif
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri Nothing []

-- ═══════════════════════ hover ═══════════════════════

hoverHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentHover
  let HoverParams textDoc pos _workDone = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe (hover noFile) (withVf uri pos) mvf
 where
  hover markup = responder $ Right $ InL $ Hover{_contents = InL markup, _range = Nothing}
  withVf uri pos vf = maybe (hover parseErr) (withExpr uri pos) (lspSafeParse (virtualFileText vf))
  withExpr uri (Position l c) expr = do
    baseEnv <- liftIO $ buildCrossEnv uri
    enriched <- liftIO $ enrichPkgsOracle uri expr baseEnv
    -- module-shaped buffers hover with the declared spine bound, exactly
    -- as diagnostics infer them — the features must agree about the buffer
    let env = moduleModeEnv enriched (fromMaybe "<buffer>" (uriToFilePath uri)) expr
    -- the options universe speaks on hover too: a `config.…` select (alias-
    -- aware) appends its declared type, documentation, and declaring file
    optDoc <- liftIO (optionHoverDoc uri (fromIntegral l) (fromIntegral c) expr)
    hover
      ( withOptDoc optDoc $
          maybe
            noExpr
            (contents env expr l c)
            (inferExprAtWithEnv env expr (fromIntegral l) (fromIntegral c))
      )
  withOptDoc mDoc (MarkupContent k v) =
    MarkupContent k (v <> maybe "" ("\n\n" <>) mDoc)
  noExpr = MarkupContent MarkupKind_Markdown "`no expression at cursor`"
  contents env expr l c t =
    MarkupContent MarkupKind_Markdown (rendered <> optInfo)
   where
    -- a bare inference variable ("a", "b1", …) means NOTHING CONSTRAINS
    -- this value here — say that, instead of leaking solver vocabulary
    rendered
      | isBareVar t = "`: " <> t <> "` — *unconstrained*"
      | otherwise = "`: " <> t <> "`"
    isBareVar v =
      not (T.null v)
        && T.all (\ch -> ch >= 'a' && ch <= 'z' || ch >= '0' && ch <= '9') v
        && (T.head v >= 'a' && T.head v <= 'z')
        && T.length v <= 3
    optInfo = maybe "" renderOpt (inferOptionAtPath env expr (fromIntegral l) (fromIntegral c))
    renderOpt oi =
      "\n\n*option* `"
        <> MS.optPath oi
        <> "` : "
        <> NT.prettyType (MS.optType oi)
        <> maybe "" ("\n\n" <>) (MS.optDescription oi)

-- ═══════════════════════ workspace symbols ═══════════════════════

workspaceSymbolHandler ::
  TRequestMessage 'Method_WorkspaceSymbol ->
  ( Either
      (TResponseError 'Method_WorkspaceSymbol)
      ([SymbolInformation] |? ([WorkspaceSymbol] |? Null)) ->
    LspT () IO ()
  ) ->
  LspM () ()
workspaceSymbolHandler req responder = do
  let TRequestMessage _ _ _ params = req
      WorkspaceSymbolParams _workDone _partial query = params
  mroot <- getRootPath
  syms <- liftIO (workspaceSymbols mroot query)
  responder $ Right $ InL syms

{- | Every top-level binding in every project @.nix@ file whose name
contains the query (case-insensitive) — "jump to any binding by name". A
direct project scan (parse + topBindings), session-memoized per root:
projects are small next to nixpkgs, and the scan sees flat repos the
import-reachable module graph cannot.
-}
workspaceSymbols :: Maybe FilePath -> Text -> IO [SymbolInformation]
workspaceSymbols mroot query =
  maybe (pure []) viaRoot mroot
 where
  q = T.toLower query
  viaRoot root = do
    entries <- projectBindings root
    pure
      [ SymbolInformation
          { _name = name
          , _kind = SymbolKind_Variable
          , _tags = Nothing
          , _containerName = Nothing
          , _deprecated = Nothing
          , _location = Location (filePathToUri file) (Range pos pos)
          }
      | (file, name, pos) <- entries
      , T.null q || q `T.isInfixOf` T.toLower name
      ]

{-# NOINLINE projectBindingsRef #-}
projectBindingsRef :: IORef (Map.Map FilePath [(FilePath, Text, Position)])
projectBindingsRef = unsafePerformIO (newIORef Map.empty)

-- | (file, binding, position) for every top-level binding under the root
projectBindings :: FilePath -> IO [(FilePath, Text, Position)]
projectBindings root = do
  memo <- readIORef projectBindingsRef
  maybe scan pure (Map.lookup root memo)
 where
  scan = do
    files <- projectNixFiles root
    entries <- concat <$> mapM fileBindings files
    modifyIORef' projectBindingsRef (Map.insert root entries)
    pure entries
  fileBindings f = do
    parsed <- NixParse.parseNixFile f
    pure (either (const []) (bindingsOf f) parsed)
  -- top-level attrset bindings PLUS the let bindings passed through on the
  -- way (the common `let helpers in { … }` file shape)
  bindingsOf f e =
    [ (f, varNameText k, annPos pos)
    | NamedVar (StaticKey k :| _) _ pos <- collectBindings e
    ]
  collectBindings (Layer (NSet _ bs)) = bs
  collectBindings (Layer (NAbs _ body)) = collectBindings body
  collectBindings (Layer (NLet bs body)) = bs ++ collectBindings body
  collectBindings (Layer (NWith _ body)) = collectBindings body
  collectBindings _ = []
  annPos p =
    let sp = srcSpanToSpan (NixAnn.SrcSpan p p)
        CSpan.Loc ln col = CSpan.spanStart sp
     in Position (fromIntegral (max 0 (ln - 1))) (fromIntegral (max 0 (col - 1)))

-- | project @.nix@ files (skipping VCS/build dirs), capped defensively
projectNixFiles :: FilePath -> IO [FilePath]
projectNixFiles root = take 2000 <$> go root
 where
  go dir = do
    names <- listDirectory dir
    concat
      <$> mapM
        ( \name -> do
            let p = dir FP.</> name
            isDir <- doesDirectoryExist p
            if isDir
              then if name `elem` skipDirs then pure [] else go p
              else pure [p | FP.takeExtension p == ".nix"]
        )
        names
  skipDirs = [".git", ".direnv", "node_modules", "result", "dist-newstyle", ".cache"]

-- ═══════════════════════ definition ═══════════════════════

definitionHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentDefinition
  let DefinitionParams textDoc pos _workDone _partialResult = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withExpr uri pos) (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR $ InR Null
  -- External-first: if the cursor is on a `pkgs.<name>` select and the nixpkgs
  -- index resolves it, jump straight into nixpkgs. Otherwise fall through to the
  -- normal cross-module scope resolution. Purely additive; never blocks (the
  -- index is Nothing until its background build lands).
  withExpr uri (Position l c) expr = do
    mIdx <- liftIO $ lookupNixpkgsIndex uri
    let mHit = mIdx >>= \idx -> nixpkgsHit idx (fromIntegral l) (fromIntegral c) expr
        li = fromIntegral l
        ci = fromIntegral c
    case mHit of -- CASE-OK: shape dispatch
      Just sp -> emitNixpkgsLoc uri sp
      Nothing ->
        -- option-declaration jump: a `config.…` (or cfg-aliased) definition
        -- navigates to its mkOption declaration in the same buffer
        case optionDeclJump li ci expr of -- CASE-OK: shape dispatch
          Just sp -> emitSpanHere uri sp
          Nothing -> do
            -- the NIXPKGS option universe: jump to the declaring module
            mOpt <- liftIO (nixpkgsOptionJump uri li ci expr)
            case mOpt of -- CASE-OK: shape dispatch
              Just (f, sp) -> emitSpanIn f sp
              Nothing -> do
                -- through-the-import jump: `dep.field` where `dep = import
                -- ./dep.nix` opens the neighbor file at field's binding
                mImp <- liftIO (importJump uri li ci expr)
                maybe (scopePath uri l c expr) (uncurry emitSpanIn) mImp
  emitSpanHere uri sp =
    responder $
      Right $
        InL
          ( Definition
              ( InL
                  ( Location
                      uri
                      ( Range
                          (toLspPos0 (CSpan.spanStart sp))
                          (toLspPos0 (CSpan.spanEnd sp))
                      )
                  )
              )
          )
  emitSpanIn file sp = emitSpanHere (filePathToUri file) sp
  toLspPos0 (CSpan.Loc ln col) =
    Position (fromIntegral (max 0 (ln - 1))) (fromIntegral (max 0 (col - 1)))
  nixpkgsHit idx l c expr = do
    (base, key) <- selectAtCursor l c expr
    if base == "pkgs" then Nixpkgs.lookupPackage idx key else Nothing
  scopePath uri l c expr = do
    sg <- liftIO $ buildCrossScopeGraphWith uri (Just expr)
    let cursorLine = fromIntegral l + 1; cursorCol = fromIntegral c + 1
    maybe nullResp (resolveRef uri sg) (findRef (cursorLine, cursorCol) sg)
  emitNixpkgsLoc uri sp =
    let declUri = maybe uri filePathToUri (CSpan.spanFile sp)
        zeroBased n = fromIntegral (max 0 (n - 1))
        toPos (CSpan.Loc ln col) = Position (zeroBased ln) (zeroBased col)
        loc = Location declUri (Range (toPos (CSpan.spanStart sp)) (toPos (CSpan.spanEnd sp)))
     in responder $ Right $ InL (Definition (InL loc))
  resolveRef uri sg ref = either (const nullResp) (emitDecl uri) (Scope.resolve sg ref)
  emitDecl uri decl =
    let declUri = spanFileUri uri (Scope.spanFile (Scope.declSpan decl))
        loc =
          Location
            declUri
            ( Range
                (toLspPos (Scope.spanStart (Scope.declSpan decl)))
                (toLspPos (Scope.spanEnd (Scope.declSpan decl)))
            )
     in responder $ Right $ InL (Definition (InL loc))

{- | `config.…` completion from the nixpkgs options index: resolve the
dotted chain (alias-aware) to an option path prefix and offer the NEXT
segments — leaves with their declared types, namespaces marked as such.
-}
optionCompletions ::
  Uri -> Text -> Int -> Int -> Maybe NExprLoc -> IO [CompletionItem]
optionCompletions uri txt l c mExpr =
  case chainAt of -- CASE-OK: shape dispatch
    Just (["inputs"], partial) -> inputsCompletions uri partial
    _ ->
      maybe (pure []) viaChain $ do
        expr <- mExpr
        (base : rest, partial) <- chainAt
        full <- configPathOf expr base rest
        Just (full, partial)
 where
  chainAt = do
    line <- listToMaybe (drop l (T.lines txt))
    Just (chainBeforeCursor (T.take c line))
  viaChain (prefix, partial) = do
    mIdx <- lookupOptionsIndex uri
    pure $ case mIdx of -- CASE-OK: shape dispatch
      Nothing -> []
      Just idx ->
        [ mkOptItem name mty
        | (name, mty) <- Opts.childrenAt idx prefix partial
        ]
  mkOptItem name mty =
    optItemBase
      { _label = name
      , _kind = Just CompletionItemKind_Property
      , _detail = Just (fromMaybe "namespace" mty)
      }

-- | `inputs.` completes from the project's flake.lock
inputsCompletions :: Uri -> Text -> IO [CompletionItem]
inputsCompletions uri partial = do
  mRoot <- findProjectRoot uri
  maybe (pure []) viaRoot mRoot
 where
  viaRoot root = do
    let lock = root </> "flake.lock"
    present <- doesFileExist lock
    if not present
      then pure []
      else do
        parsed <- eitherDecodeFileStrict lock
        pure (either (const []) items parsed)
  items v =
    [ (mkFlakeItem name)
    | name <- lockInputNames v
    , partial `T.isPrefixOf` name
    ]
  mkFlakeItem name =
    optItemBase
      { _label = name
      , _kind = Just CompletionItemKind_Module
      , _detail = Just "flake input"
      }

-- | the root node's input names from a parsed flake.lock
lockInputNames :: Aeson.Value -> [Text]
lockInputNames v =
  fromMaybe [] $ do
    Aeson.Object o <- Just v
    Aeson.String rootName <- KeyMap.lookup "root" o
    Aeson.Object nodes <- KeyMap.lookup "nodes" o
    Aeson.Object rootNode <- KeyMap.lookup (Key.fromText rootName) nodes
    Aeson.Object inputs <- KeyMap.lookup "inputs" rootNode
    Just (map Key.toText (KeyMap.keys inputs))

-- | a blank completion item to record-update from
optItemBase :: CompletionItem
optItemBase =
  CompletionItem
    { _label = ""
    , _labelDetails = Nothing
    , _kind = Nothing
    , _tags = Nothing
    , _detail = Nothing
    , _documentation = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = Nothing
    , _insertTextFormat = Nothing
    , _insertTextMode = Nothing
    , _textEdit = Nothing
    , _textEditText = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

{- | The options-universe hover payload for a `config.…` select at the
cursor: declared type, documentation, and the declaring file.
-}
optionHoverDoc :: Uri -> Int -> Int -> NExprLoc -> IO (Maybe Text)
optionHoverDoc uri l c expr =
  maybe (pure Nothing) viaIndex $ do
    (base, path) <- selectPathAtCursor l c expr
    configPathOf expr base path
 where
  viaIndex full = do
    mIdx <- lookupOptionsIndex uri
    pure $ do
      idx <- mIdx
      e <- Opts.lookupExact idx full
      Just (renderEntry e)
  renderEntry e =
    "*option* `"
      <> T.intercalate "." (Opts.oePath e)
      <> "` : "
      <> Opts.oeType e
      <> maybe "" ("\n\n" <>) (Opts.oeDoc e)
      <> "\n\n*declared in* `"
      <> T.pack (Opts.oeFile e)
      <> "`"

{- | Resolve @base@ + trailing keys to a full `config.…` option path: the
base is @config@ itself, or a binding aliasing a `config` select
(@cfg = config.services.foo@).
-}
configPathOf :: NExprLoc -> Text -> [Text] -> Maybe [Text]
configPathOf expr base path
  | base == "config" = Just path
  | otherwise = do
      v <- bindingValueByName base expr
      pre <- configSelectPath v
      Just (pre ++ path)
 where
  configSelectPath (Layer (NSelect _ inner p))
    | Just "config" <- symOfE inner =
        Just [varNameText k | StaticKey k <- toList p]
  configSelectPath _ = Nothing
  symOfE (Layer (NSym n)) = Just (varNameText n)
  symOfE _ = Nothing

{- | The option-declaration jump within the SAME buffer: a `config.…` select
(alias-aware) resolves to the span of its local mkOption declaration.
-}
optionDeclJump :: Int -> Int -> NExprLoc -> Maybe CSpan.Span
optionDeclJump l c expr = do
  (base, path) <- selectPathAtCursor l c expr
  full <- configPathOf expr base path
  Module.optionDeclSpanFor full (moduleBodyOf expr)
 where
  moduleBodyOf (Layer (NAbs _ b)) = moduleBodyOf b
  moduleBodyOf e = e

{- | The NIXPKGS option jump: same resolution, answered from the options
INDEX — the cursor on `config.services.nginx.enable` opens the declaring
module file in nixpkgs at the mkOption's span.
-}
nixpkgsOptionJump :: Uri -> Int -> Int -> NExprLoc -> IO (Maybe (FilePath, CSpan.Span))
nixpkgsOptionJump uri l c expr =
  maybe (pure Nothing) viaIndex $ do
    (base, path) <- selectPathAtCursor l c expr
    configPathOf expr base path
 where
  viaIndex full = do
    mIdx <- lookupOptionsIndex uri
    pure $ do
      idx <- mIdx
      e <- Opts.lookupExact idx full
      Just (Opts.oeFile e, Opts.oeSpan e)

{- | The through-the-import jump: @dep.field@ where @dep = import ./dep.nix@
opens the neighbor file at @field@'s top-level binding.
-}
importJump :: Uri -> Int -> Int -> NExprLoc -> IO (Maybe (FilePath, CSpan.Span))
importJump uri l c expr =
  maybe (pure Nothing) resolve $ do
    (base, path) <- selectPathAtCursor l c expr
    key <- listToMaybe path
    v <- bindingValueByName base expr
    rel <- importPathOf v
    file <- uriToFilePath uri
    pure (FP.takeDirectory file FP.</> rel, key)
 where
  resolve (target, key) = do
    parsed <- NixParse.parseNixFile target
    pure $ either (const Nothing) (fmap (target,) . bindingSpanIn key) parsed
  importPathOf (Layer (NApp f (Layer (NLiteralPath p))))
    | isImportHead f = Just (coerce p)
  importPathOf _ = Nothing
  isImportHead (Layer (NSym n)) = varNameText n == "import"
  isImportHead (Layer (NSelect _ _ (StaticKey k :| []))) = varNameText k == "import"
  isImportHead _ = False
  bindingSpanIn key e =
    listToMaybe
      [ annSpanToSpan pos
      | NamedVar (StaticKey k :| _) _ pos <- Edge.topBindings e
      , varNameText k == key
      ]
  annSpanToSpan p = srcSpanToSpan (NixAnn.SrcSpan p p)

-- ═══════════════════════ rename ═══════════════════════

renameHandler ::
  TRequestMessage 'Method_TextDocumentRename ->
  (Either (TResponseError 'Method_TextDocumentRename) (WorkspaceEdit |? Null) -> LspT () IO ()) ->
  LspM () ()
renameHandler req responder = do
  let TRequestMessage _ _ _ params = req
  let RenameParams _workDone textDoc pos newName = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withExpr uri pos newName) (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR Null
  withExpr uri (Position l c) newName expr =
    let sg = Scope.fromNixExpr Nothing expr
        cl = fromIntegral l + 1
        cc = fromIntegral c + 1
     in maybe nullResp (resolveRef uri sg newName) (findRef (cl, cc) sg)
  resolveRef uri sg newName ref =
    either (const nullResp) (emitEdit uri sg newName) (Scope.resolve sg ref)
  emitEdit uri sg newName decl =
    let allRefs = Scope.findReferences sg decl
        declEdit =
          TextEdit
            ( Range
                (toLspPos (Scope.spanStart (Scope.declSpan decl)))
                (toLspPos (Scope.spanEnd (Scope.declSpan decl)))
            )
            newName
        refEdits =
          [ TextEdit
              ( Range
                  (toLspPos (Scope.spanStart (Scope.refSpan r)))
                  (toLspPos (Scope.spanEnd (Scope.refSpan r)))
              )
              newName
          | r <- allRefs
          ]
        wsEdit =
          WorkspaceEdit
            { _changes = Just (Map.singleton uri (declEdit : refEdits))
            , _documentChanges = Nothing
            , _changeAnnotations = Nothing
            }
     in responder $ Right $ InL wsEdit

-- ═══════════════════════ references ═══════════════════════

referencesHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentReferences
  let ReferenceParams textDoc pos _workDone _partialResult _context = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withExpr uri pos) (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR Null
  withExpr uri (Position l c) expr = do
    sg <- liftIO $ buildCrossScopeGraphWith uri (Just expr)
    let cl = fromIntegral l + 1; cc = fromIntegral c + 1
    maybe nullResp (resolveRef uri sg) (findRef (cl, cc) sg)
  resolveRef uri sg ref = either (const nullResp) (emitRefs uri sg) (Scope.resolve sg ref)
  emitRefs uri sg decl =
    let allRefs = Scope.findReferences sg decl
        declLoc =
          Location
            uri
            ( Range
                (toLspPos (Scope.spanStart (Scope.declSpan decl)))
                (toLspPos (Scope.spanEnd (Scope.declSpan decl)))
            )
        refLocs = map (refLoc uri) allRefs
     in responder $ Right $ InL (declLoc : refLocs)
  refLoc uri r =
    let refUri = spanFileUri uri (Scope.spanFile (Scope.refSpan r))
     in Location
          refUri
          ( Range
              (toLspPos (Scope.spanStart (Scope.refSpan r)))
              (toLspPos (Scope.spanEnd (Scope.refSpan r)))
          )

-- ═══════════════════════ completion ═══════════════════════

completionHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentCompletion
  let CompletionParams textDoc pos _workDone _partialResult _context = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withVf uri pos) mvf
 where
  nullResp = responder $ Right $ InR (InR Null)
  withVf uri (Position l c) vf = do
    let txt = virtualFileText vf
    -- nixpkgs completion works off the raw text, so it survives the half-typed
    -- source the parser rejects; scope/builtin completion needs a parse. Union
    -- both. Neither blocks (the index is Nothing until warm).
    idx <- liftIO $ lookupNixpkgsIndex uri
    env <- liftIO $ buildCrossEnv uri
    let li = fromIntegral l
        ci = fromIntegral c
    nixItems <- liftIO $ maybe (pure []) (nixpkgsItems txt li ci) idx
    mExpr <- liftIO (noteGoodParse uri txt)
    let path = fromMaybe "<buffer>" (uriToFilePath uri)
        env' = maybe env (moduleModeEnv env path) mExpr
        -- THE PANOPTICON TIER: dotted chains complete from the chain's
        -- INFERRED type — locals, cfg spines, closure-typed imports,
        -- builtins, lib
        members =
          maybe
            []
            (\e -> memberCompletions env' (Infer.inferExprBindingsPartial env' e) txt li ci)
            mExpr
        scopeItems = maybe [] (\e -> completionsForExpr env' txt e li ci) mExpr
    -- the OPTIONS universe: `config.…` (alias-aware) completes from the
    -- nixpkgs-wide declaration index
    optItems <- liftIO (optionCompletions uri txt li ci mExpr)
    let chosen
          | not (null nixItems) = nixItems
          | not (null optItems) = optItems
          | not (null members) = members
          | otherwise = scopeItems
    responder $ Right $ InL chosen
  -- Package names are pure (index keys); a package's symbols go through the eval
  -- backend — the shape template today, the nixlang compiler when it lands.
  nixpkgsItems txt li ci idx =
    maybe (pure []) (resolveCtx idx) (nixpkgsCompletionContext txt li ci)
  resolveCtx idx (PkgName prefix) = pure (pkgNameCompletions idx prefix)
  resolveCtx idx (PkgSymbol path prefix) = do
    -- real names via the warm nix-repl pool, falling back to the shape template.
    spine <- evalSpine nixpkgsBackend idx path
    either (const (pure [])) (served path prefix) spine
  served path prefix names = do
    -- Prefix-aware pre-warm: warm the spines of the children the user is narrowing
    -- toward, so drilling into one is a hit. Bounded — skip an empty prefix (would
    -- be the whole namespace) and cap the batch.
    warmMatchingChildren path prefix names
    pure (attrCompletions "nixpkgs attr" names prefix)
  warmMatchingChildren path prefix names
    | T.null prefix = pure ()
    | otherwise =
        enqueueDemand
          warmPool
          (take maxWarmChildren [path <> [n] | n <- names, prefix `T.isPrefixOf` n])

-- | Cap on children pre-warmed per completion — bounds a short prefix's fan-out.
maxWarmChildren :: Int
maxWarmChildren = 32

-- ═══════════════════════ signature help ═══════════════════════

signatureHelpHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentSignatureHelp
  let SignatureHelpParams{_textDocument = textDoc, _position = pos} = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withExpr uri pos) (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR Null
  withExpr uri (Position l c) expr = do
    env <- liftIO $ buildCrossEnv uri
    maybe
      nullResp
      (responder . Right . InL)
      (signatureAtCursor env expr (fromIntegral l) (fromIntegral c))

-- ═══════════════════════ code actions ═══════════════════════

codeActionHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentCodeAction
  let CodeActionParams _workDone _partialResult textDoc range _context = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withVf uri range) mvf
 where
  nullResp = responder $ Right $ InR Null
  withVf uri range vf = do
    (cfg, env, path) <- diagCtx uri
    let txt = virtualFileText vf
        diags = fullLint cfg env path txt
        inRange = filter (rangeOverlapsDiag range) diags
        mSg = Scope.fromNixExpr Nothing <$> lspSafeParse txt
        actions = concatMap (violationActionIn (Just txt) uri mSg) inRange
    responder $ Right $ InL (map InR actions)

-- ═══════════════════════ document symbols ═══════════════════════

documentSymbolHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentDocumentSymbol
  let DocumentSymbolParams _workDone _partialResult textDoc = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe emptyResp withExpr (mvf >>= lspSafeParse . virtualFileText)
 where
  emptyResp = responder $ Right $ InR (InL [])
  withExpr expr = responder $ Right $ InR (InL (collectTopBindingSymbols expr))

-- ═══════════════════════ semantic tokens ═══════════════════════

semanticTokensFullHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentSemanticTokensFull
  let SemanticTokensParams _workDone _partialResult textDoc = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp withExpr (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR Null
  withExpr expr = responder $ Right $ InL (semanticTokens expr)

-- ═══════════════════════ inlay hints ═══════════════════════

inlayHintHandler req responder = do
  let TRequestMessage _ _ _ params = req :: TRequestMessage 'Method_TextDocumentInlayHint
  let InlayHintParams _workDone textDoc range = params
  let TextDocumentIdentifier uri = textDoc
  mvf <- getVirtualFile (toNormalizedUri uri)
  maybe nullResp (withExpr uri range) (mvf >>= lspSafeParse . virtualFileText)
 where
  nullResp = responder $ Right $ InR Null
  withExpr uri range expr = do
    baseEnv <- liftIO $ buildCrossEnv uri
    let env = moduleModeEnv baseEnv (fromMaybe "<buffer>" (uriToFilePath uri)) expr
    responder $ Right $ InL (inlayHintsForExpr env expr range)

-- ═══════════════════════ diagnostics engine ═══════════════════════

{- | Single-file diagnostics for buffer text: safe-parse, then type-check +
  lint under the given config's severity\/suppression judgment against the
  supplied (cache-backed cross-module) env. The real file path drives
  module-kind detection, so a flake\/module buffer infers exactly as
  `narsil check` would. Never blocks; empty list on parse failure.
-}
fullLint :: Cfg.Config -> TypeEnv -> FilePath -> Text -> [Diagnostic]
fullLint cfg env path txt
  -- an IGNORED file gets no squiggles — the same globs (explicit +
  -- profile-contributed, `off` ignores the world) the CLI walker honors
  | Profiles.isIgnored cfg path = []
  | otherwise = maybe [] (diagnosticsForExprWith cfg env path) (lspSafeParse txt)

{-# NOINLINE lastGoodParseRef #-}

{- | The LAST GOOD parse per buffer. Dotted completion fires at exactly the
moment the buffer does NOT parse (`server.|`), so the panopticon tier
answers from the most recent successful AST — types don't change while you
type a field name.
-}
lastGoodParseRef :: IORef (Map.Map Uri NExprLoc)
lastGoodParseRef = unsafePerformIO (newIORef Map.empty)

noteGoodParse :: Uri -> Text -> IO (Maybe NExprLoc)
noteGoodParse uri txt =
  maybe (Map.lookup uri <$> readIORef lastGoodParseRef) record (lspSafeParse txt)
 where
  record e = do
    modifyIORef' lastGoodParseRef (Map.insert uri e)
    pure (Just e)

{- | A span's file as a URI — falling back to the REQUEST's uri for spans
whose file is absent or the parser's buffer placeholder (an expression
parsed from editor text carries @"<string>"@, which is not a path).
-}
spanFileUri :: Uri -> Maybe FilePath -> Uri
spanFileUri _fallback (Just f) | f /= "<string>" = filePathToUri f
spanFileUri fallback _ = fallback

-- | The config + cross-env + path triple every diagnostics publish needs.
diagCtx :: Uri -> LspM () (Cfg.Config, TypeEnv, FilePath)
diagCtx uri = do
  cfg <- liftIO Cfg.getLspProjectConfig
  env <- liftIO (buildCrossEnv uri)
  pure (cfg, env, fromMaybe "<buffer>" (uriToFilePath uri))

-- ═══════════════════════ legacy lint ═══════════════════════

-- | Legacy alias: default config, baseline env, anonymous buffer.
lintFile :: Text -> [Diagnostic]
lintFile = fullLint Cfg.defaultConfig builtinEnv "<buffer>"
