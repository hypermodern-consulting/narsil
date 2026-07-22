{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                    // lsp // handlers // features
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He'd operated on an almost permanent adrenaline high, a byproduct of youth
--    and proficiency, jacked into a custom cyberspace deck."
--
--                                                                                     — Neuromancer
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--   The pure compute behind the language-feature handlers: cursor-driven
--   navigation (go-to-definition / rename / references), completion, signature
--   help, code actions, inlay hints, and option lookup. Each takes an AST (and
--   sometimes a 'TypeEnv') plus a cursor position and returns LSP wire types —
--   no I/O, no LspM. The handlers in "Narsil.LSP.Handlers" do the VFS reads
--   and responder plumbing; everything they decide WITH lives here.
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Handlers.Features (
  -- navigation
  findRef,
  toLspPos,
  -- completion
  completionsForExpr,
  memberCompletions,
  PkgsCtx (..),
  nixpkgsCompletionContext,
  chainBeforeCursor,
  pkgNameCompletions,
  attrCompletions,
  -- signature help
  signatureAtCursor,
  -- code actions
  rangeOverlapsDiag,
  violationAction,
  violationActionIn,
  -- inlay hints
  inlayHintsForExpr,
  -- option lookup + hover fallbacks
  inferOptionAtPath,
  noFile,
  parseErr,
)
where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Data.Char (isAlphaNum)
import Data.List (find, nub, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Language.LSP.Protocol.Types
import Narsil.Core.Span (Loc (..), Span (..))
import Narsil.Inference.Nix (TypeEnv (..), builtinEnv)
import Narsil.Inference.Nix qualified as Infer
import Narsil.Inference.Nix.Builtins (builtinSchemeTable)
import Narsil.Inference.Nix.Lib (libSchemeTable)
import Narsil.Inference.Nix.Type qualified as NT
import Narsil.LSP.Handlers.Cursor (childExprs, exprName, findExprAt)
import Narsil.Layout.ModuleSystem qualified as MS
import Narsil.Layout.Scope qualified as Scope
import Narsil.Lint.Nix qualified as NixLint
import Narsil.Nixpkgs.Index qualified as Nixpkgs
import Narsil.Syntax.Annotation (srcSpanToSpan, varNameText, pattern Layer, pattern LayerAnn)
import Nix.Expr.Types (Binding (..), NExprF (..), NKeyName (..), Params (..), Recursivity (..))
import Nix.Expr.Types.Annotated (NExprLoc)

-- ═══════════════════════ navigation ═══════════════════════

{- | Pure: find the reference in the scope graph whose span contains the
  1-based @(line, col)@ cursor. The cursor ON a DECLARATION also resolves —
  as a self-reference at the binding site — so references/rename work from
  the place users most often invoke them.

  Overlapping candidates are ranked NARROWEST-first (an attribute reference
  spans its whole select — `myServer.hostname` — and must not shadow the
  `myServer` variable under the cursor), with variable references preferred
  over attribute references on equal extent.
-}
findRef :: (Int, Int) -> Scope.ScopeGraph -> Maybe Scope.Reference
findRef (l, c) sg = listToMaybe (sortOn rank (matchingRefs ++ matchingDecls))
 where
  rank r =
    let sp = Scope.refSpan r
        s = Scope.spanStart sp
        e = Scope.spanEnd sp
     in ( Scope.posLine e - Scope.posLine s
        , Scope.posCol e - Scope.posCol s
        , kindRank (Scope.refKind r)
        )
  kindRank Scope.VarRef = 0 :: Int
  kindRank _ = 1
  matchingRefs =
    [ r
    | s <- Map.elems (Scope.sgScopes sg)
    , r <- Scope.scopeReferences s
    , spanContains (l, c) (Scope.refSpan r)
    ]
  matchingDecls =
    [ Scope.Reference (Scope.declName d) (Scope.declSpan d) (Scope.declScope d) Scope.VarRef
    | s <- Map.elems (Scope.sgScopes sg)
    , d <- Scope.scopeDeclarations s
    , spanContains (l, c) (Scope.declSpan d)
    ]

spanContains :: (Int, Int) -> Scope.SourceSpan -> Bool
spanContains (cl, cc) sp =
  let s = Scope.spanStart sp
      e = Scope.spanEnd sp
      sl = Scope.posLine s
      sc = Scope.posCol s
      el = Scope.posLine e
      ec = Scope.posCol e
   in cl >= sl && cl <= el && (cl /= sl || cc >= sc) && (cl /= el || cc <= ec)

-- | Pure: convert a 1-based scope-graph 'Scope.SourcePos' to a 0-based LSP 'Position'.
toLspPos :: Scope.SourcePos -> Position
toLspPos sp = Position (fromIntegral (Scope.posLine sp - 1)) (fromIntegral (Scope.posCol sp - 1))

-- ═══════════════════════ completion ═══════════════════════

{- | Pure: completion items at the cursor for an expression — the names IN
  SCOPE at the cursor (lexical, from the AST path containing it), builtins,
  and module-system options, all filtered by the identifier prefix the
  cursor sits on (read from the buffer text, 0-based LSP position). Inside
  a dotted chain (@foo.ba@) scope\/builtin completion stays out of the way —
  that is attribute territory (the nixpkgs path handles @pkgs.…@).
-}
completionsForExpr :: TypeEnv -> Text -> NExprLoc -> Int -> Int -> [CompletionItem]
completionsForExpr _env txt expr l c =
  maybe [] withPfx (prefixAtCursor txt l c)
 where
  withPfx pfx =
    let scoped = scopeCompletions expr pfx l c
        builtins' = builtinCompletions pfx
        opts = MS.extractOptions expr
        matchingOpts = Map.filterWithKey (\k _ -> pfx `T.isPrefixOf` k) opts
        optItems =
          [ mkCompletionItem
              k
              (Just CompletionItemKind_Property)
              (Just (NT.prettyType (MS.optType v)))
          | (k, v) <- Map.toList matchingOpts
          ]
     in nub $ scoped ++ builtins' ++ optItems

{- | The identifier prefix the cursor is completing, from the buffer text:
the trailing identifier run before the cursor. 'Nothing' inside a dotted
chain (attribute completion — not name completion). An empty run (cursor
after whitespace) completes everything.
-}
prefixAtCursor :: Text -> Int -> Int -> Maybe Text
prefixAtCursor txt l c = do
  line <- safeIx l (T.lines txt)
  ctxOf (chainBeforeCursor (T.take c line))
 where
  ctxOf ([], partial) = Just partial
  ctxOf _ = Nothing

{- | THE PANOPTICON COMPLETION: the cursor sits after a dotted chain
(@server.@, @cfg.@, @builtins.@, @dep.@ where @dep = import ./dep.nix@) and
the members come from the chain's INFERRED TYPE — the checker answering
"what fields does this thing have", which is exactly what a type checker is
for. Resolution: the chain root among the (partial) bindings first — local
lets, cfg spines from module declarations, closure-typed imports — then the
env's monotypes (the @builtins@ record); @lib.@ serves its scheme tables;
@pkgs.@ stands down (the eval backend owns it). Remaining segments walk
record fields; each item carries its field's pretty type.
-}
memberCompletions ::
  TypeEnv -> [Infer.Binding] -> Text -> Int -> Int -> [CompletionItem]
memberCompletions env binds txt l c = fromMaybe [] $ do
  line <- safeIx l (T.lines txt)
  chainItems (chainBeforeCursor (T.take c line))
 where
  chainItems ([], _) = Nothing
  chainItems ("pkgs" : _, _) = Nothing
  chainItems (["lib"], pfx) = Just (libMemberItems pfx)
  chainItems (root : rest, pfx) = do
    t0 <- rootType root
    t <- foldM stepField t0 rest
    Just (fieldItems pfx t)
  rootType n =
    (Infer.bindType <$> find ((== n) . Infer.bindName) binds)
      <|> (schemeBody <$> Map.lookup n (envBindings env))
  schemeBody (NT.Forall _ t) = t
  stepField t k = case t of -- CASE-OK: shape dispatch
    NT.TRec fields _ -> fst <$> Map.lookup k fields
    NT.TUnion ts -> listToMaybe (mapMaybe (`stepField` k) ts)
    _ -> Nothing
  fieldItems pfx t = case t of -- CASE-OK: shape dispatch
    NT.TRec fields _ ->
      [ mkCompletionItem k (Just CompletionItemKind_Field) (Just (NT.prettyType ft))
      | (k, (ft, _)) <- Map.toList fields
      , pfx `T.isPrefixOf` k
      ]
    NT.TUnion ts -> nub (concatMap (fieldItems pfx) ts)
    NT.TDerivation ->
      [ mkCompletionItem k (Just CompletionItemKind_Field) (Just "derivation attribute")
      | k <- derivationTemplateAttrs
      , pfx `T.isPrefixOf` k
      ]
    _ -> []
  libMemberItems pfx =
    [ mkCompletionItem k (Just CompletionItemKind_Function) (Just (NT.prettyScheme s))
    | (k, s) <- Map.toList (Map.union libSchemeTable builtinSchemeTable)
    , pfx `T.isPrefixOf` k
    ]

-- | the attrs every mkDerivation output carries (the eval backend's tier-1 floor)
derivationTemplateAttrs :: [Text]
derivationTemplateAttrs =
  [ "drvPath"
  , "meta"
  , "name"
  , "out"
  , "outPath"
  , "outputs"
  , "override"
  , "overrideAttrs"
  , "passthru"
  , "pname"
  , "src"
  , "system"
  , "version"
  ]

-- | the names lexically in scope at the cursor, as completion items
scopeCompletions :: NExprLoc -> Text -> Int -> Int -> [CompletionItem]
scopeCompletions expr pfx l c =
  [ mkCompletionItem n (Just CompletionItemKind_Variable) (Just "local binding")
  | n <- nub (namesInScopeAt (l + 1) (c + 1) expr)
  , pfx `T.isPrefixOf` n
  ]

{- | The names lexically visible at a 1-based cursor position: walk the AST
spine containing the cursor, collecting the binders each enclosing node
introduces (let names, lambda formals + \@-binding, rec-attrset names).
Subtrees not containing the cursor contribute nothing — an inner lambda's
formal is not in scope outside it.
-}
namesInScopeAt :: Int -> Int -> NExprLoc -> [Text]
namesInScopeAt l c = go
 where
  go node@(LayerAnn srcSpan e)
    | not (containsCursor (srcSpanToSpan srcSpan)) = []
    | otherwise = bindersOf e ++ concatMap go (childExprs (unwrap node))
  unwrap (LayerAnn _ e) = e
  containsCursor sp =
    let s = spanStart sp
        en = spanEnd sp
     in (l, c) >= (locLine s, locCol s) && (l, c) <= (locLine en, locCol en)
  bindersOf (NLet bindings _) = concatMap bindingNames bindings
  bindersOf (NSet Recursive bindings) = concatMap bindingNames bindings
  bindersOf (NAbs params _) = paramNames params
  bindersOf _ = []
  bindingNames (NamedVar (StaticKey k :| []) _ _) = [varNameText k]
  bindingNames (Inherit _ keys _) = map varNameText keys
  bindingNames _ = []
  paramNames (Param n) = [varNameText n]
  paramNames (ParamSet mName _ pairs) =
    maybe [] (pure . varNameText) mName ++ map (varNameText . fst) pairs

builtinCompletions :: Text -> [CompletionItem]
builtinCompletions pfx =
  let names = Map.keys (envBindings builtinEnv)
      matching = filter (pfx `T.isPrefixOf`) names
   in map builtinItem matching

builtinItem :: Text -> CompletionItem
builtinItem name =
  let detail = fmap NT.prettyScheme (Map.lookup name (envBindings builtinEnv))
      kind =
        if "builtins" `T.isPrefixOf` name
          then CompletionItemKind_Module
          else CompletionItemKind_Function
   in mkCompletionItem name (Just kind) detail

mkCompletionItem :: Text -> Maybe CompletionItemKind -> Maybe Text -> CompletionItem
mkCompletionItem label' kind' detail' =
  CompletionItem
    { _label = label'
    , _labelDetails = Nothing
    , _kind = kind'
    , _tags = Nothing
    , _detail = detail'
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

{- | What a @pkgs.…@ completion at the cursor is completing: a package NAME
(@pkgs.<prefix>@) or a SYMBOL at an attribute PATH (@pkgs.<a>.<b>.<prefix>@, the
path being @[a,b]@ to any depth). The symbol case is resolved through the eval
backend (shape template / spine-force / compiler) by the handler — this layer
stays pure.
-}
data PkgsCtx
  = PkgName !Text
  | PkgSymbol ![Text] !Text
  deriving (Eq, Show)

{- | Recognize a @pkgs.…@ completion context from the buffer text + cursor. A
backward scan over the dotted chain on the current line, so it survives the
half-typed source the parser rejects (a bare @pkgs.@ / @pkgs.hello.@) and is
independent of the parsed AST. 'Nothing' if the cursor isn't completing under
@pkgs@.
-}
nixpkgsCompletionContext :: Text -> Int -> Int -> Maybe PkgsCtx
nixpkgsCompletionContext txt l c =
  safeIx l (T.lines txt) >>= ctxOf . chainBeforeCursor . T.take c
 where
  ctxOf ("pkgs" : rest, prefix)
    | null rest = Just (PkgName prefix)
    | otherwise = Just (PkgSymbol rest prefix)
  ctxOf _ = Nothing

-- | Package-name completions: index keys matching the prefix, capped.
pkgNameCompletions :: Nixpkgs.NixpkgsIndex -> Text -> [CompletionItem]
pkgNameCompletions idx prefix =
  take
    maxNixpkgsItems
    [ mkCompletionItem name (Just CompletionItemKind_Module) (Just "nixpkgs package")
    | name <- Map.keys (Nixpkgs.pkgsByName idx)
    , prefix `T.isPrefixOf` name
    ]

{- | Attribute completions from a list of attr names matching the prefix —
the symbol case, given names the eval backend produced.
-}
attrCompletions :: Text -> [Text] -> Text -> [CompletionItem]
attrCompletions detail names prefix =
  take
    maxNixpkgsItems
    [ mkCompletionItem name (Just CompletionItemKind_Field) (Just detail)
    | name <- names
    , prefix `T.isPrefixOf` name
    ]

maxNixpkgsItems :: Int
maxNixpkgsItems = 1000

-- | Safe list index: 'Nothing' for negative or out-of-range @i@.
safeIx :: Int -> [a] -> Maybe a
safeIx i xs
  | i < 0 = Nothing
  | otherwise = listToMaybe (drop i xs)

{- | The dotted chain and trailing partial just before the cursor: e.g.
@"… = pkgs.hello.over"@ → @(["pkgs","hello"], "over")@, @"pkgs.rip"@ →
@(["pkgs"], "rip")@, @"pkgs."@ → @(["pkgs"], "")@. Backward scan over identifier
runs separated by dots; stops at the first non-identifier, non-dot character.
-}
chainBeforeCursor :: Text -> ([Text], Text)
chainBeforeCursor before =
  let (firstRev, rest0) = T.span isPkgChar (T.reverse before)
   in (go [] rest0, T.reverse firstRev)
 where
  go acc rest = step acc (T.uncons rest)
  step acc (Just ('.', more)) =
    let (segRev, rest') = T.span isPkgChar more
     in go (T.reverse segRev : acc) rest'
  step acc _ = acc

-- | Characters that may appear in a Nix attribute / package name.
isPkgChar :: Char -> Bool
isPkgChar ch = isAlphaNum ch || ch == '_' || ch == '\'' || ch == '-'

-- ═══════════════════════ signature help ═══════════════════════

{- | Pure: signature help for the call enclosing the cursor — resolves the
  applied function name and renders its builtin type scheme as parameters.
-}
signatureAtCursor :: TypeEnv -> NExprLoc -> Int -> Int -> Maybe SignatureHelp
signatureAtCursor _env expr l c = do
  target <- findExprAt l c expr
  (funcExpr, _) <- findEnclosingCall expr target
  name <- exprName funcExpr
  lookupBuiltinSig name

findEnclosingCall :: NExprLoc -> NExprLoc -> Maybe (NExprLoc, [NExprLoc])
findEnclosingCall root target = go root
 where
  go (Layer (NApp func arg))
    | arg == target = Just (func, [arg])
    | otherwise = go func <|> go arg <|> deepSearch
   where
    deepSearch = maybe (fmap addArg (go arg)) (Just . addArg) (go func)
     where
      addArg (f, as) = (f, arg : as)
  go (Layer e) = checkChildren (childExprs e)
  checkChildren [] = Nothing
  checkChildren (x : xs) = go x <|> checkChildren xs

lookupBuiltinSig :: Text -> Maybe SignatureHelp
lookupBuiltinSig name = do
  scheme <- Map.lookup name (envBindings builtinEnv)
  let typeStr = NT.prettyScheme scheme
      params = extractParamLabels scheme
      paramInfos =
        [ ParameterInformation (InL p) Nothing
        | (p, i) <- zip params [(0 :: Int) ..]
        , i < (5 :: Int)
        ]
      sigInfo =
        SignatureInformation
          (name <> " : " <> typeStr)
          Nothing
          (if null paramInfos then Nothing else Just paramInfos)
          Nothing
  pure $ SignatureHelp [sigInfo] (Just (0 :: UInt)) (Just (InL (0 :: UInt)))
 where
  extractParamLabels (NT.Forall _ t) = collect t
  collect (NT.TFun a b) = NT.prettyType a : collect b
  collect _ = []

-- ═══════════════════════ code actions ═══════════════════════

{- | Pure: does the given range overlap the start of the diagnostic's range?
  Used to find the diagnostics a code-action request applies to.
-}
rangeOverlapsDiag :: Range -> Diagnostic -> Bool
rangeOverlapsDiag range (Diagnostic r _ _ _ _ _ _ _ _) =
  let Range (Position rl rc) (Position rel rec) = range
      Range (Position dl dc) _ = r
   in (rl < dl || (rl == dl && rc <= dc)) && (rel > dl || (rel == dl && rec >= dc))

{- | Pure: quick-fix code actions for a diagnostic, keyed off the rule codes
the diagnostics layer ACTUALLY emits. A non-lisp-case finding carries a
complete rename: the declaration plus every reference, computed from the
scope graph — apply-and-done, not a suggestion.
-}
violationAction :: Uri -> Maybe Scope.ScopeGraph -> Diagnostic -> [CodeAction]
violationAction = violationActionIn Nothing

{- | 'violationAction' with the BUFFER TEXT: the fix-carrying tier. The
checker's diagnostics know their repairs — a did-you-mean replaces the
typo'd word, `+`-on-lists becomes `++`, `optionalString` fed a list becomes
`optionals`, an unused binding deletes its line. Every textual edit is
guarded on a single unambiguous occurrence in the diagnostic's line.
-}
violationActionIn :: Maybe Text -> Uri -> Maybe Scope.ScopeGraph -> Diagnostic -> [CodeAction]
violationActionIn mTxt uri mSg diag
  | Just sugg <- didYouMeanOf m
  , Just wrong <- wrongNameOf m =
      wordFix wrong sugg ("Rename to `" <> sugg <> "`")
  | "operator `+` cannot combine [" `T.isInfixOf` m =
      lineFix " + " " ++ " "Change `+` to `++`"
  | "expected String, got [" `T.isInfixOf` m =
      lineFix "optionalString" "optionals" "Use `optionals` (list passthrough)"
  | "unused-binding:" `T.isInfixOf` m = deleteLineFix
  | otherwise = violationKeyed uri mSg diag
 where
  m = diagMsg diag
  Diagnostic{_range = Range (Position dl _) _} = diag
  diagLine = mTxt >>= \t -> listToMaybe (drop (fromIntegral dl) (T.lines t))
  didYouMeanOf t = do
    rest <- afterText "did you mean '" t
    let s = T.takeWhile (/= '\'') rest
    if T.null s then Nothing else Just s
  wrongNameOf t =
    listToMaybe
      ( mapMaybe
          id
          [ do
              rest <- afterText "attribute '" t
              let w = T.takeWhile (/= '\'') rest
              if T.null w then Nothing else Just w
          , do
              rest <- afterText "unbound variable: " t
              let w = T.takeWhile (`notElem` ("; " :: String)) rest
              if T.null w then Nothing else Just w
          ]
      )
  afterText pre t =
    let (_, b) = T.breakOn pre t
     in if T.null b then Nothing else Just (T.drop (T.length pre) b)
  wordFix needle replacement title = lineFix needle replacement title
  lineFix needle replacement title = maybe [] pure $ do
    ln <- diagLine
    if T.count needle ln == 1
      then do
        let (pre, _) = T.breakOn needle ln
            col = fromIntegral (T.length pre)
        Just
          ( editAction
              title
              ( singleEdit
                  (Range (Position dl col) (Position dl (col + fromIntegral (T.length needle))))
                  replacement
              )
              diag
          )
      else Nothing
  deleteLineFix = maybe [] pure $ do
    ln <- diagLine
    if ";" `T.isSuffixOf` T.stripEnd ln && "=" `T.isInfixOf` ln
      then
        Just
          ( editAction
              "Delete unused binding"
              (singleEdit (Range (Position dl 0) (Position (dl + 1) 0)) "")
              diag
          )
      else Nothing
  singleEdit range newText =
    WorkspaceEdit
      { _changes = Just (Map.singleton uri [TextEdit range newText])
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }

-- | the rule-code-keyed actions (titles, plus the lisp-case rename edit)
violationKeyed :: Uri -> Maybe Scope.ScopeGraph -> Diagnostic -> [CodeAction]
violationKeyed uri mSg diag
  | "NARSIL-N001" `T.isInfixOf` msg = [simpleAction "Replace `with` by explicit bindings" True diag]
  | "NARSIL-N015" `T.isInfixOf` msg = renameAction
  | "NARSIL-N011" `T.isInfixOf` msg = [simpleAction "Use writeShellApplication instead" True diag]
  | "MISSING-META" `T.isInfixOf` msg = [simpleAction "Insert `meta` attribute" True diag]
  | "MISSING-DESCRIPTION" `T.isInfixOf` msg = [simpleAction "Add description to meta" True diag]
  | "OR-NULL-FALLBACK" `T.isInfixOf` msg =
      [simpleAction "Replace `or null` by if-then-else" False diag]
  | otherwise = []
 where
  msg = T.toUpper (diagMsg diag)
  -- "NARSIL-N015 (`myThing`): …" — the offending name travels in the message
  renameAction = maybe [] pure $ do
    sg <- mSg
    name <- nameInBackticks (diagMsg diag)
    let newName = NixLint.suggestLispCase name
        Diagnostic{_range = Range (Position dl dc) _} = diag
    ref <- findRef (fromIntegral dl + 1, fromIntegral dc + 1) sg
    decl <- either (const Nothing) Just (Scope.resolve sg ref)
    let spans = Scope.declSpan decl : map Scope.refSpan (Scope.findReferences sg decl)
        edits =
          [ TextEdit (Range (toLspPos (Scope.spanStart sp)) (toLspPos (Scope.spanEnd sp))) newName
          | sp <- spans
          ]
    let wsEdit =
          WorkspaceEdit
            { _changes = Just (Map.singleton uri edits)
            , _documentChanges = Nothing
            , _changeAnnotations = Nothing
            }
    pure (editAction ("Rename to `" <> newName <> "`") wsEdit diag)

-- | the first backtick-quoted name in a diagnostic message, if any
nameInBackticks :: Text -> Maybe Text
nameInBackticks m =
  let after = T.drop 1 (T.dropWhile (/= '`') m)
      name = T.takeWhile (/= '`') after
   in if T.null after || T.null name then Nothing else Just name

-- | a quick-fix carrying a ready-to-apply 'WorkspaceEdit'
editAction :: Text -> WorkspaceEdit -> Diagnostic -> CodeAction
editAction title wsEdit diag =
  CodeAction
    { _title = title
    , _kind = Just CodeActionKind_QuickFix
    , _diagnostics = Just [diag]
    , _isPreferred = Just True
    , _disabled = Nothing
    , _edit = Just wsEdit
    , _command = Nothing
    , _data_ = Nothing
    }

simpleAction :: Text -> Bool -> Diagnostic -> CodeAction
simpleAction title preferred diag =
  CodeAction
    { _title = title
    , _kind = Just CodeActionKind_QuickFix
    , _diagnostics = Just [diag]
    , _isPreferred = Just preferred
    , _disabled = Nothing
    , _edit = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

diagMsg :: Diagnostic -> Text
diagMsg (Diagnostic _ _ _ _ _ msg _ _ _) = msg

-- ═══════════════════════ inlay hints ═══════════════════════

{- | Pure: inferred-type inlay hints for the let/attr bindings within @range@,
  placed after each binding name. Uses PARTIAL inference results: a type
  error in one binding keeps the hints for everything typed before it —
  one bad line must not blank the file.
-}
inlayHintsForExpr :: TypeEnv -> NExprLoc -> Range -> [InlayHint]
inlayHintsForExpr env expr range = withBindings (Infer.inferExprBindingsPartial env expr)
 where
  withBindings bindings =
    [ InlayHint
        ( Position
            (fromIntegral (locLine (spanStart sp) - 1))
            (fromIntegral (locCol (spanEnd sp) + 1))
        )
        (InL (": " <> NT.prettyType bindType))
        (Just InlayHintKind_Type)
        Nothing
        Nothing
        (Just True)
        Nothing
        Nothing
    | Infer.Binding name bindType sp <- bindings
    , not (T.null name)
    , cursorInRange
        ( Position
            (fromIntegral (locLine (spanEnd sp) - 1))
            (fromIntegral (locCol (spanEnd sp) + 1))
        )
        range
    ]

cursorInRange :: Position -> Range -> Bool
cursorInRange (Position l c) (Range (Position rl rc) (Position rel rec)) =
  l >= rl && l <= rel && (l /= rl || c >= rc) && (l /= rel || c <= rec)

-- ═══════════════════════ option lookup + hover fallbacks ═══════════════════════

{- | Pure: the module-system 'MS.OptionInfo' for the option named at the cursor,
  if the cursor sits on a name declared via @options@ in the expression.
-}
inferOptionAtPath :: TypeEnv -> NExprLoc -> Int -> Int -> Maybe MS.OptionInfo
inferOptionAtPath _env expr l c = do
  target <- findExprAt l c expr
  let name = exprName target; opts = MS.extractOptions expr
  name >>= (`Map.lookup` opts)

-- | Hover-fallback markup shown when no file is open at the requested URI.
noFile :: MarkupContent
noFile = MarkupContent MarkupKind_Markdown "`no file`"

-- | Hover-fallback markup shown when the open file fails to parse.
parseErr :: MarkupContent
parseErr = MarkupContent MarkupKind_Markdown "`parse error`"
