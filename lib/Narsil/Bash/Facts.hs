{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                  // bash // facts
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "As she walked from the Louvre, she seemed to sense some articulated
--    structure shifting to accommodate her course through the city. The
--    waiter would be merely a part of the thing, one limb, a delicate probe
--    or palp. The whole would be larger, much larger. How could she have
--    imagined that it would be possible to live, to move, in the unnatural
--    field of Virek's wealth without suffering distortion?"
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                           // ast // walk // facts
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Facts (
  extractFacts,
)
where

import Control.Monad.Reader (Reader, runReader)
import Data.Foldable (toList)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Narsil.Bash.Facts.Token (
  extractLiteral,
  extractParamExpansion,
  isQuotedToken,
  mkSpan,
  tokenToText,
 )
import Narsil.Bash.Facts.Value (
  configValueFact,
  extractVarRef,
  parseConfigTemplate,
  parseConfigValueDynamic,
  selectValueParser,
 )
import Narsil.Bash.Parse (BashAST (..))
import Narsil.Bash.Patterns
import Narsil.Bash.Types
import Narsil.Core.Span (Span)
import ShellCheck.AST qualified as SA
import ShellCheck.Interface (Position)

-- ── entry point: walk entire AST collecting facts ─────────────────

-- | walk a bash AST bottom-up, extracting facts at every token
extractFacts :: BashAST -> [Fact]
extractFacts (BashAST root posMap) = runReader (traverseTokens root) posMap

-- | recurse into token children, collecting facts at each node
traverseTokens :: SA.Token -> Reader (Map SA.Id (Position, Position)) [Fact]
traverseTokens (SA.OuterToken shellCheckId innerToken) = do
  local <- factFromInnerToken shellCheckId innerToken
  rest <- mapM traverseTokens (toList innerToken)
  pure (local ++ concat rest)

-- ── inner-token dispatch ─────────────────────────────────────────

-- | dispatch based on ShellCheck inner token type
factFromInnerToken ::
  SA.Id -> SA.InnerToken SA.Token -> Reader (Map SA.Id (Position, Position)) [Fact]
factFromInnerToken shellCheckId innerToken = do
  sourceSpan <- mkSpan shellCheckId
  dispatch sourceSpan innerToken
 where
  dispatch sourceSpan (SA.Inner_T_Assignment _ name indices value) =
    pure $ factFromAssignment sourceSpan (assignmentLhs name indices) value
  dispatch sourceSpan (SA.Inner_T_SimpleCommand assigns commandWords) =
    factFromCommand sourceSpan assigns commandWords
  dispatch sourceSpan (SA.Inner_T_Pipeline _ _) = factFromPipeline sourceSpan
  dispatch sourceSpan (SA.Inner_T_Subshell _) = factFromSubshell sourceSpan
  dispatch sourceSpan (SA.Inner_T_Redirecting _ _) = factFromRedirect sourceSpan
  dispatch sourceSpan (SA.Inner_T_IoFile _ _) = factFromRedirect sourceSpan
  dispatch sourceSpan (SA.Inner_T_FdRedirect _ _) = factFromRedirect sourceSpan
  dispatch _ _ = pure []

-- ── assignment facts ─────────────────────────────────────────────

-- | facts from a single variable assignment (config.* or regular env var)

{- | Reconstruct the assignment LHS. ShellCheck keeps an array subscript in a
separate indices field, so @config[server]=…@ arrives as name=@config@,
indices=@[server]@. We rebuild @config[server]@ so it routes to the config-array
path (only for the @config@ namespace — ordinary bash arrays are left as the bare
name, preserving prior behavior). (REVIEW-3 #24)
-}
assignmentLhs :: String -> [SA.Token] -> Text
assignmentLhs name indices
  | name == "config"
  , not (null indices) =
      T.pack name <> "[" <> T.intercalate "." (map tokenToText indices) <> "]"
  | otherwise = T.pack name

factFromAssignment :: Span -> Text -> SA.Token -> [Fact]
factFromAssignment sourceSpan variableName valueToken =
  maybe
    (envVarFacts sourceSpan variableName valueToken)
    (\configPath -> configArrayFacts sourceSpan configPath valueToken)
    (parseConfigArrayAssign variableName)

-- ── command facts ────────────────────────────────────────────────

-- | facts from a simple command (pre-command assigns are ignored)
factFromCommand ::
  Span -> [SA.Token] -> [SA.Token] -> Reader (Map SA.Id (Position, Position)) [Fact]
factFromCommand sourceSpan _assigns = commandFacts sourceSpan

-- | placeholder: pipeline facts (children are traversed separately)
factFromPipeline :: Span -> Reader (Map SA.Id (Position, Position)) [Fact]
factFromPipeline _ = pure []

-- | placeholder: subshell facts (children are traversed separately)
factFromSubshell :: Span -> Reader (Map SA.Id (Position, Position)) [Fact]
factFromSubshell _ = pure []

-- | placeholder: redirect facts (children are traversed separately)
factFromRedirect :: Span -> Reader (Map SA.Id (Position, Position)) [Fact]
factFromRedirect _ = pure []

-- ── command body dispatch ────────────────────────────────────────

-- | inspect command tokens: config.* commands vs regular command invocations
commandFacts :: Span -> [SA.Token] -> Reader (Map SA.Id (Position, Position)) [Fact]
commandFacts _ [] = pure []
commandFacts sourceSpan (commandToken : arguments) =
  let commandText = tokenToText commandToken
   in if "config." `T.isPrefixOf` commandText
        then pure $ configFactsFromToken sourceSpan commandToken
        else commandInvocationFacts sourceSpan commandText arguments

-- | collect invocation facts: store path usage + argument flag facts
commandInvocationFacts ::
  Span -> Text -> [SA.Token] -> Reader (Map SA.Id (Position, Position)) [Fact]
commandInvocationFacts sourceSpan command arguments = do
  let pathFact = factFromStorePath sourceSpan command
  let commandName = resolveCommandName command
  argumentFacts <- extractArgFacts commandName arguments
  pure (pathFact ++ argumentFacts)

-- ── store path vs bare command classification ────────────────────

-- | classify a command text: store path, dynamic var, bare command, or ignored
factFromStorePath :: Span -> Text -> [Fact]
factFromStorePath sourceSpan command
  | T.null command = []
  | isStorePath command = [UsesStorePath (StorePath command) sourceSpan]
  | Just variable <- extractVarRef command = [DynamicCommand variable sourceSpan]
  | "@__nix_compile_interp_" `T.isPrefixOf` command = [BareCommand command sourceSpan]
  | "@" `T.isPrefixOf` command = []
  | isIgnoredCommand command = []
  | otherwise = [BareCommand command sourceSpan]

-- | extract short command name from a store path (e.g. /nix/store/xxx-curl/bin/curl -> curl)
resolveCommandName :: Text -> Text
resolveCommandName path
  | isStorePath path = lastSegment (reverse (T.splitOn "/" path))
  | otherwise = path
 where
  lastSegment (name : _) | not (T.null name) = name
  lastSegment _ = path

-- ── argument flag extraction (--flag=$VAR, --flag $VAR) ───────────

{- | scan command arguments for variable references in flags
handles both --flag=$VAR (same token) and --flag $VAR (adjacent tokens)
-}
extractArgFacts :: Text -> [SA.Token] -> Reader (Map SA.Id (Position, Position)) [Fact]
extractArgFacts command = loop
 where
  loop [] = pure []
  loop (token : remainingTokens) =
    maybe afterFlag (emitWith remainingTokens) (factFromFlagArgument command token)
   where
    -- both the same-token (--flag=$VAR) and adjacent-token (--flag $VAR) emits
    -- carry the FLAG token's span; only the tail to recurse on differs.
    emitWith rest getFact = do
      sourceSpan <- mkSpan (tokenId token)
      restFacts <- loop rest
      pure (getFact sourceSpan : restFacts)
    afterFlag = pairCase remainingTokens
    pairCase (valueToken : restAfterValue) =
      maybe
        (loop remainingTokens)
        (emitWith restAfterValue)
        (factFromFlagValuePair command token valueToken)
    pairCase [] = pure []

  tokenId (SA.OuterToken tokenId' _) = tokenId'

{- | detect --flag=$VAR within a single token
returns a (Span -> Fact) thunk since the caller owns the span
-}
factFromFlagArgument :: Text -> SA.Token -> Maybe (Span -> Fact)
factFromFlagArgument command token =
  let text = tokenToText token
      (flag, eqRest) = T.breakOn "=" text
   in if isFlag flag && not (T.null eqRest)
        then fmap (CmdArg command flag) (extractVarRef (T.drop 1 eqRest))
        else Nothing
 where
  isFlag f = "-" `T.isPrefixOf` f

-- | detect --flag $VAR across two adjacent tokens
factFromFlagValuePair :: Text -> SA.Token -> SA.Token -> Maybe (Span -> Fact)
factFromFlagValuePair command flagToken valueToken
  | isFlag flagText
  , Just variableName <- extractVarRef valueText =
      Just (CmdArg command flagText variableName)
  | otherwise = Nothing
 where
  flagText = tokenToText flagToken
  valueText = tokenToText valueToken
  isFlag f = "-" `T.isPrefixOf` f

-- ── config[path.to.key] syntax ───────────────────────────────────

-- | parse config[path.to.key] assignment name → ConfigPath
parseConfigArrayAssign :: Text -> Maybe ConfigPath
parseConfigArrayAssign name
  | "config[" `T.isPrefixOf` name && "]" `T.isSuffixOf` name =
      let pathText = T.dropEnd 1 (T.drop 7 name)
          parts = T.splitOn "." pathText
       in if validConfigPath parts then Just parts else Nothing
  | otherwise = Nothing

-- ── config[...] = value facts ────────────────────────────────────

-- | extract facts from a config[...]=value assignment
configArrayFacts :: Span -> ConfigPath -> SA.Token -> [Fact]
configArrayFacts sourceSpan configPath valueToken =
  maybe noVar withVar (extractVarRef valueText)
 where
  valueText = tokenToText valueToken
  quoted = isQuotedToken valueToken
  withVar variable = [ConfigAssign configPath variable quoted sourceSpan]
  litFact = [ConfigLit configPath (parseLiteral valueText) sourceSpan]
  noVar
    | "${" `T.isInfixOf` valueText =
        maybe
          litFact
          (\parts -> [ConfigTemplate configPath parts quoted sourceSpan])
          (parseConfigTemplate valueText)
    | otherwise = litFact

-- ── env var facts: ${var:-default}, ${var:=default}, ${var:?err} ──

-- | extract facts from a regular (non-config) shell variable assignment
envVarFacts :: Span -> Text -> SA.Token -> [Fact]
envVarFacts sourceSpan variableName valueToken =
  maybe fromLiteral fromExpansion (extractParamExpansion valueToken)
 where
  fromExpansion (DefaultValue _var (Just defaultValue)) = defaultFacts defaultValue
  fromExpansion (AssignDefault _var (Just defaultValue)) = defaultFacts defaultValue
  fromExpansion (AssignDefault _var Nothing) = [DefaultIs variableName (LitString "") sourceSpan]
  fromExpansion (DefaultValue _var Nothing) = [DefaultIs variableName (LitString "") sourceSpan]
  fromExpansion (ErrorIfUnset _var _) = [Required variableName sourceSpan]
  fromExpansion (SimpleRef variable) = [AssignFrom variableName variable sourceSpan]
  fromExpansion (UseAlternate _var _) = []

  fromLiteral =
    maybe [] (\lit -> [AssignLit variableName lit sourceSpan]) (extractLiteral valueToken)

  defaultFacts defaultValue =
    maybe
      [DefaultIs variableName (parseLiteral defaultValue) sourceSpan]
      (\other -> [DefaultFrom variableName other sourceSpan])
      (defaultFromVar defaultValue)

  -- if the default value is itself a variable reference, emit DefaultFrom
  defaultFromVar defaultValue
    | Just (SimpleRef variable) <- parseParamExpansion defaultValue = Just variable
    | otherwise = Nothing

-- ── config.* command facts ───────────────────────────────────────

-- | extract config assignment facts from a config.* command token
configFactsFromToken :: Span -> SA.Token -> [Fact]
configFactsFromToken sourceSpan (SA.OuterToken _ (SA.Inner_T_NormalWord parts)) =
  configFactsFromParts sourceSpan parts
configFactsFromToken sourceSpan token = configFacts sourceSpan (tokenToText token)

-- ── token-part-level config analysis ─────────────────────────────

{- | extract config assignment facts from NormalWord token parts
splits on =, validates path, then parses the value side
-}
configFactsFromParts :: Span -> [SA.Token] -> [Fact]
configFactsFromParts sourceSpan tokenParts = maybe [] fromPrefix matchedPrefix
 where
  combinedText = T.concat (map tokenToText tokenParts)
  (leftHandSide, rightHandSide) = T.breakOn "=" combinedText
  matchedPrefix = T.stripPrefix "config." leftHandSide

  fromPrefix pathText
    | T.null rightHandSide = []
    | not (validConfigPath pathParts) = []
    | otherwise = buildConfigFacts pathParts
   where
    pathParts = T.splitOn "." pathText

  buildConfigFacts parts =
    map (configValueFact parts quoted sourceSpan) (maybeToList parsed)
   where
    (valueTokens, quoted) = findValueTokens tokenParts
    parsed = selectValueParser valueTokens (T.drop 1 rightHandSide) quoted

-- ── value token extraction ───────────────────────────────────────

{- | scan token parts for the portion after = and determine quoting
n.b. we need to find = within literal tokens, then grab the next token
-}
findValueTokens :: [SA.Token] -> ([SA.Token], Quoted)
findValueTokens parts = loop parts False
 where
  loop [] _ = ([], Unquoted)
  loop (token@(SA.OuterToken _ innerToken) : remainingTokens) seenEquals
    | SA.Inner_T_Literal content <- innerToken
    , not seenEquals
    , "=" `T.isInfixOf` T.pack content =
        loop remainingTokens True
    | SA.Inner_T_DoubleQuoted _ <- innerToken, seenEquals = ([token], Quoted)
    | seenEquals = ([token], Unquoted)
    | otherwise = loop remainingTokens seenEquals

-- ── config.* text fallback parser ────────────────────────────────

{- | extract config facts from raw text (used when token-level parsing fails)
tries dynamic (var-containing) parsing first, then falls back to parseConfigAssignment
-}
configFacts :: Span -> Text -> [Fact]
configFacts sourceSpan text
  | facts@(_ : _) <- dynamicFallback = facts
  | otherwise = fallbackConfigFacts sourceSpan text
 where
  dynamicFallback
    | Just pathText <- T.stripPrefix "config." leftHandSide
    , Just rightHandSide <- T.stripPrefix "=" rightHandSide0
    , "$" `T.isInfixOf` rightHandSide
    , let pathParts = T.splitOn "." pathText
    , validConfigPath pathParts
    , Just parsed <- parseConfigValueDynamic rightHandSide Unquoted =
        [configValueFact pathParts Unquoted sourceSpan parsed]
    | otherwise = []
   where
    (leftHandSide, rightHandSide0) = T.breakOn "=" text

  fallbackConfigFacts sp text_ = maybe [] fromAssignment (parseConfigAssignment text_)
   where
    fromAssignment ConfigAssignment{..} =
      either
        (\variable -> [ConfigAssign configPath variable configQuoted sp])
        (\literal -> [ConfigLit configPath literal sp])
        configValue

-- ── shell builtin classification ─────────────────────────────────

-- | is this command a shell builtin (no store path needed)?
isIgnoredCommand :: Text -> Bool
isIgnoredCommand command = command `elem` shellBuiltins

-- | exhaustive list of POSIX + bash builtins
shellBuiltins :: [Text]
shellBuiltins =
  [ "if"
  , "then"
  , "else"
  , "elif"
  , "fi"
  , "case"
  , "esac"
  , "for"
  , "while"
  , "until"
  , "do"
  , "done"
  , "function"
  , "return"
  , "break"
  , "continue"
  , "set"
  , "unset"
  , "export"
  , "declare"
  , "local"
  , "readonly"
  , "typeset"
  , "let"
  , "source"
  , "."
  , "cd"
  , "pwd"
  , "pushd"
  , "popd"
  , "dirs"
  , "echo"
  , "printf"
  , "read"
  , "exit"
  , "exec"
  , "trap"
  , "wait"
  , "kill"
  , "true"
  , "false"
  , ":"
  , "test"
  , "["
  , "bg"
  , "fg"
  , "jobs"
  , "disown"
  , "builtin"
  , "command"
  , "type"
  , "hash"
  , "help"
  , "enable"
  , "shopt"
  , "bind"
  , "complete"
  , "compgen"
  , "getopts"
  , "shift"
  , "times"
  , "ulimit"
  , "umask"
  , "history"
  , "fc"
  ]
