{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                          // nix // compile // log
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "Doors opened, closed behind him. Wheels left ferroconcrete, drinks
--    arrived, dinner was served."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                // core // logging
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Core.Log (
  AppM,
  runLog,
  logStr,
  module Katip,
)
where

import System.IO (stderr)

import Data.Text (Text)
import Data.Text.Lazy.Builder qualified as Builder
import Katip hiding (logStr)
import Katip qualified

-- | the application monad: katip structured logging over 'IO'.
type AppM = KatipContextT IO

-- | build a katip 'Katip.LogStr' from 'Text' (re-exposed since Katip's own is hidden here).
logStr :: Text -> Katip.LogStr
logStr = Katip.logStr

-- | run an 'AppM' action, logging items at or above the given severity to stderr.
runLog :: Severity -> AppM a -> IO a
runLog minSeverity action = do
  -- No severity text prefix: diagnostics already carry their own
  -- "error[CODE]:" / "warning[CODE]:" word (see Narsil.Core.Diagnostic), and
  -- ColorIfTerminal still colours each line by severity on a TTY. A "[ERROR]"
  -- prefix here only doubled up ("[ERROR] error[TYPE]: …"). Debug lines keep a
  -- marker since they have no inherent one and only appear under -vv.
  let prefixFor DebugS = "[debug] "
      prefixFor _ = ""
      fmt _color _verb item =
        let pfx = prefixFor (_itemSeverity item)
            msg = unLogStr (_itemMessage item)
         in Builder.fromText pfx <> msg
  handleScribe <- mkHandleScribeWithFormatter fmt ColorIfTerminal stderr (permitItem minSeverity) V0
  initLogEnv "narsil" "production"
    >>= registerScribe "stderr" handleScribe defaultScribeSettings
    >>= \le -> runKatipContextT le () "main" action
