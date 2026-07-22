{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                                  // lsp // server
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "'You're here, aren't you?' she called, adding to the ring of sound,
--    ripples and reflections of her fragmented voice.
--
--                                                                                  —Yes, I am here.
--
--    'Wigan would say you've always been here, wouldn't he?'
--
--                            —Yes, but it isn't true. I came to be, here. Once I was not. Once, for
--    a brilliant time, time without duration, I was everywhere as well... But
--    the bright time broke. The mirror was flawed. Now I am only one... But
--    I have my song, and you have heard it."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                        // lsp // protocol // wire
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.LSP.Server (run) where

import Control.Monad.IO.Class
import Data.Text ()
import Language.LSP.Server
import Narsil.LSP.Handlers (handlers)

-- | Run the LSP server over stdio, wiring up the 'handlers'; returns the exit code.
run :: IO Int
run =
  runServer $
    ServerDefinition
      { parseConfig = \_old _val -> Right ()
      , onConfigChange = const $ pure ()
      , doInitialize = \env _req -> pure (Right env)
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      , defaultConfig = ()
      , configSection = "narsil"
      }
