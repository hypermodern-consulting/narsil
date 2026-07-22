{-# LANGUAGE OverloadedStrings #-}

-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                               // bash // builtins
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--
--   "He was like a kid who'd grown up beside an ocean, taking it as much
--    for granted as he took the sky, but knowing nothing of currents,
--    shipping routes, or the ins and outs of weather."
--
--                                                                                      — Count Zero
--
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
--                                                                           // commands // database
-- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

module Narsil.Bash.Builtins (
  -- * Lookup
  lookupArgType,
  lookupCommand,

  -- * Command schema
  CommandSchema (..),
  ArgSpec (..),

  -- * All builtins
  builtins,
)
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Narsil.Bash.Types (Type (..))

data ArgSpec = ArgSpec
  { argType :: !Type
  , argRequired :: !Bool
  , argDescription :: !Text
  }
  deriving (Eq, Show)

data CommandSchema = CommandSchema
  { cmdArgs :: !(Map Text ArgSpec)
  , cmdPositional :: ![ArgSpec]
  , cmdDescription :: !Text
  }
  deriving (Eq, Show)

lookupArgType :: Text -> Text -> Maybe Type
lookupArgType cmd flag = do
  schema <- Map.lookup cmd builtins
  spec <- Map.lookup flag (cmdArgs schema)
  Just (argType spec)

lookupCommand :: Text -> Maybe CommandSchema
lookupCommand = flip Map.lookup builtins

builtins :: Map Text CommandSchema
builtins =
  Map.fromList
    [
      ( "curl"
      , CommandSchema
          { cmdDescription = "HTTP client"
          , cmdPositional = [ArgSpec TString False "URL"]
          , cmdArgs =
              Map.fromList
                [ ("--connect-timeout", ArgSpec TInt False "Connection timeout in seconds")
                , ("--max-time", ArgSpec TInt False "Maximum time in seconds")
                , ("-m", ArgSpec TInt False "Maximum time in seconds")
                , ("--retry", ArgSpec TInt False "Number of retries")
                , ("--retry-delay", ArgSpec TInt False "Delay between retries")
                , ("--retry-max-time", ArgSpec TInt False "Max time for retries")
                , ("-o", ArgSpec TPath False "Output file")
                , ("--output", ArgSpec TPath False "Output file")
                , ("-H", ArgSpec TString False "Header")
                , ("--header", ArgSpec TString False "Header")
                , ("-d", ArgSpec TString False "POST data")
                , ("--data", ArgSpec TString False "POST data")
                , ("-u", ArgSpec TString False "User:password")
                , ("--user", ArgSpec TString False "User:password")
                , ("-X", ArgSpec TString False "HTTP method")
                , ("--request", ArgSpec TString False "HTTP method")
                ]
          }
      )
    ,
      ( "jq"
      , CommandSchema
          { cmdDescription = "JSON processor"
          , cmdPositional = [ArgSpec TString True "Filter expression"]
          , cmdArgs =
              Map.fromList
                [ ("--indent", ArgSpec TInt False "Indentation level")
                , ("-r", ArgSpec TBool False "Raw output")
                , ("--raw-output", ArgSpec TBool False "Raw output")
                , ("-e", ArgSpec TBool False "Exit status from output")
                , ("-s", ArgSpec TBool False "Slurp mode")
                , ("-c", ArgSpec TBool False "Compact output")
                ]
          }
      )
    ,
      ( "grep"
      , CommandSchema
          { cmdDescription = "Pattern search"
          , cmdPositional = [ArgSpec TString True "Pattern"]
          , cmdArgs =
              Map.fromList
                [ ("-m", ArgSpec TInt False "Max count")
                , ("--max-count", ArgSpec TInt False "Max count")
                , ("-A", ArgSpec TInt False "After context lines")
                , ("-B", ArgSpec TInt False "Before context lines")
                , ("-C", ArgSpec TInt False "Context lines")
                , ("-e", ArgSpec TString False "Pattern")
                , ("-f", ArgSpec TPath False "Pattern file")
                ]
          }
      )
    ,
      ( "sleep"
      , CommandSchema
          { cmdDescription = "Delay execution"
          , cmdPositional = [ArgSpec TInt True "Seconds"]
          , cmdArgs = Map.empty
          }
      )
    ,
      ( "timeout"
      , CommandSchema
          { cmdDescription = "Run with timeout"
          , cmdPositional = [ArgSpec TInt True "Duration"]
          , cmdArgs =
              Map.fromList
                [ ("-s", ArgSpec TString False "Signal")
                , ("--signal", ArgSpec TString False "Signal")
                , ("-k", ArgSpec TInt False "Kill after")
                ]
          }
      )
    ,
      ( "head"
      , CommandSchema
          { cmdDescription = "Show first lines"
          , cmdPositional = [ArgSpec TPath False "File"]
          , cmdArgs =
              Map.fromList
                [ ("-n", ArgSpec TInt False "Number of lines")
                , ("--lines", ArgSpec TInt False "Number of lines")
                , ("-c", ArgSpec TInt False "Number of bytes")
                , ("--bytes", ArgSpec TInt False "Number of bytes")
                ]
          }
      )
    ,
      ( "tail"
      , CommandSchema
          { cmdDescription = "Show last lines"
          , cmdPositional = [ArgSpec TPath False "File"]
          , cmdArgs =
              Map.fromList
                [ ("-n", ArgSpec TInt False "Number of lines")
                , ("--lines", ArgSpec TInt False "Number of lines")
                , ("-c", ArgSpec TInt False "Number of bytes")
                , ("--bytes", ArgSpec TInt False "Number of bytes")
                ]
          }
      )
    ,
      ( "split"
      , CommandSchema
          { cmdDescription = "Split files"
          , cmdPositional = [ArgSpec TPath False "Input file"]
          , cmdArgs =
              Map.fromList
                [ ("-n", ArgSpec TInt False "Number of chunks")
                , ("-l", ArgSpec TInt False "Lines per chunk")
                , ("--lines", ArgSpec TInt False "Lines per chunk")
                , ("-b", ArgSpec TString False "Bytes per chunk")
                , ("-a", ArgSpec TInt False "Suffix length")
                ]
          }
      )
    ,
      ( "dd"
      , CommandSchema
          { cmdDescription = "Convert and copy"
          , cmdPositional = []
          , cmdArgs =
              Map.fromList
                [ ("bs", ArgSpec TString False "Block size")
                , ("count", ArgSpec TInt False "Block count")
                , ("skip", ArgSpec TInt False "Skip blocks")
                , ("seek", ArgSpec TInt False "Seek blocks")
                , ("if", ArgSpec TPath False "Input file")
                , ("of", ArgSpec TPath False "Output file")
                ]
          }
      )
    ,
      ( "mkdir"
      , CommandSchema
          { cmdDescription = "Create directories"
          , cmdPositional = [ArgSpec TPath True "Directory"]
          , cmdArgs =
              Map.fromList
                [ ("-m", ArgSpec TString False "Mode")
                , ("--mode", ArgSpec TString False "Mode")
                ]
          }
      )
    ,
      ( "chmod"
      , CommandSchema
          { cmdDescription = "Change file mode"
          , cmdPositional =
              [ ArgSpec TString True "Mode"
              , ArgSpec TPath True "File"
              ]
          , cmdArgs = Map.empty
          }
      )
    ,
      ( "chown"
      , CommandSchema
          { cmdDescription = "Change file owner"
          , cmdPositional =
              [ ArgSpec TString True "Owner"
              , ArgSpec TPath True "File"
              ]
          , cmdArgs = Map.empty
          }
      )
    ,
      ( "xargs"
      , CommandSchema
          { cmdDescription = "Build command lines"
          , cmdPositional = [ArgSpec TString False "Command"]
          , cmdArgs =
              Map.fromList
                [ ("-n", ArgSpec TInt False "Max args")
                , ("--max-args", ArgSpec TInt False "Max args")
                , ("-P", ArgSpec TInt False "Max procs")
                , ("--max-procs", ArgSpec TInt False "Max procs")
                , ("-d", ArgSpec TString False "Delimiter")
                , ("--delimiter", ArgSpec TString False "Delimiter")
                ]
          }
      )
    ,
      ( "nc"
      , CommandSchema
          { cmdDescription = "Network utility"
          , cmdPositional =
              [ ArgSpec TString True "Host"
              , ArgSpec TInt True "Port"
              ]
          , cmdArgs =
              Map.fromList
                [ ("-w", ArgSpec TInt False "Timeout")
                , ("-p", ArgSpec TInt False "Source port")
                ]
          }
      )
    ,
      ( "wget"
      , CommandSchema
          { cmdDescription = "HTTP download"
          , cmdPositional = [ArgSpec TString True "URL"]
          , cmdArgs =
              Map.fromList
                [ ("-O", ArgSpec TPath False "Output file")
                , ("--output-document", ArgSpec TPath False "Output file")
                , ("-t", ArgSpec TInt False "Retries")
                , ("--tries", ArgSpec TInt False "Retries")
                , ("-T", ArgSpec TInt False "Timeout")
                , ("--timeout", ArgSpec TInt False "Timeout")
                , ("--wait", ArgSpec TInt False "Wait between retrievals")
                ]
          }
      )
    ,
      ( "rsync"
      , CommandSchema
          { cmdDescription = "Remote sync"
          , cmdPositional =
              [ ArgSpec TPath True "Source"
              , ArgSpec TPath True "Destination"
              ]
          , cmdArgs =
              Map.fromList
                [ ("--timeout", ArgSpec TInt False "IO timeout")
                , ("--port", ArgSpec TInt False "Port")
                , ("--bwlimit", ArgSpec TInt False "Bandwidth limit KB/s")
                , ("--max-size", ArgSpec TString False "Max file size")
                , ("--min-size", ArgSpec TString False "Min file size")
                ]
          }
      )
    ,
      ( "ssh"
      , CommandSchema
          { cmdDescription = "Secure shell"
          , cmdPositional = [ArgSpec TString True "Host"]
          , cmdArgs =
              Map.fromList
                [ ("-p", ArgSpec TInt False "Port")
                , ("-o", ArgSpec TString False "Option")
                , ("-i", ArgSpec TPath False "Identity file")
                , ("-l", ArgSpec TString False "Login name")
                , ("-F", ArgSpec TPath False "Config file")
                ]
          }
      )
    ,
      ( "scp"
      , CommandSchema
          { cmdDescription = "Secure copy"
          , cmdPositional =
              [ ArgSpec TPath True "Source"
              , ArgSpec TPath True "Destination"
              ]
          , cmdArgs =
              Map.fromList
                [ ("-P", ArgSpec TInt False "Port")
                , ("-i", ArgSpec TPath False "Identity file")
                , ("-F", ArgSpec TPath False "Config file")
                , ("-l", ArgSpec TInt False "Bandwidth limit")
                ]
          }
      )
    ,
      ( "find"
      , CommandSchema
          { cmdDescription = "Find files"
          , cmdPositional = [ArgSpec TPath True "Path"]
          , cmdArgs =
              Map.fromList
                [ ("-maxdepth", ArgSpec TInt False "Max depth")
                , ("-mindepth", ArgSpec TInt False "Min depth")
                , ("-mtime", ArgSpec TInt False "Modification time days")
                , ("-mmin", ArgSpec TInt False "Modification time minutes")
                , ("-size", ArgSpec TString False "Size")
                , ("-name", ArgSpec TString False "Name pattern")
                , ("-type", ArgSpec TString False "Type")
                ]
          }
      )
    ,
      ( "parallel"
      , CommandSchema
          { cmdDescription = "Run commands in parallel"
          , cmdPositional = [ArgSpec TString False "Command"]
          , cmdArgs =
              Map.fromList
                [ ("-j", ArgSpec TInt False "Number of jobs")
                , ("--jobs", ArgSpec TInt False "Number of jobs")
                , ("--delay", ArgSpec TInt False "Delay between jobs")
                , ("--timeout", ArgSpec TInt False "Timeout per job")
                , ("--retries", ArgSpec TInt False "Number of retries")
                ]
          }
      )
    ,
      ( "nix"
      , CommandSchema
          { cmdDescription = "Nix package manager"
          , cmdPositional = [ArgSpec TString True "Subcommand"]
          , cmdArgs =
              Map.fromList
                [ ("--max-jobs", ArgSpec TInt False "Max build jobs")
                , ("-j", ArgSpec TInt False "Max build jobs")
                , ("--cores", ArgSpec TInt False "Cores per build")
                ]
          }
      )
    ]
