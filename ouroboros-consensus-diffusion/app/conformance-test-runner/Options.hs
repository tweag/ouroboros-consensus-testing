{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command line argument parser for the test runner.
module Options (parseOptions, Options (..)) where

import ExitCodes
import Options.Applicative
  ( CompletionResult (execCompletion)
  , Parser
  , ParserInfo
  , ParserResult (CompletionInvoked, Failure)
  , auto
  , defaultPrefs
  , execParserPure
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , renderFailure
  , short
  , strArgument
  , strOption
  , value
  , (<**>)
  )
import qualified Options.Applicative as O
import Ouroboros.Network.PeerSelection (PortNumber)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optTestFile :: FilePath
  , optOutputTopologyFile :: String
  , optPort :: PortNumber
  }

options :: ParserInfo Options
options =
  info
    (optsP <**> helper)
    ( mconcat
        [ fullDesc
        , progDesc
            ( mconcat
                [ "Locally simulate peers described by the TEST_FILE "
                , "to tests a node's resulting state for consensus"
                ]
            )
        , header "runner - A conformance test runner"
        ]
    )

optsP :: Parser Options
optsP = do
  optTestFile <- strArgument $ metavar "TEST_FILE"
  optOutputTopologyFile <-
    strOption
      ( mconcat
          [ long "output-topology-file"
          , short 'o'
          , metavar "FILE_NAME"
          , value "topology.json"
          , help "File path for the testing topology file (JSON)"
          ]
      )
  optPort <-
    option
      auto
      ( mconcat
          [ long "port"
          , short 'p'
          , metavar "PORT_NUMBER"
          , value 3001
          , help "Starting port for simulated peers"
          ]
      )
  pure Options{..}

parseOptions :: [String] -> IO (Options)
parseOptions args =
  case execParserPure defaultPrefs options args of
    O.Success opts -> pure opts
    Failure failure -> do
      let (msg, _) = renderFailure failure "conformance-test-runner"
      hPutStrLn stderr msg
      exitWithStatus BadUsage
    CompletionInvoked compl -> do
      -- Completion handler
      msg <- execCompletion compl "conformance-test-runner"
      putStr msg
      exitWithStatus Success
