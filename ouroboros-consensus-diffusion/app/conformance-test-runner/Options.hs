{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command line argument parser for the test runner.
module Options (execParserPure, handleParseResult, overFailure, defaultPrefs, options, Options (..)) where

import Options.Applicative
import Ouroboros.Network.PeerSelection (PortNumber)

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
  optTestFile <- argument auto (metavar "TEST_FILE")
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
