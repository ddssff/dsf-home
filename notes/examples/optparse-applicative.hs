-- Example: cabal-debian

import Options.Applicative as O (execParserPure, prefs, idm)
import System.Environment (getArgs)

parseProgramArguments = getArgs >>= execParserPure (prefs idm) commandLineOptionsParserInfo


import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )
