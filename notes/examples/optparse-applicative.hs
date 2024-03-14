-- Example: cabal-debian

import Options.Applicative as O -- (execParserPure, prefs, idm, Parser)
import System.Environment (getArgs)
-- import Options.Applicative
import Data.Semigroup ((<>))

test :: IO ()
test = do
  let r = execParserPure defaultPrefs (info sample fullDesc) ["--hello", "Dave", "-q", "--enthusiasm", "3"]
  putStrLn (show r)
-- parseProgramArguments = getArgs >>= execParserPure (prefs idm) commandLineOptionsParserInfo

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }
  deriving (Show)

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
