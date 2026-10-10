#!/usr/bin/env -S runghc -i

{-# LANGUAGE RecordWildCards #-}

-- | Recursive grep with line numbers, skipping the build tree:
--
-- >  grep -r -n --exclude-dir=dist-newstyle -e "$@"
--
-- The arguments go straight through, so the first is the pattern (it
-- follows -e, which means a pattern beginning with a dash works) and
-- any that follow it are the files or directories to search.  With no
-- path given, grep -r searches the working directory.

import qualified Options.Applicative as Opt (execParser, execParserPure, many, Parser, ParserInfo)
import Options.Applicative (argument, fullDesc, help, info, long, metavar, some, str, strOption, short, switch, value)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess(..), CmdSpec(..), showCommandForUser)
import System.Process.Typed (proc, runProcess)
import System.Process.Typed.Internal (ProcessConfig(..))

data Options =
  Options
  { excludeDirs_ :: [String]
  , caseInsensitive_ :: [String]
  , extra_ :: [String] -- ^ Unused arguments from command line
  }

options :: Opt.Parser Options
options = do
  Options
  <$> argument str (metavar "COMMAND")
  <*> 
  args <- getArgs
  

-- | Wrapper for grep(1) that adds some --exclude-dir arguments before the "-e"
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      exe <- getProgName
      hPutStrLn stderr ("usage: " <> exe <> " PATTERN [PATH ...]")
      -- Same code grep itself uses for a usage error.
      exitWith (ExitFailure 2)
    _ -> do
      let cmd = proc "grep" (flags <> args)
      -- hPutStrLn stderr (" -> " <> showProcessConfigForUser cmd)
      -- runProcess inherits the standard streams, so matches and any
      -- complaints from grep appear as they normally would.  Its exit
      -- code is grep's: 0 if something matched, 1 if nothing did, 2 on
      -- error -- worth passing along, since callers test it.
      exitWith =<< runProcess cmd
  where
    flags = ["-r", "-n"] <> fmap (\path -> "--exclude-dir=" <> path) excludeDirs <> ["-e"]
    excludeDirs = ["dist-newstyle", "dist-native", "dist-javascript", "dist-wasm", ".git"]

-- | extra System.Process
showProcessConfigForUser :: ProcessConfig i o e -> String
showProcessConfigForUser ProcessConfig{..} = showCmdSpecForUser pcCmdSpec
showCreateProcessForUser :: CreateProcess -> String
showCreateProcessForUser CreateProcess{..} = showCmdSpecForUser cmdspec
showCmdSpecForUser (ShellCommand s) = s
showCmdSpecForUser (RawCommand path args) = showCommandForUser path args
