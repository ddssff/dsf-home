#!/usr/bin/env -S runghc -i

-- | Recursive grep with line numbers, skipping the build tree:
--
-- >  grep -r -n --exclude-dir=dist-newstyle -e "$@"
--
-- The arguments go straight through, so the first is the pattern (it
-- follows -e, which means a pattern beginning with a dash works) and
-- any that follow it are the files or directories to search.  With no
-- path given, grep -r searches the working directory.

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (proc, runProcess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      me <- getProgName
      hPutStrLn stderr ("usage: " <> me <> " PATTERN [PATH ...]")
      -- Same code grep itself uses for a usage error.
      exitWith (ExitFailure 2)
    _ ->
      -- runProcess inherits the standard streams, so matches and any
      -- complaints from grep appear as they normally would.  Its exit
      -- code is grep's: 0 if something matched, 1 if nothing did, 2 on
      -- error -- worth passing along, since callers test it.
      exitWith =<< runProcess (proc "grep" (flags <> args))
  where
    flags = ["-r", "-n", "--exclude-dir=dist-newstyle", "--exclude-dir=dist-native", "--exclude-dir=dist-javascript", "-e"]
