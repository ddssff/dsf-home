#!/usr/bin/runhaskell

--module Archive where

import Data.List
import System.Time
import System.Locale
import System.Directory
import System.IO
import Data.Maybe
import System.Environment
import System.Cmd
import System.Exit
import Text.Regex
import Data.Char(chr)
--import Data.Char
import qualified Data.ByteString as B
import Linspire.Unix.Process

data Option
    = DryRun
    | Unlink
    | Current
    | Prune String
    | Rsync String
    | Extra String
    deriving (Eq, Show)

rsyncArgs = ["-v", "-P", "-c", "--delete-excluded", "--delete-after", "--partial", "--force", "--size-only"]

main :: IO ()
main =
    do
      (options, original, backupDirectory) <- getOptions
      today <- getClockTime >>= toCalendarTime >>= return . formatCalendarTime defaultTimeLocale "%Y%m%d"
      -- If the ALWAYS flag is set we may need to create a directory with hours
      -- minutes and seconds in the name.
      hPutStrLn stderr (original ++ " -> " ++ backupDirectory ++ today)
      createDirectoryIfMissing True backupDirectory
      -- Remove any incomplete archives.  These are left by runs that died
      -- while the older archive was being copied.
      removeIncomplete options backupDirectory
      -- Create a copy of the latest complete archive with today's
      -- date and the outofdate prefix.
      linkToLatest options backupDirectory today
      -- Now we need to do the rsync from the remote directory.
      updateFromOriginal options original backupDirectory today

getOptions :: IO ([Option], String, String)
getOptions =
    do
      args <- getArgs
      let options = processArgs args
      case filter isExtra options of
        [Extra orig, Extra backup] -> return (options, orig ++ "/", backup ++ "/")
        [Extra orig] -> usage "missing destination" >>= error
        [] -> usage "missing source and destination" >>= error
        (_ : _ : unexpected) -> usage ("unexpected: " ++ show unexpected) >>= error
    where
      processArgs [] = []
      -- processArgs options ("-A" : etc) = processArgs ([Always] : options) etc
      processArgs ("--prune" : x : etc) = Prune x : processArgs etc
      processArgs ("--unlink" : etc) = Unlink : processArgs etc
      processArgs ("--current" : etc) = Current : processArgs etc
      processArgs ("--exclude" : x : etc) = Rsync "--exclude" : Rsync x : processArgs etc
      processArgs ("-n" : etc) = DryRun : Rsync "-n" : processArgs etc
      processArgs ("--dry-run" : etc) = DryRun : Rsync "-n" : processArgs etc
      processArgs (x : etc) | elem x rsyncArgs = Rsync x : processArgs etc
      processArgs (x : etc) | isPrefixOf "--timeout=" x = Rsync x : processArgs etc
      processArgs (x : etc) | isPrefixOf "--bwlimit=" x = Rsync x : processArgs etc
      processArgs (x : etc) = Extra x : processArgs etc
      isExtra (Extra _) = True
      isExtra _ = False

usage :: String -> IO String
usage message =
    do
      putStrLn . concat . intersperse "\n" $
        [message,
         "",
         "Usage:",
         "   archive [options] original backupdir",
         "",
         "Options:",
         "  --prune <number>	-- limit the number of backup dirs to <number>",
         "  --unlink		-- Keep only the most recent hard link.  The newest",
         "		   backup is always complete, but the previous day",
         "		   will only include the files that changed or were",
         "		   removed.",
         "  -A			-- Always create a new backup, without this option",
         "		   today's backup will be updated if it already exists",
         "  --exclude		-- Passed to rsync, implies rsync's --delete-excluded",
         "		   flag (so that adding this flag makes files go away",
         "		   in newer backups.)",
         "  --current		-- Create a link named 'current' to the new archive.",
         "",
         "Create a backup of ORIGINAL in BACKUPDIR in a directory whose name is",
         "todays date.  The original may be on a remote machine.",
         "",
         "This is achieved without wasting disk space on unchanged files using",
         "a simple incremental backup technique I read about somewhere using",
         "cp -al to create a hard linked copy of the previous backup and rsync",
         "to modify that copy into a copy of the current directory.  It does use",
         "a lot of inodes, but I haven't run out yet.",
         "",
         "Example crontab:",
         " 0 1 * * * /root/bin/archive --prune 20 --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@dsf:/home/dsf /backups/dsf",
         " 0 2 * * * /root/bin/archive --prune 20 root@p4:/home/audio /backups/audio",
         " 20 2 * * * /root/bin/archive --prune 10 root@p4:/disks/hdc3/cdroms /backups/cdroms",
         " 30 2 * * * /root/bin/archive --exclude '/.mozilla/**/Cache/' --exclude '/.kde/share/cache/' root@t22:/root /backups/ldt-t22",
         " 40 2 * * * /root/bin/archive root@dsf:/var/lib/geneweb /backups/geneweb"]
      return message

removeIncomplete options backupDirectory =
    getDirectoryContents backupDirectory >>= return . filter isIncomplete  >>= mapM removeArchive
    where
      isIncomplete name = maybe False (== "incomplete.") . archivePrefix $ name
      removeArchive name = command options ("rm -rf " ++ backupDirectory ++ name)
      archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"

linkToLatest options backupDirectory today =
    do
      archives <- getDirectoryContents backupDirectory >>= return . filter isComplete
      case listToMaybe . sortBy (\ a b -> compare (archiveDate b) (archiveDate a)) $ archives of
        -- There is an outofdate archive from today, use it as is.
        Just archive | (backupDirectory ++ archive) == outofdate today ->
                return ()
        -- there is a finished archive from today, add the outofdate. prefix
        Just archive | (backupDirectory ++ archive) == destination today ->
                myRenameDirectory options (backupDirectory ++ archive) (outofdate today)
        -- The newest archive wasn't finished, rename it.
        Just archive | isPrefixOf "incomplete." archive ->
	        myRenameDirectory options (backupDirectory ++ archive) (outofdate today)
        Just archive ->
            -- The newest archive is finished and not from today, link to it.
            do command options ("cp -al " ++ backupDirectory ++ archive ++ " " ++ incomplete today)
               myRenameDirectory options (incomplete today) (outofdate today)
        -- There are no archives in the directory at all
        Nothing -> createDirectoryIfMissing True (outofdate today)
    where
      destination date = backupDirectory ++ date
      incomplete date = backupDirectory ++ "incomplete." ++ date
      isComplete name = maybe False (flip elem ["outofdate.", ""]) . archivePrefix $ name
      outofdate date = backupDirectory ++ "outofdate." ++ date

updateFromOriginal options original backupDirectory today =
    do
      let cmd = ("rsync " ++ rsync options ++
                 "-aHxSpDtl --partial --delete --recursive --delete-excluded --stats " ++
                 original ++ " " ++ outofdate today)
      hPutStrLn stderr ("> " ++ cmd)
      hPutStr stderr "  Updating from original ... "
      output <- if elem DryRun options then
                    return [Result ExitSuccess] else
                    lazyCommand cmd [] >>= mapM doOutput
      case exitCodeOnly output of
        [] ->
            error "No exit status from rsync process!?"
        ExitFailure n : _ ->
            do
              hPutStrLn stderr ("Exiting with code " ++ show n)
              exitWith (ExitFailure n)
        ExitSuccess : _ ->
            do
              myRenameDirectory options (outofdate today) (destination today)
              -- We would like to mark archives that are identical to
              -- others, but at this point we don't know what the
              -- original archive was.  Probably need to pass another
              -- argument in.
              case matchRegex sizeRE . byteStringToString . B.concat . stdoutOnly $ output of
                Just ["0"] -> hPutStrLn stderr "Zero bytes transferred."
                _ -> hPutStrLn stderr "More than zero bytes transferred."
    where
      sizeRE = mkRegex "Total transferred file size: ([0-9]*) bytes"
      rsync (Rsync x : options) = x ++ " " ++ rsync options
      rsync (_ : options) = rsync options
      rsync [] = ""
      destination date = backupDirectory ++ date
      outofdate date = backupDirectory ++ "outofdate." ++ date
      doOutput x@(Stdout s) = do B.hPut stdout s; return x
      doOutput x@(Stderr s) = do B.hPut stderr s; return x
      doOutput x = return x

byteStringToString :: B.ByteString -> String
byteStringToString b = map (chr . fromInteger . toInteger) . B.unpack $ b

myRenameDirectory options old new =
    if elem DryRun options then
        hPutStrLn stderr ("renameDirectory " ++ show old ++ " " ++ show new) else
        renameDirectory old new

command options s =
          do hPutStrLn stderr ("> " ++ s)
             if elem DryRun options then
                 return () else
                 do
                   result <- system s
                   case result of
                     ExitSuccess -> return ()
                     failure -> exitWith failure

archiveDate name =
          case matchRegex archiveNameRE name of
            Just [_, date] -> Just date
            Nothing -> Nothing
            _ -> error "internal error"
archivePrefix name =
          case matchRegex archiveNameRE name of
            Just [prefix, _] -> Just prefix
            Nothing -> Nothing
            _ -> error "internal error"
archiveNameRE = mkRegex "^(.*)([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])$"
