module Block where

import System.Posix
import Debug.Trace
import Data.Bits
import Data.List
import Data.Maybe
import Data.Ratio
import Control.Monad (liftM)
import Text.Regex

fromPath :: String -> IO DeviceID
fromPath path =
    do status <- System.Posix.getFileStatus path
       return (System.Posix.deviceID status)

fromNode :: String -> IO DeviceID
fromNode "/dev/root" = fromPath "/"
fromNode node =
    do status <- System.Posix.getFileStatus node
       return (System.Posix.specialDeviceID status)

fromSysName :: String -> IO DeviceID
fromSysName name = fromNode ("/dev/" ++ name)

fromNumber :: Int -> DeviceID
fromNumber n = read (show n)

toNumber :: DeviceID -> Int
toNumber b = read (show b)

major :: DeviceID -> Int
major b = Data.Bits.shiftR (toNumber b) 8

minor :: DeviceID -> Int
minor b = (toNumber b) .&. 0xff

fromMajorMinor :: Int -> Int -> DeviceID
fromMajorMinor maj min = fromNumber (Data.Bits.shiftL maj 8 + min)


-- Convert a string in the form "NN:NN\n" (such as might appear in
-- /sys/block/.../dev) to a DeviceID
fromDevString :: String -> DeviceID
fromDevString s = 
    let devStringRE = mkRegex "^([0-9]+):([0-9]+)\n$" in
    let parsed = fromJust (matchRegex devStringRE s) in
    fromMajorMinor (read (parsed !! 0)) (read (parsed !! 1))

fromSysPath :: String -> IO DeviceID
fromSysPath path =
    do
      text <- readFile (path ++ "/dev")
      return (fromDevString text)

test1 path =
    do
      b <- fromNode path
      print (toNumber b)

test2 path =
    do
      lst <- fromSysPath path
      print lst
