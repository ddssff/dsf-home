-- Simple CGI application
-- ghc --make -fglasgow-exts -W -o dbtest cgitest.hs

import qualified Network.CGI as CGI
import Text.Html
import Data.List

main :: IO ()
main =
    CGI.wrapper application

application :: [(String,String)] -> IO Html
application cgivars =
    return (ulist (concatHtml (map (\ (name, value) -> li (stringToHtml (name ++ "=" ++ value))) cgivars)))
