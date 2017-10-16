--
-- h1.hs
-- @author Sidharth Mishra
-- @description sample hs file for cheat sheet
-- @copyright 2017 Sidharth Mishra
-- @created Mon Oct 16 2017 09:39:12 GMT-0700 (PDT)
-- @last-modified Mon Oct 16 2017 09:39:12 GMT-0700 (PDT)
--

import System.Environment (getArgs)

readFromCommandLine :: IO()
readFromCommandLine = do
  args <- getArgs
  case args of
    [input, output] -> print $ input ++ " :: " ++ output
    _ -> print "error:: needed 2 command line arguments"

main :: IO()
main = readFromCommandLine

