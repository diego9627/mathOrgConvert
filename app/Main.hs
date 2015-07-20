module Main where

import           System.Environment
import           Converter
import qualified Text.Pandoc.UTF8 as UTF8

main :: IO ()
main = do
  args <- getArgs
  inpt <- UTF8.readFile (args !! 0)
  UTF8.writeFile (args !! 1) (toMathOrgMD inpt)
  return ()
  
