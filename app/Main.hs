module Main where

import           Lib
import           System.Environment
import qualified System.IO          as IO

main :: IO ()
main = do
  arg  <- getArgs
  prog <- getProgName

  case arg of
    [config] -> IO.hPutStrLn IO.stderr $ "Argument: " ++ config
    _        -> IO.hPutStrLn IO.stderr $ "Usage: " ++ prog ++ " <config path>"
