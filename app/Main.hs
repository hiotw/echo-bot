module Main where

import           Lib
import           System.Environment
import qualified System.IO          as IO
import qualified Bot.Telegram       as TBot

main :: IO ()
main = do
  arg  <- getArgs
  prog <- getProgName

  case arg of
    [config] -> run config
    _        -> IO.hPutStrLn IO.stderr $ "Usage: " ++ prog ++ " <config path>"


run :: FilePath -> IO ()
run path = do
  hBot <- TBot.new (TBot.Config path)
  TBot.echo hBot

  print "Done"
