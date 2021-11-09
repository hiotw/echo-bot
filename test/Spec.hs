module Main where


import Test.HUnit
import System.Environment
import Telegram.Test as T


main :: IO ()
main = do
  counts <- runTestTT T.tests
  putStrLn $ showCounts counts
