module Telegram.Test where

import Test.HUnit
import Bot.Internal.Telegram as TBot

import Data.ByteString.Lazy.Char8 as BS

test1 = TestCase ( do handle <- new ( Config "" ) 
                      assertEqual "Handle from invalid config"
                                  TBot.empty
                                  handle 
                 )


test2 = TestCase ( do handle <- new ( Config "/dev/null" )
                      assertEqual "Handle from invalid config"
                                  TBot.empty
                                  handle
                 )


test3 = TestCase ( do handle <- new ( Config "./test/Telegram/parsable_config.txt" )
                      assertEqual "New with valid path"
                                  ( TBot.Handle "" "" )
                                  handle
                 )


-- Prerequisite: pening updates on Telegram server's side
test4 = TestCase ( do updates <- ( TBot.update TBot.empty )
                      assertEqual "Update with empty handle"
                                  updates 
                                  ( BS.pack $  "{\"ok\":false,\"error_code\":\
                                         \404,\"description\":\"Not Found\"}" )
                 )


tests = TestList [ TestLabel "test 1" test1 
                 , TestLabel "test 2" test2
                 , TestLabel "test 3" test3
                 , TestLabel "test 4" test4
                 ]
