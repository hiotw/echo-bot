module Telegram.Test where

import Test.HUnit
import Bot.Internal.Telegram as TBot


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


tests = TestList [ TestLabel "test 1" test1 
                 , TestLabel "test 2" test2
                 , TestLabel "test 3" test3
                 ]
