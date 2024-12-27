module Main (main) where

import Test.HUnit

import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec

testSuite :: Test
testSuite =
  TestLabel "Spec Test Suit" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]

main :: IO ()
main = runTestTTAndExit $ test [testSuite, testesTarefa1, testesTarefa2, testesTarefa3]
