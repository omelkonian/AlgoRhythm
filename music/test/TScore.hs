{-# LANGUAGE ScopedTypeVariables #-}
module TScore where

import           System.Directory               (doesFileExist)
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

import           GenSetup
import           Music
import           Score                          (writeToLilypondFile)

scoreTests = testGroup "Score"
  [ testCase "successfully write to file" $
      let res = do let f = "test.ly"
                   m <- generate genMelody
                   _ <- writeToLilypondFile f m
                   doesFileExist f
      in  unsafePerformIO res @?= True
  ]
