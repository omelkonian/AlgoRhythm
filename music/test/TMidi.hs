{-# LANGUAGE ScopedTypeVariables #-}
module TMidi where

import           System.Directory               (doesFileExist)
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit

import           GenSetup
import           MIDI                           (writeToMidiFile)
import           Music

midiTests = testGroup "MIDI"
  [ testCase "successfully write to file" $
      let res = do let f = "test.midi"
                   m <- generate genMelody
                   writeToMidiFile f m
                   doesFileExist f
      in  unsafePerformIO res @?= True
  ]
