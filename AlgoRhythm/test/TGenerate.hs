{-# LANGUAGE TemplateHaskell #-}

module TGenerate where

import           Test.Framework                 (testGroup)
import           Test.Framework.Providers.HUnit (testCase)
import           System.IO.Unsafe               (unsafePerformIO)
import           Test.HUnit

import           Generate
import           Generate.QuickCheck
import           Music
import           Data.Ratio
import qualified Data.Music.Lilypond as Ly

import           Control.Monad

ioFromGen = runGenerator (quickCheckState ())

genTests = testGroup "Generate"
  [ testCase "genNote yields a single note" $
      let res = ioFromGen (genNote)
      in (countNotes $ unsafePerformIO res) == 1 @? "unexpected note count",
    testCase "replicate generators yields correct number of results" $
      let res = ioFromGen (replicateM 10 genNote)
      in (countNotes (line $ unsafePerformIO res) == 10) @? "unexpected note count",
    testCase "pitchClass constraint" $
      let res = ioFromGen melodyInC
      in (all (inC . fst) $ unsafePerformIO res) @? "found notes not in the key of C"
  ]

instance Monoid Int where
  mappend = (+)
  mempty  = 0

countNotes :: Melody -> Int
countNotes = foldMap (const 1)

melodyInC :: MusicGenerator (GenState ()) Melody
melodyInC = do
  addConstraint pitchClass inC
  notes <- replicateM 20 genNote
  return $ line notes

inC :: PitchClass -> Bool
inC pc = elem pc [C, D, E, F, G, A, B]

inG :: PitchClass -> Bool
inG pc = elem pc [G, A, B, C, D, E, Fs, G]
