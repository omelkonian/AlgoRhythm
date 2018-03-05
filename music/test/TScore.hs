{-# LANGUAGE ScopedTypeVariables #-}
module TScore where

import           Control.Arrow                        ((>>>))
import           Data.DeriveTH
import           System.IO.Unsafe
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck                      ((==>))
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Music
import           Score                                (writeToLilypondFile)
import           Setup

import           Data.Ratio                           ((%))

genDur :: Gen Duration
genDur = elements [1%16,1%8,1%4,1%2]

genOctave :: Gen Octave
genOctave = elements [Oct3,Oct4,Oct5]

genPitch :: Gen Pitch
genPitch = do
  p <- arbitrary
  o <- genOctave
  return (p, o)

genMelody :: Gen (Music Pitch)
genMelody = do
  d <- genDur
  p <- genPitch
  return $ p <| d

genList :: Gen [Melody]
genList = listOf1 genMelody

scoreTests = testGroup "Score"
  [ testProperty "sample" $
      let r = unsafePerformIO $ do t <- generate genMelody
                                   print t
                                   writeToLilypondFile "test.ly" t
                                   return t
      in  r == Rest (1%1)
  -- \(ps :: [Music Pitch])->
  --     not (null ps) ==>
  --     let go = writeToLilypondFile f (line ps)
  --         f = if length ps > 50 then "big.ly" else "small.ly"
  --     in  (trace (show ps) $ unsafePerformIO go) == ()
  ]
