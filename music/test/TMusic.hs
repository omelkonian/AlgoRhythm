{-# LANGUAGE ScopedTypeVariables #-}
module TMusic where

import           Control.Arrow                        ((>>>))
import           Data.DeriveTH
import           Test.Framework                       (testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit
import           Test.QuickCheck                      ((==>))
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Data.Ratio                           ((%))
import           Music
import           Score
import           Setup

musicTests = testGroup "Music"
  [ testCase "transpose" $
      transposePitch 4 (C<@4) @?= E<@4
  , testProperty "transposeId" $ \(p :: Pitch) ->
      transposePitch 0 p == p
  , testProperty "transposeTrans" $ \(p :: Pitch) (m :: Int) (n :: Int) ->
      m > 0 && n > 0 ==>
      (transposePitch n >>> transposePitch m) p == transposePitch (m + n) p
  ]
