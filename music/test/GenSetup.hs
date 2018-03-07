{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
module GenSetup ( genMelody
             , genNote
             , genPitch
             , genDur
             , generate
             ) where

import           Data.DeriveTH
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Music

-- | Automatically derive 'Arbitrary' instances.
derive makeArbitrary ''PitchClass
derive makeArbitrary ''Octave
derive makeArbitrary ''Music
derive makeArbitrary ''Interval

-- | Simple generators.
genMelody :: Gen Melody
genMelody = line <$> listOf1 genNote

genNote :: Gen Melody
genNote = (<|) <$> genPitch <*> genDur

genPitch :: Gen Pitch
genPitch = (,) <$> arbitrary <*> arbitrary

genDur :: Gen Duration
genDur = elements [1%16,1%8,1%4,1%2]
