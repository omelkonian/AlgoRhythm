{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenSetup
       ( genScale
       , genChord
       , genMelody
       , genNote
       , genPitch
       , genDur
       , generate
       ) where

import Data.DeriveTH
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Music

-- | Automatically derive 'Arbitrary' instances.
derive makeArbitrary ''PitchClass
derive makeArbitrary ''Octave
derive makeArbitrary ''Music
derive makeArbitrary ''Interval

-- | Simple generators.
genScale :: Gen AbstractScale
genScale = elements
  [ major, pentatonicMajor, ionian, dorian, phrygian, lydian, mixolydian, aeolian
  , locrian, minor, harmonicMinor, melodicMinor, pentatonicMinor, blues
  , bebopDominant, bebopDorian, bebopMajor, bebopMelodicMinor, bebopHarmonicMinor
  , altered, wholeTone, halfDiminished, flamenco
  ]

genChord :: Gen AbstractChord
genChord = elements
  [ maj, mi, dim, aug, sus4, d7sus4, maj6, m6, maj7, m7, d7, dim7, m7b5
  , maj9, m9, d9, d7b5, d7s5, d7b9, d7s9, d7b5b9, d7b5s9, d7s5b9, d7s5s9
  ]

genMelody :: Gen Melody
genMelody = line <$> listOf1 genNote

genNote :: Gen Melody
genNote = (<|) <$> genPitch <*> genDur

genPitch :: Gen Pitch
genPitch = (,) <$> arbitrary <*> arbitrary

genDur :: Gen Duration
genDur = elements [1%16,1%8,1%4,1%2]
