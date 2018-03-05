{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
module Setup where

import           Data.DeriveTH
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

import           Music

derive makeArbitrary ''PitchClass
derive makeArbitrary ''Octave
derive makeArbitrary ''Music
