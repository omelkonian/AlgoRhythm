{-# language GADTs #-}

-- | An example implementation of a `Generate.Chaos` that generates music with
--   chaotic octave and pitch selection.
module Generate.Applications.ChaosPitches (
    genChaosMusic
  , chaos1
  , bSolo
  , chaos1Selector) where

import Music
import Utils.Vec
import Generate.Generate
import Control.Monad.State hiding (state)
import Generate.Chaos

-- | Generates `Music` with chaos function f x = 1 - 1.9521 * x^2 in range [-1,1]
--   with initial x = 1.2.
genChaosMusic :: IO (Music Pitch)
genChaosMusic = do
  let mapping = defaultMapping {pcSel=chaos1Selector, octSel=chaos1Selector }
  runChaosGenerator chaos1 mapping bSolo

-- | ChaosState with chaos function f x = 1 - 1.9521 * x^2 in range [-1,1]
--   with initial x = 1.2.
chaos1 :: ChaosState D1
chaos1 = do
  let startX = 1.2
  buildChaos (startX :. Nil) (f :. Nil)
  where f :: (Vec D1 Double -> Double)
        f (x:.Nil) = max (-1) (min 1 (1 - 1.9521 * x**2))

-- | `MusicGenerator` that uses `chaos1` to generate some blues music.
bSolo :: MusicGenerator (ChaosState D1) Melody
bSolo = do
  addConstraint pitchClass (`elem` (E +| blues :: [PitchClass]))
  run1 <- local $ do
    octave   >! (`elem` [4,5])
    duration >! (`elem` [1%32, 1%16])
    notes <- 12 .#. genNote
    return $ line notes
  run2 <- local $ do
    octave     >! (`elem` [2,3,4])
    duration   >! (`elem` [1%8, 1%16])
    pitchClass >! (`elem` [E, Fs, Gs, B, Cs])
    notes <- 6 .#. genNote
    return $ line notes
  return $ run1 :=: run2

-- | The selector that maps the chaos function from `chaos1` to an element in a.
chaos1Selector :: Selector (ChaosState n) a
chaos1Selector s as = do
  ([d], s') <- runStateT genNextIteration s
  let dNormalised = (d+1) / 2
  let maxI = fromIntegral (length as - 1)
  let index = round (dNormalised * maxI)
  let a = as !! index
  return (snd a, s')
