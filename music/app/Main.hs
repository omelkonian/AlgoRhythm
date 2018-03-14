{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import           Export (playDev, writeToLilypondFile, writeToMidiFile)
import           Music
import           Music.Generate
import           Control.Monad

import           Data.TypeLevel.Num hiding ((-), (+), (*), (/))
import           Music.Generate.Chaos

main :: IO ()
main = do
  -- x   <- runGenerator chaos1 tbBlues
  mel <- runGenerator chaos1 bSolo
  playDev 0 mel
  -- writeToMidiFile "gen.midi" (x :=: (mel :+: ((mel><) ~> P8)))
  -- writeToLilypondFile "gen.ly" x

bSolo :: MusicGenerator (ChaosState D2) Melody
bSolo = do
  addConstraint octave     (`elem` [4,5])
  addConstraint pitchClass (`elem` [E, G, A, As, B, D])
  run1 <- local $ do
    addConstraint duration ((==) (1%16))
    notes <- replicateM 12 genNote
    return $ line notes
  run2 <- local $ do
    addConstraint duration (`elem` [1%8, 1%16])
    addConstraint pitchClass (`elem` [E, Fs, Gs, B, Cs])
    notes <- replicateM 8 genNote
    return $ line notes
  return $ run1 :+: run2


tbBlues :: MusicGenerator (ChaosState D2) Melody
tbBlues = do
  addConstraint duration ((==) hn)
  c1 <- one
  c4 <- four
  c5 <- five
  return ((8##c1) :+: (4##c4) :+: (4##c1) :+: (2##c5) :+: (2##c4) :+: (2##c1) :+: (2##c5))

one :: MusicGenerator (ChaosState D2) Melody
one = local $ do
  addConstraint pitchClass (`elem` [E, Gs, B])
  addConstraint octave     (`elem` [3, 4])
  c <- genChord 4
  return $ c

four :: MusicGenerator (ChaosState D2) Melody
four = local $ do
  addConstraint pitchClass (`elem` [A, Cs, E])
  addConstraint octave     (`elem` [3,4])
  c <- genChord 4
  return $ c

five :: MusicGenerator (ChaosState D2) Melody
five = local $ do
  addConstraint pitchClass (`elem` [B, Ds, Fs, A])
  addConstraint octave     (`elem` [3, 4])
  c <- genChord 4
  return $ c
