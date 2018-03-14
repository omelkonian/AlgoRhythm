{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import           Export (playDev, writeToLilypondFile, writeToMidiFile)
import           Music
import           Music.Generate
import           Control.Monad

import           Music.Generate.QuickCheck

main :: IO ()
main = do
  x   <- runGenerator () tbBlues
  mel <- runGenerator () bSolo
  playDev 0 x
  writeToMidiFile "gen.midi" (x :=: (mel :+: ((mel><) ~> P8)))
  writeToLilypondFile "gen.ly" x

bSolo :: MusicGenerator () Melody
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


tbBlues :: MusicGenerator () Melody
tbBlues = do
  addConstraint duration ((==) hn)
  c1 <- one
  c4 <- four
  c5 <- five
  return ((8##c1) :+: (4##c4) :+: (4##c1) :+: (2##c5) :+: (2##c4) :+: (2##c1) :+: (2##c5))

one :: MusicGenerator () Melody
one = local $ do
  addConstraint pitchClass (`elem` [E, Gs, B])
  addConstraint octave     (`elem` [3, 4])
  c <- genChord 4
  return $ c

four :: MusicGenerator () Melody
four = local $ do
  addConstraint pitchClass (`elem` [A, Cs, E])
  addConstraint octave     (`elem` [3,4])
  c <- genChord 4
  return $ c

five :: MusicGenerator () Melody
five = local $ do
  addConstraint pitchClass (`elem` [B, Ds, Fs, A])
  addConstraint octave     (`elem` [3, 4])
  c <- genChord 4
  return $ c
