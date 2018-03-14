{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Export
import Generate
import Generate.Chaos
import Music
import Grammar.Examples (final)

main :: IO ()
main = do
  cp <- final C (16 * wn)
  -- writeToMidiFile "cp.midi" cp
  -- putStrLn "Wrote to MIDI."
  playDev 4 (MIDIConfig (3%5) RhodesPiano) cp
  putStrLn "Playback finished."

  -- x   <- runGenerator chaos1 tbBlues
  -- mel <- runGenerator chaos1 bSolo
  -- playDev 4 defaultMIDIConfig mel
  -- writeToMidiFile "gen.midi" (x :=: (mel :+: ((mel><) ~> P8)))
  -- writeToLilypondFile "gen.ly" x

bSolo :: MusicGenerator s Melody
bSolo = do
  addConstraint octave     (`elem` [4,5])
  addConstraint pitchClass (`elem` [E, G, A, As, B, D])
  run1 <- local $ do
    addConstraint duration ((1%16) ==)
    notes <- replicateM 12 genNote
    return $ line notes
  run2 <- local $ do
    addConstraint duration (`elem` [1%8, 1%16])
    addConstraint pitchClass (`elem` [E, Fs, Gs, B, Cs])
    notes <- replicateM 8 genNote
    return $ line notes
  return $ run1 :+: run2


tbBlues :: MusicGenerator s Melody
tbBlues = do
  addConstraint duration (hn ==)
  c1 <- bluesChord E d7
  c4 <- bluesChord A d9
  c5 <- bluesChord B d7s9
  return ((8##c1) :+: (4##c4) :+: (4##c1) :+: (2##c5) :+: (2##c4) :+: (2##c1) :+: (2##c5))

bluesChord :: PitchClass -> AbstractChord -> MusicGenerator s Melody
bluesChord p ch = inChord p ch >> inOctaves [3,4] >> genChord 4

inChord :: PitchClass -> AbstractChord -> MusicGenerator s ()
inChord p ch = addConstraint pitchClass (`elem` (p=|ch :: [PitchClass]))

inOctaves :: [Octave] -> MusicGenerator s ()
inOctaves oc = addConstraint octave (`elem` oc)
