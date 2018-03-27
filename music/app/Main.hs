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
  writeToLilypondFile "out.ly" small
  putStrLn (show $ musicToLilypond small)
  playDev 4 (MIDIConfig (3%5) RhodesPiano) cp
  putStrLn "Playback finished."

small :: MusicCore
small = (C#4 <: []) <| (11%16)
