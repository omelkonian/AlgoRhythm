{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import Export
import Grammar.Examples (final, Config (..))
import Music

main :: IO ()
main = do
  let ?config = Config
        { baseOct = def
        , basePc  = D
        , baseScale = harmonicMinor
        , chords  =
            [ (10, maj), (10, mi)
            , (8, d7), (8, maj6), (8, m6)
            , (7, maj7), (7, m7)
            , (6, dim), (6, aug), (6, sus4), (6, d7sus4)
            , (5, dim7), (5, m7b5)
            , (4, maj9), (4, m9), (4, d9)
            , (1, d7b5), (1, d7s5), (1, d7b9), (1, d7s9), (1, d7b5b9)
            ]
        , scales  =
            [ (10, ionian), (10, dorian), (10, phrygian), (10, lydian)
            , (10, mixolydian), (10, aeolian), (10, pentatonicMajor)
            , (10, locrian), (10, minor), (10, harmonicMinor)
            , (10, melodicMinor), (10, pentatonicMinor), (10, blues)
            , (5, bebopDominant), (5, bebopDorian), (5, bebopMajor)
            , (5, bebopMelodicMinor), (5, bebopHarmonicMinor)
            , (1, altered), (1, wholeTone), (1, halfDiminished), (1, flamenco)
            ]
        , octaves = [(1, Oct2), (5, Oct3), (10,Oct4), (5, Oct5), (1, Oct6)]
        }
  cp <- final (64 * wn)
  -- writeToMidiFile "cp.midi" cp
  -- putStrLn "Wrote to MIDI."
  playDev 4 (MIDIConfig (3%5) RhodesPiano) cp

  -- writeToLilypondFile "out.ly" small
  -- putStrLn (show $ musicToLilypond small)
  putStrLn "Playback finished."

small :: MusicCore
small = (C#4 <: []) <| (11%16)
