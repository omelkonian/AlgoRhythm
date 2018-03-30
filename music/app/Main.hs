{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import Export
import Grammar
import Music

main :: IO ()
main = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = Gs
        , baseOct = Oct3
        , baseScale = harmonicMinor
        , chords  =
            [ (10, maj), (10, mi)
            , (8, d7), (8, maj6), (8, m6)
            , (7, maj7), (7, m7)
            , (6, dim), (6, aug)
            -- , (6, sus4), (6, d7sus4)
            -- , (5, dim7), (5, m7b5)
            -- , (4, maj9), (4, m9), (4, d9)
            -- , (1, d7b5), (1, d7s5), (1, d7b9), (1, d7s9), (1, d7b5b9)
            ]
        }
  let ?melodyConfig = MelodyConfig
        { scales  =
            [ (10, ionian), (10, dorian), (10, phrygian), (10, lydian)
            , (10, mixolydian), (10, aeolian), (10, pentatonicMajor)
            , (10, locrian), (10, minor), (10, harmonicMinor)
            , (10, melodicMinor), (10, pentatonicMinor), (10, blues)
            -- , (5, bebopDominant), (5, bebopDorian), (5, bebopMajor)
            -- , (5, bebopMelodicMinor), (5, bebopHarmonicMinor)
            -- , (1, altered), (1, wholeTone), (1, halfDiminished), (1, flamenco)
            ]
        , octaves = [(1, Oct2), (5, Oct3), (15, Oct4), (5, Oct5), (1, Oct6)]
        }
  cp <- final (8 * wn)
  let midiConfig = MIDIConfig (4%5) RhodesPiano
  -- writeToMidiFile "cp.midi" midiConfig cp
  -- putStrLn "Wrote to MIDI."
  -- writeToLilypondFile "cp.ly" cp
  -- putStrLn "Wrote to Lilypond"
  playDev 4 midiConfig (2 ## cp)
  putStrLn "Playback finished"

  -- tab <- tablaTest
  -- writeToMidiFile "tablas.midi" defaultMIDIConfig tab
  -- putStrLn "Wrote to MIDI."

  -- writeToLilypondFile "out.ly" small
  -- putStrLn (show $ musicToLilypond small)

small :: MusicCore
small = (C#4 <: []) <| (11%16)
