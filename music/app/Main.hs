{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PostfixOperators #-}

module Main where

import Export
import Grammar
import Music
import Generate
import Dynamics
import Generate.Applications.Diatonic

main :: IO ()
main = do
  let ?harmonyConfig = HarmonyConfig
        { basePc  = Ds
        , baseOct = Oct3
        , baseScale = major
        , chords  =
            [ (10, maj), (10, mi)
            , (8, d7)
            , (7, maj7), (7, m7)
            , (6, dim)
            -- , (6, sus4), (6, d7sus4)
            -- , (5, dim7), (5, m7b5)
            -- , (4, maj9), (4, m9), (4, d9)
            -- , (1, d7b5), (1, d7s5), (1, d7b9), (1, d7s9), (1, d7b5b9)
            ]
        }
  let ?midiConfig = MIDIConfig (6%5) [AcousticGrandPiano, AcousticGrandPiano]

  harmonicStructure <- runGrammar uuHarmony (64 * wn) ?harmonyConfig
  background <- voiceLead harmonicStructure

  solo <- runGenerator () $ diatonicMelody harmonicStructure Ds major (64 * wn)
  -- writeToMidiFile "cp.midi" midiConfig cp
  -- putStrLn "Wrote to MIDI."
  -- writeToLilypondFile "cp.ly" cp
  -- putStrLn "Wrote to Lilypond"
  writeToMidiFile "out.midi" (addDynamics $ solo :=: toMusicCore background)

  -- tab <- tablaTest
  -- writeToMidiFile "tablas.midi" defaultMIDIConfig tab
  -- putStrLn "Wrote to MIDI."

  -- writeToLilypondFile "out.ly" small
  -- putStrLn (show $ musicToLilypond small)

small :: MusicCore
small = (C#4 <: []) <| (11%16)
