{-# LANGUAGE ImplicitParams #-}
module Grammar.Integration
       ( integrate
       ) where

import Control.Monad (when)

import Dynamics
import Grammar.Harmony
import Grammar.Melody
import Grammar.TonalHarmony
import Grammar.Types
import Grammar.VoiceLeading
import Music

integrate :: (?melodyConfig :: MelodyConfig, ?harmonyConfig :: HarmonyConfig)
          => Duration -> IO (MusicCore, MusicCore)
integrate t = do
  when (t < 4 * wn) $
    fail "integrate: requested duration should be at least 4 bars of music"
  harmonicStructure <- runGrammar tonalHarmony t ?harmonyConfig
  melodicStructure <- runGrammar melody t ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return (dyn $ toMusicCore background, dyn $ toMusicCore foreground)
