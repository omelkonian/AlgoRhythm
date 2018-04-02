{-# LANGUAGE ImplicitParams #-}
module Grammar.Integration
       ( integrate
       ) where

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
  harmonicStructure <- runGrammar tonalHarmony t ?harmonyConfig
  melodicStructure <- runGrammar melody t ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return (dyn $ toMusicCore background, dyn $ toMusicCore foreground)
