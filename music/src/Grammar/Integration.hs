{-# LANGUAGE ImplicitParams        #-}
module Grammar.Integration
       ( final
       ) where

import Grammar.Types
import Grammar.Harmony
import Grammar.VoiceLeading
import Grammar.Melody
import Grammar.Harmony2 as H2
import Music

final :: (?melodyConfig :: MelodyConfig, ?harmonyConfig :: HarmonyConfig)
      => Duration -> IO MusicCore
final t = do
  -- harmonicStructure <- test t
  harmonicStructure <- runGrammar H2.harmony2 (H2.I, t) ?harmonyConfig
  -- harmonicStructure <- runGrammar harmony (I, t) ?harmonyConfig
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=:
           (soften' <$> toMusicCore foreground)
  where
    soften (p, _) = p <: [Dynamic PPP]
    soften' (p, _) = p <: [Dynamic P]
