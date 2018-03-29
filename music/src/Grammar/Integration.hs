{-# LANGUAGE ImplicitParams        #-}
module Grammar.Integration
       ( final
       ) where

import Grammar.Types
import Grammar.Harmony
import Grammar.VoiceLeading
import Grammar.Melody
import Music

final :: (?melodyConfig :: MelodyConfig, ?harmonyConfig :: HarmonyConfig)
      => Duration -> IO MusicCore
final t = do
  harmonicStructure <- runGrammar harmony (I, t) ?harmonyConfig
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=:
           (soften' <$> toMusicCore foreground)
  where
    soften (p, _) = p <: [Dynamic PPP]
    soften' (p, _) = p <: [Dynamic P]
