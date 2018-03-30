{-# LANGUAGE ImplicitParams #-}
module Grammar.Integration
       ( final
       ) where

import           Grammar.Harmony
import           Grammar.Melody
import qualified Grammar.TonalHarmony as Tonal
import           Grammar.Types
import qualified Grammar.UUHarmony    as UU
import           Grammar.VoiceLeading
import           Music

final :: (?melodyConfig :: MelodyConfig, ?harmonyConfig :: HarmonyConfig)
      => Duration -> IO MusicCore
final t = do
  -- harmonicStructure <- runGrammar harmony (I, t) ?harmonyConfig
  harmonicStructure <- runGrammar UU.harmony2 (UU.Piece, t) ?harmonyConfig
  -- harmonicStructure <- runGrammar Tonal.harmony3 (Tonal.Piece, t) ?harmonyConfig
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=:
           (soften' <$> toMusicCore foreground)
  where
    soften (p, _) = p <: [Dynamic PPP]
    soften' (p, _) = p <: [Dynamic P]
