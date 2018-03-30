{-# LANGUAGE ImplicitParams #-}
module Grammar.Integration
       ( final
       ) where

import           Grammar.Harmony
import           Grammar.Melody
import qualified Grammar.TonalHarmony as Tonal
import           Grammar.Types
import           Grammar.Dynamics
import qualified Grammar.UUHarmony    as UU
import           Grammar.VoiceLeading
import           Music

final :: (?melodyConfig :: MelodyConfig, ?harmonyConfig :: HarmonyConfig)
      => Duration -> IO MusicCore
final t = do
  harmonicStructure <- runGrammar harmony (I, t) ?harmonyConfig
  -- harmonicStructure <- runGrammar UU.harmony (UU.Piece, t) ?harmonyConfig
  -- harmonicStructure <- runGrammar Tonal.harmony (Tonal.Piece, t) ?harmonyConfig
  melodicStructure <- runGrammar melody (MQ, t) ()
  background <- voiceLead harmonicStructure
  foreground <- mkSolo harmonicStructure melodicStructure
  return $ (soften <$> toMusicCore background) :=:
           (addDynamics (toMusicCore foreground) t)
  where
    soften (p, _) = p <: [Dynamic PPP]
    soften' (p, _) = p <: [Dynamic P]
