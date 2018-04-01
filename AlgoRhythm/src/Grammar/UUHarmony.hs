{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.UUHarmony
       ( uuHarmony
       ) where

import qualified Grammar.Harmony   as H
import           Grammar.Types
import           Music

data Degree =
  -- terminals
  I | II | III | IV | V | VI | VII
  -- non-terminals
  | Piece | Phrase | Tonic | Dominant | SubDominant
  deriving (Eq, Show, Enum, Bounded)

uuHarmony :: Grammar H.Modulation Degree
uuHarmony = Piece |:
  [ (Piece, 1, always) :-> \t -> foldr1 (:-:) $ replicate (t // (4 * wn)) $ Phrase%:(4 * wn)

  , (Phrase, 1, always) :-> \t -> Tonic%:t/2 :-: Dominant%:t/4 :-: Tonic%:t/2
  , (Phrase, 1, always) :-> \t -> Dominant%:t/2 :-: Tonic%:t/2

  , (Phrase, 1, always) :-> \t -> H.Modulation P5 $: Phrase%:t

  , (Tonic, 1, (> wn)) :-> \t -> Let (Tonic%:t/2) (\x -> x :-: x)
  , (Tonic, 1, (<= wn)) :-> (I %:)

  , (Dominant, 3, (>= wn)) :-> \t -> SubDominant%:t/2 :-: Dominant%:t/2
  , (Dominant, 1, (<= wn)) :-> (V %:)
  , (Dominant, 1, (<= wn)) :-> (VII %:)
  , (Dominant, 1, (<= wn)) :-> \t -> II%:t/2 :-: V%:t/2

  , (SubDominant, 3, (> hn)) :-> \t -> Let (SubDominant%:t/2) (\x -> x :-: x)
  , (SubDominant, 1, (<= hn)) :-> (II %:)
  , (SubDominant, 1, (<= hn)) :-> (IV %:)
  , (SubDominant, 1, (<= wn)) :-> \t -> III%:t/2 :-: IV%:t/2
  ]

-- | Expands modulations and intreprets degrees to chords.
instance Expand H.HarmonyConfig Degree H.Modulation SemiChord where
  expand conf = expand conf . fmap ((toEnum :: Int -> H.Degree) . fromEnum)
