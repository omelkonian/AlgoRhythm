{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.Harmony2
       ( harmony2, Degree2 (..)
       ) where

import Data.Ratio (denominator, numerator)

import qualified Grammar.Harmony   as H
import           Grammar.Types
import           Music

data Degree2 =
  -- terminals
  I | II | III | IV | V | VI | VII
  -- non-terminals
  | Piece | Phrase | Tonic | Dominant | SubDominant
  deriving (Eq, Show, Enum, Bounded)

harmony2 :: Grammar H.Modulation Degree2
harmony2 =
  let bars4 t = foldl1 (:-:)
              $ replicate (fromInteger $ quot (numerator t) (denominator t * 4))
              $ Phrase%:(4 * wn)
  in
  [ (Piece, 1, always) :-> \t -> bars4 t

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

toDegree :: Degree2 -> H.Degree
toDegree d = case d of
  I   -> H.I
  II  -> H.II
  III -> H.III
  IV  -> H.IV
  V   -> H.V
  VI  -> H.VI
  VII -> H.VII
  e   -> error $ "toDegree: incomplete grammar rewrite [" ++ show e ++ "]"

-- | Expands modulations and intreprets degrees to chords.
instance Expand H.HarmonyConfig Degree2 H.Modulation SemiChord where
  expand conf (m :-: m') = (:-:) <$> expand conf m <*> expand conf m'
  expand conf (Let x f)  = f <$> expand conf x
  expand conf (Aux _ (H.Modulation itv) t) =
    expand (conf {H.basePc = H.basePc conf ~~> itv}) t
  expand conf (Prim (a, t)) = do
    ch <- conf `H.interpret` toDegree a
    return $ Prim (ch, t)
