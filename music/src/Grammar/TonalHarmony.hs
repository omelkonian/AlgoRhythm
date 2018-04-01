{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Grammar.TonalHarmony
       ( tonalHarmony
       ) where

import qualified Grammar.Harmony as H
import           Grammar.Types
import           Music

data Degree =
  -- terminals
  I | II | III | IV | V | VI | VII
  -- non-terminals
  | Piece | TR | DR | SR | TS | DS | SS
  deriving (Eq, Show, Enum, Bounded)

(|~>) :: Head [a] -> (a -> Body meta a) -> [Rule meta a]
(xs, w, activ) |~> k = [(x, w, activ) :-> k x | x <- xs]

tonalHarmony :: Grammar H.Modulation Degree
tonalHarmony = Piece |:
  [ -- Phrase level
    (Piece, 1, always) :-> \t -> foldr1 (:-:) $ replicate (t // (4 * wn)) $ TR%:(4 * wn)

    -- Functional level: Expansion
  , (TR, 1, (> wn)) :-> \t -> TR%:t/2 :-: DR%:t/2
  , (TR, 1, always) :-> \t -> DR%:t/2 :-: TS%:t/2
  , (DR, 1, always) :-> \t -> SR%:t/2 :-: DS%:t/2
  ] ++
  (([TR, SR, DR], 1, (> wn)) |~> \x t -> x%:t/2 :-: x%:t/2) ++
  [
    (TR, 1, always) :-> (TS %:)
  , (DR, 1, always) :-> (DS %:)
  , (SR, 1, always) :-> (SS %:)

    -- Functional level: Modulation
  , (DS, 1, (>= qn)) :-> \t -> H.Modulation P5 $: DS%:t
  , (SS, 1, (>= qn)) :-> \t -> H.Modulation P4 $: SS%:t

    -- Scale-degree level: Secondary dominants
  ] ++
  (([TS, DS, SS], 1, (>= hn)) |~> \x t -> (H.Modulation P5 $: x%:t/2) :-: x%:t/2) ++
  [ -- Scale-degree level: Functional-Scale interface
    (TS, 1, (>= wn)) :-> \t -> I%:t/3 :-: IV%:t/3 :-: I%:t/3
  , (TS, 1, always) :-> (I %:)
  , (SS, 1, always) :-> (IV %:)
  , (DS, 1, always) :-> (V %:)
  , (DS, 1, always) :-> (VI %:)
  ]

-- | Expands modulations and intreprets degrees to chords.
instance Expand H.HarmonyConfig Degree H.Modulation SemiChord where
  expand conf = expand conf . fmap ((toEnum :: Int -> H.Degree) . fromEnum)
