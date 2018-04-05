{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImplicitParams #-}
module Grammar.Tabla
       ( tabla
       ) where

import Grammar.Types
import Grammar.Utilities
import Music

-- | Raw MIDI representation.
newtype MidiNumber = MidiNumber Int
instance ToMusicCore MidiNumber where
  toMusicCore = toMusicCore . fmap (\(MidiNumber n) -> toEnum (n - 12) :: Pitch)

-- | Tabla music.
data TablaNote =
  -- terminals
  Tr | Kt | Dhee | Tee | Dha | Ta | Ti | Ge | Ke | Na | Ra | Noop
  -- non-terminals
  | Start | S | XI | XD | XJ | XA | XB | XG | XH | XC | XE| XF
  | TA7 | TC2 | TE1 | TF1 | TF4 | TD1 | TB2 | TE4 | TC1 | TB3 | TA8 | TA3 | TB1 | TA1
  deriving (Eq, Show)

instance ToMusicCore TablaNote where
  toMusicCore = toMusicCore . fromListM . concatMap percussionMap . toList
    where percussionMap :: (TablaNote, Duration) -> [(Maybe MidiNumber, Duration)]
          percussionMap (tableNote, t) =
            (\n -> (n, t)) <$> (if null xs then [Nothing] else Just <$> xs)
            where xs = MidiNumber <$> case tableNote of
                    Tr   -> [38, 39]
                    Kt   -> [45, 40]
                    Dhee -> [50] -- dhin
                    Tee  -> [38] -- ti
                    Dha  -> [46]
                    Ta   -> [40]
                    Ti   -> [38]
                    Ge   -> [44] -- ga
                    Ke   -> [45] -- ka
                    Na   -> [52] -- tin
                    Ra   -> [39]
                    Noop -> []
                    _    -> error "Incomplete grammar rewrite"

(|-->) :: (?tablaBeat :: Duration) => a -> [a] -> Rule meta a
x |--> xs = (x, 1, always) |-> foldl1 (:-:) (map (:%: ?tablaBeat) xs)

-- | Grammar for tabla improvisation based on the paper:
-- "Modelling Improvisatory and Compositional Processes" by Bernard Bel.
tabla :: (?tablaBeat :: Duration) => Grammar () TablaNote
tabla = Start |:
  [ (Start, 1, always) :-> \t ->
      foldr1 (:-:) $ replicate (t // (16 * ?tablaBeat)) $ S:%:def
  , S |--> [TE1, XI]
  , XI |--> [TA7, XD]
  , XD |--> [TA8]
  , XI |--> [TF1, XJ]
  , XJ |--> [TC2, XA]
  , XA |--> [TA1, XB]
  , XB |--> [TB3, XD]
  , XI |--> [TF1, XG]
  , XG |--> [TB2, XA]
  , S |--> [TA1, XH]
  , XH |--> [TF4, XB]
  , XH |--> [TA3, XC]
  , XC |--> [TE4, XD]
  , XC |--> [TA3, XE]
  , XE |--> [TA1, XD]
  , XE |--> [TC1, XD]
  , XC |--> [TB1, XB]
  , S |--> [TB1, XF]
  , XF |--> [TA1, XJ]
  , XF |--> [TD1, XG]

  , TA7 |--> [Kt, Dha, Tr, Kt, Dha, Ge, Na]
  , TC2 |--> [Tr, Kt]
  , TE1 |--> [Tr]
  , TF1 |--> [Kt]
  , TF4 |--> [Ti, Dha, Tr, Kt]
  , TD1 |--> [Noop]
  , TB2 |--> [Dha, Ti]
  , TE4 |--> [Ti, Noop, Dha, Ti]
  , TC1 |--> [Ge]
  , TB3 |--> [Dha, Tr, Kt]
  , TA8 |--> [Dha, Ti, Dha, Ge, Dhee, Na, Ge, Na]
  , TA3 |--> [Tr, Kt, Dha]
  , TB1 |--> [Ti]
  , TA1 |--> [Dha]
  ]
