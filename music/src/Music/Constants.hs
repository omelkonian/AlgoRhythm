module Music.Constants
       ( -- * Durations
         -- * Chords
         maj, mi, dim, aug, maj7, min7, dom7, dim7
         -- * Scales
       , ionian, aeolian, major, minor, harmonicMinor, pentatonicMinor, blues
       ) where

import           Music.Types

------------------------------- TODO Durations ---------------------------------


------------------------------------ Chords ------------------------------------
maj, mi, dim, aug, maj7, min7, dom7, dim7 :: ChordType
-- Triads
maj = [P1, M3, P5]
mi  = [P1, Mi3, P5]
dim = [P1, Mi3, A4]
aug = [P1, M3, Mi6]
-- 7ths
maj7 = [P1, M3, P5, M7]
min7 = [P1, Mi3, P5, Mi7]
dom7 = [P1, M3, P5, Mi7]
dim7 = [P1, Mi3, A4, M6]
-- TODO 9ths/11ths/13ths/adds/sus

------------------------------------ Scales ------------------------------------
ionian, aeolian, major, minor, harmonicMinor, pentatonicMinor, blues :: ScaleType
major = [P1, M2, M3, P4, P5, M6, M7]
ionian = major
minor = [P1, M2, Mi3, P4, P5, Mi6, Mi7]
aeolian = minor
harmonicMinor = [P1, M2, Mi3, P4, P5, Mi6, M7]
pentatonicMinor = [P1, Mi3, P4, P5, Mi7]
blues = [P1, Mi3, P4, A4, P5, Mi7]
-- TODO More scale types
