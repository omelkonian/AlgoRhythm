{-# LANGUAGE PostfixOperators #-}
module Music.Constants where

import           Data.List       (insert)

import           Music.Operators
import           Music.Types
import           Music.Utilities

-- Roman numbers.
i, ii, iii, iv, v, vi, vii :: Int
[i, ii, iii, iv, v, vi, vii] = [1..7]

---------------------------------- Durations -----------------------------------

-- Basic.
wn, hn, qn, en, sn, tn :: Duration
wn = 1 ; hn = 1%2 ; qn = 1%4 ; en = 1%8 ; sn = 1%16 ; tn = 1%32

-- Triplets.
(^^^), tripl :: Duration -> Duration
(^^^) d = 2*d / 3
tripl = (^^^)

-- Dotted.
(^.), dot :: Duration -> Duration
(^.) d = d + d/2
dot = (^.)

------------------------------------ Chords ------------------------------------
maj, mi, dim, aug, sus4, d7sus4, maj6, m6, maj7, m7, d7, dim7, m7b5 :: AbstractChord
maj9, m9, d9, d7b5, d7s5, d7b9, d7s9, d7b5b9, d7b5s9, d7s5b9, d7s5s9 :: AbstractChord
-- Triads
maj = [P1, M3, P5]
mi  = [P1, Mi3, P5]
dim = [P1, Mi3, A4]
aug = [P1, M3, Mi6]
-- sus
sus4 = [P1, P4, P5]
d7sus4 = [P1, P4, P5, Mi7]
-- 6ths
maj6 = [P1, M3, P5, M6]
m6 = [P1, Mi3, P5, M6]
-- 7ths
maj7 = [P1, M3, P5, M7]
m7 = [P1, Mi3, P5, Mi7]
d7 = [P1, M3, P5, Mi7]
dim7 = [P1, Mi3, A4, M6]
m7b5 = [P1, Mi3, A4, Mi7]
-- 9ths
maj9 = [P1, M3, P5, M7, M9]
m9 = [P1, Mi3, P5, Mi7, M9]
d9 = [P1, M3, P5, Mi7, M9]
-- Altered Dominants
d7b5 = [P1, M3, A4, Mi7]
d7s5 = [P1, M3, Mi6, Mi7]
d7b9 = [P1, M3, P5, Mi7, Mi9]
d7s9 = [P1, M3, P5, Mi7, A9]
d7b5b9 = [P1, M3, A4, Mi7, Mi9]
d7b5s9 = [P1, M3, A4, Mi7, A9]
d7s5b9 = [P1, M3, Mi6, Mi7, Mi9]
d7s5s9 = [P1, M3, Mi6, Mi7, A9]

------------------------------------ Scales ------------------------------------
major, pentatonicMajor, ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: AbstractScale
minor, harmonicMinor, melodicMinor, pentatonicMinor, blues :: AbstractScale
bebopDominant, bebopDorian, bebopMajor, bebopMelodicMinor, bebopHarmonicMinor :: AbstractScale
altered, wholeTone, halfDiminished, flamenco :: AbstractScale

-- Major scales.
major = [P1, M2, M3, P4, P5, M6, M7]
pentatonicMajor = [P1, M2, M3, P5, M6]
ionian = mode i major
dorian = mode ii major
phrygian = mode iii major
lydian = mode iv major
mixolydian = mode v major
aeolian = mode vi major
locrian = mode vii major

-- Minor scales.
minor = [P1, M2, Mi3, P4, P5, Mi6, Mi7]
harmonicMinor = [P1, M2, Mi3, P4, P5, Mi6, M7]
melodicMinor = [P1, M2, Mi3, P4, P5, M6, M7]
pentatonicMinor = [P1, Mi3, P4, P5, Mi7]
blues = [P1, Mi3, P4, A4, P5, Mi7]

-- Bebop scales.
bebopDominant = insert M7 mixolydian
bebopDorian = mode v bebopDominant
bebopMajor = insert Mi6 major
bebopMelodicMinor = insert Mi6 melodicMinor
bebopHarmonicMinor = mode vi bebopMelodicMinor

-- Other scales.
altered = [P1, Mi2, Mi3, M3, A4, Mi6, Mi7]
wholeTone = [P1, M2, M3, A4, Mi6, Mi7]
halfDiminished = mode vi melodicMinor
flamenco = [P1, Mi2, M3, P4, P5, Mi6, M7]
