{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections    #-}
module Main where

import           Data.Ratio ((%))
import           MIDI       (writeToMidiFile, play, playDev)
import           Music
import           Score      (writeToLilypondFile)

main :: IO ()
main = do
  print piece'
  writeToMidiFile "out.midi" piece'
  playDev 4 piece'
  writeToLilypondFile "piece.ly" piece'
  writeToLilypondFile "scales.ly" (cIonian :+: repeatMusic 8 eBlues)
  where
    piece' = line $ map ($ piece) [id, (~| P4), id, (~| P5), (~| P4), id]
    piece = line [ Note dur (pc <@ oct, [Dynamics dyn])
                 | pc  <- [C, Fs, C, F]
                 | oct <- [4, 4, 3, 3]
                 | dur <- [1%4, 1%8, 1%8, 1%2]
                 | dyn <- [PPP,FFF,PPP,FFF]
                 ]

-- | Create a scale
scale :: Duration -> [(PitchClass, Int)] -> MusicCore
scale d = foldr1 (:+:) . map (Note d . (,[]) . uncurry (<@))

-- | Repeat a piece of music
repeatMusic :: Int -> Music a -> Music a
repeatMusic n m | n == 1    = m
                | otherwise = m :+: repeatMusic (n - 1) m

cIonian :: MusicCore
cIonian = scale (1 % 4) [(p, 4) | p <- [D, E, F, G, A, B, C]]
eBlues :: MusicCore
eBlues = scale (1 % 2)
  [
    (E, 4),
    (G, 4),
    (A, 4),
    (As, 4),
    (B, 4),
    (D, 5),
    (E, 5)
  ]
