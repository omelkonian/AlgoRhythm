{-# LANGUAGE ParallelListComp #-}
module Main where

import           Data.Ratio ((%))
import           MIDI       (writeToMidiFile)
import           Music

main :: IO ()
main =
  print piece
  -- writeToMidiFile "out.midi" piece
  -- writeToScoreFile "score.ly" piece
  where
    piece = foldl1 (:+:) [ Note dur (pc:@:oct)
                         | pc  <- [C, Fs, C, F]
                         | oct <- [4, 4, 3, 3]
                         | dur <- [1%4, 1%8, 1%8, 1%2]
                         ]
