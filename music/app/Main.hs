{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import           Export (playDev, writeToLilypondFile, writeToMidiFile)
import           Music
import           Music.Generate
import           Control.Monad

main :: IO ()
main = do
  x <- runGenerator () example
  writeToMidiFile "gen.midi" x
  writeToLilypondFile "gen.ly" x

example :: MusicGenerator () Melody
example = do
  addConstraint pitchClass (`elem` [C, D, E, F, G, A, B])
  addConstraint octave     (`elem` [4,5])
  addConstraint duration   (`elem` [1%4,1%8,1%16])
  notes <- replicateM 7 genNote
  let melody   = line notes
  let melody'  = melody ~> P8 :=: melody
  let melody'' = melody <~ P8 :=: melody :=: melody'
  return $ melody :+: (melody :=: melody') :+: (melody :=: melody' :=: melody'')
