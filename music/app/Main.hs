{-# LANGUAGE ParallelListComp #-}
module Main where

import           Export (play, playDev, writeToLilypondFile, writeToMidiFile)
import           Music

main :: IO ()
main = do
  print piece'
  writeToMidiFile "out.midi" piece'
  playDev 4 piece'
  writeToLilypondFile "piece.ly" piece'
  writeToLilypondFile "scales.ly" (cIonian :+: Rest 1 :+: 8 ## eBlues)
  where
    cIonian, eBlues :: Melody
    cIonian = line $ C#4+|ionian <|| 1%8
    eBlues  = line $ E#4+|blues  <|| 1%16
    piece, piece' :: Music FullPitch
    piece' = line $ map ($ piece) [id, (~> P4), id, (~> P5), (~> P4), id]
    piece = line [ pc # oct <: [Dynamic dyn, Articulation art] <| dur
                 | pc  <- [B, Fs, D, E]
                 | oct <- [4, 4, 3, 3]
                 | dur <- [1%4, 1%8, 1%8, 1%2]
                 | dyn <- [PPP,FFF,PPP,FFF]
                 | art <- [Staccatissimo,Tenuto,Marcato,Staccato]
                 ]
