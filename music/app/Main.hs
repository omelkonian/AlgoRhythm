{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import           Export (playDev, writeToLilypondFile, writeToMidiFile)
import           Music

main :: IO ()
main = do
  writeToMidiFile "piece.midi" piece'
  -- writeToLilypondFile "piece.ly" piece'
  writeToMidiFile "scales.midi" scalePiece
  -- writeToLilypondFile "scales.ly" scalePiece
  playDev 4 piece'
  where
    cIonian, eBlues, scalePiece :: Melody
    scalePiece = cIonian :+: Rest wn :+: 8 ## eBlues
    cIonian = line $ (C#4)+|major <||(qn^^^)
    eBlues  = line $ (E#4)+|blues <||(qn^^^)
    piece, piece' :: Music FullPitch
    piece' = line $ map ($ piece) [id, (~> P4), id, (~> P5), (~> P4), id]
    piece = line [ pc # oct <: [Dynamic dyn, Articulation art] <| dur
                 | pc  <- [B, Fs, D, E]
                 | oct <- [4, 4, 3, 3]
                 | dur <- [(en^^^), (en^^^), (en^^^), (qn^.)]
                 | dyn <- [PPP,FFF,PPP,FFF]
                 | art <- [Staccatissimo,Tenuto,Marcato,Staccato]
                 ]
