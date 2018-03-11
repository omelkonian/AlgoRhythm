{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PostfixOperators #-}
module Main where

import           Export (playDev, writeToLilypondFile, writeToMidiFile)
import           Music

main :: IO ()
main = do
  -- writeToMidiFile "piece.midi" piece'
  -- writeToLilypondFile "piece.ly" piece'
  -- play piece'

  -- writeToMidiFile "scales.midi" scalePiece
  -- writeToLilypondFile "scales.ly" scalePiece
  -- play scalePiece

  -- writeToLilypondFile "bluesChanges.ly" cBluesProgression
  -- play cBluesProgression

  writeToLilypondFile "blues.ly" cBluesImprov
  play $ 2 *~ cBluesImprov -- double time
  where
    play = playDev 4

    cIonian, eBlues, scalePiece :: Melody
    scalePiece = cIonian :+: Rest wn :+: (8 ## eBlues)
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

    bluesProgression :: Pitch -> Harmony
    bluesProgression p =
      let tonic = p=|d7 <| wn
      in  line $ map ($ tonic) [ id     , id     , id, id
                               , (~> P4), (~> P4), id, id
                               , (~> P5), (~> P5), id, id
                               ]
    cBlues :: Harmony
    cBlues = 4 ## bluesProgression (C#3)

    cBluesImprov :: Melody
    cBluesImprov = chords cBlues :=: flatten (improviseOverD7 <$> cBlues) ~> P8

    improviseOverD7 :: Chord -> Melody
    improviseOverD7 (a:b:c:d:_) =
      a <| qn :+: (b <~ Mi2) <| en :+: b <| en :+:
        (c <| qn :=: d <| qn) :+: (en~~) :+: d <| en
