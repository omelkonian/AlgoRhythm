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
  where
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

    -- generateSome :: MusicGenerator Melody
    -- generateSome = do
    --   chords <- replicateM 4 (genChord 5)
    --   notes  <- replicateM 10 (genNote)
    --   return ((line chords) :=: line notes)

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
