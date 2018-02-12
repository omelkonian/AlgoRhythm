module Lib where

import System.Random
import Control.DeepSeq

import Euterpea hiding (play)
import Euterpea.IO.MIDI.MidiIO (unsafeOutputID)

-- play = playDev 4
playX :: (NFData a, ToMusic1 a) => Music a -> IO ()
playX = playC defParams{perfAlg = eventMerge . perform, devID = Just (unsafeOutputID 4)} where
   eventMerge :: Performance -> Performance
   eventMerge (e1:e2:es) =
       let e1' = e1{eDur = eTime e2 + eDur e2 - eTime e1}
       in  if ePitch e1 == ePitch e2 then eventMerge (e1':es)
           else e1 : eventMerge (e2:es)
   eventMerge e = e

someFunc :: IO ()
someFunc = playX x4'

simpleMel :: Music Pitch
simpleMel = line [c 4 qn, c 4 qn, g 4 qn, g 4 qn, a 4 qn, a 4 qn, g 4 hn]

randInts :: Int -> [Int]
randInts seed = recInts (mkStdGen seed)
  where recInts g = let (i,g') = next g in i : recInts g

randIntsRange :: (Int, Int) -> Int -> [Int]
randIntsRange (lower, upper) =
  map (\i -> (i `mod` (upper-lower)) + lower) . randInts

melGen :: Int -> Music (Pitch, Volume)
melGen s =
   let pitches = map pitch $ randIntsRange (30,80) s
       vols = randIntsRange (40,100) (s+1)
   in  line $ map (note qn) $ zip pitches vols

somethingWeird =
  let part1 = instrument Xylophone $ dim $ rit $ cut 6 $ melGen 345
      part2 = instrument Marimba $ cut 4 $ melGen 234
      part3 = instrument TubularBells $ cre $ acc $ cut 8 $ melGen 789
  in  chord [part1, part2, part3]
  where
    rit = Modify (Phrase [Tmp $ Ritardando 0.5])
    acc = Modify (Phrase [Tmp $ Accelerando 0.5])
    dim = Modify (Phrase [Dyn $ Diminuendo 0.5])
    cre = Modify (Phrase [Dyn $ Crescendo 0.5])

x1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
x2 = x1 :+: transpose 3 x1
x3 = x2 :+: x2 :+: invert x2 :+: retro x2
x4 = forever x3 :=: forever (tempo (2/3) x3)
x4' = forever x3 :=: (rest hn :+: forever (tempo (2/3) x3))

exportMi = writeMidi "test.midi" simpleMel
