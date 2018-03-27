{-# LANGUAGE PostfixOperators #-}
module Grammar.Utilities where

import System.Random
import Music

-- Random helper functions.
oneOf :: [a] -> IO a
oneOf = choose . fmap (\a -> (1, a))

chooseWith :: (a -> Double) -> [a] -> IO a
chooseWith f = choose . fmap (\a -> (f a, a))

choose :: [(Double, a)] -> IO a
choose items = do
  let totalWeight = sum $ fst <$> items
  index <- getStdRandom $ randomR (0, totalWeight)
  return $ pick index items

pick :: Double -> [(Double, a)] -> a
pick n ((w, a):es) =
  if n <= w then
    a
  else
    pick (n-w) es
pick _ _ = error "pick: empty list"

-- Convertion from/to lists.
type ListMusic a = [(a, Duration)]

toList :: Music a -> ListMusic a
toList (m :+: m') = toList m ++ toList m'
toList(Note d a)  = [(a, d)]
toList (_ :=: _)  = error "toList: non-sequential music"
toList (Rest _)   = error "toList: rest exists"

fromList :: ListMusic a -> Music a
fromList ((a,t):ms) = a <| t :+: fromList ms
fromList []         = (0~~)

type ListMusicM a = [(Maybe a, Duration)]

toListM :: Music a -> ListMusicM a
toListM (m :+: m') = toListM m ++ toListM m'
toListM (_ :=: _)  = error "toListM: non-sequential music"
toListM (Note d a) = [(Just a, d)]
toListM (Rest d)   = [(Nothing, d)]

fromListM :: ListMusicM a -> Music a
fromListM ((Just a,t):ms)  = a <| t :+: fromListM ms
fromListM ((Nothing,t):ms) = (t~~) :+: fromListM ms
fromListM []               = (0~~)

-- Music distances
chordDistance :: Chord -> Chord -> Int
chordDistance c c' = sum $ uncurry pitchDistance <$> zip c c'

chordDistanceI :: Chord -> Chord -> Interval
chordDistanceI c = toEnum . chordDistance c

pitchDistance :: Pitch -> Pitch -> Int
pitchDistance p p' = abs $ fromEnum p - fromEnum p'

pitchDistanceI :: Pitch -> Pitch -> Interval
pitchDistanceI p = toEnum . pitchDistance p

distancePc :: PitchClass -> PitchClass -> Interval
distancePc pc pc' = pitchDistanceI (pc#oct) (pc#(oct + offset))
  where oct = Oct4
        offset | pc > pc'  = 1
               | otherwise = 0
