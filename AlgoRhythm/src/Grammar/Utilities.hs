{-# LANGUAGE PostfixOperators #-}
module Grammar.Utilities where

import Control.Arrow (first)
import Music
import System.Random

-- Random helper functions.
(<|>) :: a -> a -> IO a
x <|> y = oneOf [x, y]

(<||>) :: IO a -> IO a -> IO a
x' <||> y' = do
  x <- x'
  y <- y'
  x <|> y

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
  if n <= w || null es
  then a
  else pick (n-w) es
pick _ _ = error "pick: empty list"

equally :: [a] -> [(Double, a)]
equally = zip (repeat 1.0)

normally :: [(Double, a)] -> [(Double, a)]
normally xs = first (/ sum (map fst xs)) <$> xs

-- Convertion from/to lists.
type ListMusic a = [(a, Duration)]

toList :: Music a -> ListMusic a
toList (m :+: m') = toList m ++ toList m'
toList(Note d a)  = [(a, d)]
toList (_ :=: _)  = error "toList: non-sequential music"
toList (Rest _)   = error "toList: rest exists"

fromList :: ListMusic a -> Music a
fromList = line . fmap (uncurry (<|))

type ListMusicM a = [(Maybe a, Duration)]

toListM :: Music a -> ListMusicM a
toListM (m :+: m') = toListM m ++ toListM m'
toListM (_ :=: _)  = error "toListM: non-sequential music"
toListM (Note d a) = [(Just a, d)]
toListM (Rest d)   = [(Nothing, d)]

fromListM :: ListMusicM a -> Music a
fromListM = line . fmap f
  where f (Just a, t) = a <| t
        f (Nothing, t) = (t~~)

-- Music distances
chordDistance :: Chord -> Chord -> Int
chordDistance c c' = sum $ uncurry pitchDistance <$> zip c c'

pitchDistance :: Pitch -> Pitch -> Int
pitchDistance p p' = abs $ fromEnum p - fromEnum p'

pitchDistanceM :: Maybe Pitch -> Pitch -> Int
pitchDistanceM Nothing  = const 1
pitchDistanceM (Just p) = pitchDistance p

distancePc :: PitchClass -> PitchClass -> Interval
distancePc pc pc' = toEnum $ abs $ fromEnum pc - fromEnum pc'
