{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE PostfixOperators #-}

module Generate.Applications.Diatonic where

  import Generate.Generate
  import Generate.QuickCheck
  import Music
  import Data.Ratio
  import Data.List
  import Data.Maybe
  import qualified Control.Arrow as Arrow
  import Control.Monad
  import Control.Monad.State
  import Debug.Trace
  import Grammar.Utilities
  import Test.QuickCheck
  import Test.QuickCheck.Gen
  import Generate.Applications.GenConfig


  -- | Sample weights for note durations during a cerain density
  densityToDurations :: Density -> [(Weight, Duration)]
  -- High density phrases
  densityToDurations High =
    [ (0.05, 1%32)
    , (0.15, 1%16)
    , (0.55, 1%8)
    , (0.30, 1%4)
    , (0.05, 1%2)
    ]
  -- Medium density phrases
  densityToDurations Medium =
    [ (0.02, 1%16)
    , (0.05, 1%8)
    , (0.55, 1%4)
    , (0.30, 1%2)
    , (0.05, 1%1)
    ]
  -- Low density phrases
  densityToDurations Low =
    [ (0.10, 1%8)
    , (0.40, 1%4)
    , (0.40, 1%2)
    , (0.10, 1%1)
    ]

  -- | Weights table containing the relative 'importance' of all
  --   possible intervals
  relativeWeights :: [(Weight, Interval)]
  relativeWeights = [ (10.0, P1)
                    , (0.50, Mi2)
                    , (2.50, M2)
                    , (8.00, Mi3)
                    , (8.00, M3)
                    , (5.00, P4)
                    , (1.00, A4)
                    , (9.00, P5)
                    , (1.00, Mi6)
                    , (4.00, M6)
                    , (4.00, Mi7)
                    , (4.00, M7)
                    , (10.0, P8)
                    , (1.00, Mi9)
                    , (2.50, M9)
                    , (8.00, A9)
                    , (8.00, M10)
                    , (5.00, P11)
                    , (1.00, A11)
                    , (9.00, P12)
                    , (1.00, Mi13)
                    , (4.00, M13)
                    , (4.00, Mi14)
                    , (4.00, M14)
                    , (10.0, P15)
                    ]

  -- | Get the relative note 'importance' from a certain scale using
  --   the global weights table
  intervalWeights :: PitchClass -> [Interval]
                                -> [(Weight, PitchClass)]
  intervalWeights key scale =
    map (\(a, b) -> (a, instantiate key b)) $
      filter (\(a, b) -> b `elem` scale) relativeWeights

  -- Convert a SemiChord to a list representing the relative
  -- importance of each note in the key the chord is played in.
  semiChordWeights :: PitchClass -> SemiChord
                                 -> [(Weight, PitchClass)]
  semiChordWeights key chord =
      map (
      (\(a, b) -> (a, instantiate key b)) .
      (\pc ->
        relativeWeights!!(
          ((12 +
            -- Find relative weights in the given key
            -- for the pitchclasses in the provided chord
            ((fromEnum ([C .. B]!!(fromEnum pc))) -
            (fromEnum ([C .. B]!!(fromEnum key)))))
          `mod` 12)
          )
      )) chord

  -- Merge two weight lists by taking the union, and adding the weights
  -- for all elements that are common to both lists.
  mergeWeights :: (Eq a) => [(Weight, a)] -> [(Weight, a)] -> [(Weight, a)]
  mergeWeights xs ys =
      let xs' = normalize xs
        in let ys' = normalize ys
          in normalize $
             -- xs / ys
             (filter
               ((not . (flip elem) (stripList ys)) . snd) xs'
             ) ++
             -- ys / xs
             (filter
               ((not . (flip elem) (stripList xs)) . snd) ys'
             ) ++
             -- ys /\ ys, with weights summed
             zipWith (\(x1, x2) (y1, _) -> ((x1 + y1) / 2, x2))
               (filter ((flip elem) intersection . snd) xs')
               (filter ((flip elem) intersection . snd) ys')
    where -- Normalize a distribution such that all weights sum to 1
          normalize xs =
            let k = (sum . map fst) xs
              in map (\(x, v) -> (x / k, v)) xs
          -- Calculate the set of intersecting elements.
          intersection = intersect (stripList xs) (stripList ys)

  -- | Constraint that requires all generated notes to be in a certain scale
  inScale :: PitchClass -> [Interval]
                        -> Constraint PitchClass
  inScale key scale = (flip elem) (instantiate key scale :: [PitchClass])

  -- | Note selector that generates a distribution based on the last
  --   note that was generated
  beamSelector :: (Eq a, Enum a) => Double
                                 -> Accessor st s a
                                 -> Selector a a
  beamSelector k _ s xs = do
    (el, _) <- quickCheckSelector s (getDistributions s k xs)
    return (el, el)

  -- Retrieve weights relative to a certain value for all possible
  -- values of a certain aspect
  getDistributions :: (Eq a, Enum a) => a
                                     -> Double
                                     -> [(Weight, a)]
                                     -> [(Weight, a)]
  getDistributions el k xs =
      case idx of
        -- Check if the given element is in fact
        -- an element of the given list
        (Just _)  -> (map (\(w, v) -> (getWeight v w, v)) xs)
        (Nothing) -> xs
    where idx = (elemIndex el (stripList xs))
          -- A the weight for an element is related to the distance
          -- between that element and the previously generated element
          -- by a negative exponential distribution
          getWeight el' ow | el == el' = ow * 0.5
          getWeight el' ow | otherwise =
            ow * k^^(0 - abs((fromJust idx) -
              (fromJust (elemIndex el' (stripList xs )))))
          -- TODO include trends in distribution

  -- Strip a weighted list to it's elements
  stripList :: [(Weight, a)] -> [a]
  stripList = map snd

  -- Generate a sequence of values for a certain aspect using the
  -- 'beamed selector'.
  -- n denotes the number of values to be generated, options denotes the list
  -- of options from which the beamed selector should choose, and k is the width
  -- of the beam, where the probability distribution is roughly denoted by
  -- (k^distance between center of beam and value)
  genAspect :: (Eq a, Enum a) => Accessor GenState a a
                              -> a
                              -> Int
                              -> Double
                              -> [(Weight, a)]
                              -> MusicGenerator () [a]
  genAspect accessor initial n k options = trace (show n) $ do
    lift $ runGenerator initial $
      do putOptions accessor options
         putSelector accessor (beamSelector k accessor)
         replicateM n (accessor??)

  -- | Generate a diatonic phrase. Strictly speaking, the generated
  --   melodies don't have to be diatonic, as any possible scale can be
  --   given to function as the generator's basis
  diatonicPhrase :: Duration -> Density
                             -> PitchClass
                             -> [Interval]
                             -> SemiChord
                             -> [(Int, Octave)]
                             -> MusicGenerator () MusicCore
  diatonicPhrase dur density key scale chord octD = do
    durations <- boundedRhythm dur density

    octaves <- genAspect octave 4
      (length durations) 2.0
        (map (Arrow.first fromIntegral) octD)

    pitches <- genAspect pitchClass key
      (length durations) 1.3
      (mergeWeights
        (intervalWeights key  scale)
        (semiChordWeights key chord))
    let fullPitches = ((flip (<:) $ []) <$> (zipWith (#) pitches octaves))
    return $ line
      (zipWith (<|) fullPitches durations)

  -- | Generate a diatonic melody over a given chord progression. This is done by
  --   generating separate phrases that are linked together with a rest in
  --   between. The phraseses are aware of the chord they are over, so that they
  --   will use notes from the current chord with a higher probability.
  diatonicMelody :: GenConfig -> MusicGenerator () MusicCore
  diatonicMelody config=
    let timeline = chordalTimeline (chords config)
      in f timeline 0
    where f [] pos = return $  Rest 0
          f tl pos =
            do density <- lift (fromDistribution (phraseDistribution config))
               len     <- lift $ phraseLength density
               pause   <- lift pauseLength
               phrase <- diatonicPhrase
                 len density
                 (key config)
                 (baseScale config)
                 (fst $ head tl)
                 (octaveDistribution config)
               r <- f (remainder tl (pos + len + pause)) (pos + len + pause)
               return $ phrase :+: (Rest pause) :+: r
                   where remainder []       _ = []
                         remainder [x]      _ = []
                         remainder (x:y:xs) p | p < snd y = (y:xs)
                                              | otherwise = remainder (y:xs) p

  -- | Generate a (random) length for a phrase. A higher density will result in
  --   phrases with more notes allowed, in order to enforce that the average
  --   high density phrase will take roughly the same amount of time as the
  --   average low density phrase.
  phraseLength :: Density -> IO Duration
  phraseLength density = do
    aux <- generate $ oneof
      (map (elements . (\x -> [x]))
        [2..maxLen]
      )
    return $ aux * qn
      where maxLen =
              case density of
                Low    -> 8
                Medium -> 16
                High   -> 32

  -- | Choose a random rest length
  pauseLength :: IO Duration
  pauseLength = do
    aux <- generate $ oneof
      (map (elements . (\x -> [x]))
        [1..8]
      )
    return $ aux * en

  -- | Generate an element from a distribution
  fromDistribution :: [(Int, a)] -> IO a
  fromDistribution dist = do
    sample <- generate $ frequency
      (map (\(x, y) -> (x, elements [y])) dist)
    return sample

  -- | Convert a sequential piece of music to a timeline, containing pairs of
  --   all musical elements in the piece with the point in time they occur on
  chordalTimeline :: Music SemiChord -> [(SemiChord, Duration)]
  chordalTimeline chords = getTimeline (toListM chords) 0

  -- | Convert a list of musical elements and durations to a list
  --   of all elements and the absolute point in time they occur on.
  getTimeline :: [(Maybe a, Duration)] -> Duration -> [(a, Duration)]
  getTimeline []     _ = []
  getTimeline ((x, y):xs) p =
    case x of
      Nothing  -> getTimeline xs (p + y)
      (Just v) -> (v, p):getTimeline xs (p + y)

  -- | Trim a generated rhythm sequence to a certain length.
  trimToLength :: Duration -> [Duration] -> [Duration]
  trimToLength d [] = []
  trimToLength d (x:xs) | d - x <= 0 = [d]
  trimToLength d (x:xs) | otherwise  = x:(trimToLength (d - x) xs)

  -- | Generate a rythm piece with a maximum length.
  boundedRhythm :: Duration -> Density -> MusicGenerator () [Duration]
  boundedRhythm bound density = do
    dur <- (duration??)
    rhythm <- genAspect duration
      dur (round (bound / qn)) 2.0 (densityToDurations density)
    return $ trimToLength bound rhythm

  -- | Concatenates the result of a list of monadic computations that
  --   all yield a list themselves
  concatM :: (Monad m) => [m [a]] -> m [a]
  concatM [] = return []
  concatM (x:xs) = do
    v  <- x
    vs <- concatM xs
    return (v ++ vs)
  {-
    TODO: Chord generation
    TODO: pitch attributes
    TODO: time-awareness
    be instantiated to a concrete piece of music
  -}
