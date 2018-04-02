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

  -- | Denotes the global note density in a piece of music
  data Density = High | Medium | Low

  -- | Sample weights for note durations during a cerain density
  densityToDurations :: Density -> [(Weight, Duration)]
  densityToDurations High =
    [ (0.20, 1%32)
    , (0.35, 1%16)
    , (0.45, 1%8)
    , (0.10, 1%4)
    , (0.05, 1%2)
    ]
  densityToDurations Medium =
    [ (0.10, 1%16)
    , (0.10, 1%8)
    , (0.35, 1%4)
    , (0.20, 1%2)
    , (0.05, 1%1)
    ]
  densityToDurations Low =
    [ (0.10, 1%8)
    , (0.20, 1%4)
    , (0.20, 1%2)
    , (0.20, 1%1)
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

  semiChordWeights :: PitchClass -> SemiChord
                                 -> [(Weight, PitchClass)]
  semiChordWeights key chord =
      map (
      (\(a, b) -> (a, instantiate key b)) .
      (\pc ->
        relativeWeights!!(
          ((12 +
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
             zipWith (\(x1, x2) (y1, _) -> (x1 + y1, x2))
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
        (Just _)  -> (map (\(w, v) -> (getWeight v w, v)) xs)
        (Nothing) -> xs
    where idx = (elemIndex el (stripList xs))
          getWeight el' ow =
            ow * k^^(0 - abs((fromJust idx) -
              (fromJust (elemIndex el' (stripList xs )))))
          -- TODO include trends in distribution

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
                             -> MusicGenerator () MusicCore
  diatonicPhrase dur density key scale chord = do
    durations <- boundedRhythm dur density

    octaves <- genAspect octave 4
      (length durations) 2.0 [(0.6, 4), (0.4, 5)]

    pitches <- genAspect pitchClass key
      (length durations) 1.3
      (mergeWeights
        (intervalWeights key  scale)
        (semiChordWeights key chord))
    let fullPitches = ((flip (<:) $ []) <$> (zipWith (#) pitches octaves))
    return $ line
      (zipWith (<|) fullPitches durations)

  diatonicMelody :: Music SemiChord -> PitchClass
                                    -> [Interval]
                                    -> Duration
                                    -> MusicGenerator () MusicCore
  diatonicMelody chords key scale dur | otherwise =
    let timeline = chordalTimeline chords
      in f timeline dur
    where f tl dur | dur <= 0  = return (Rest 0)
          f tl dur | otherwise =
            do density <- lift phraseDensity
               len     <- lift $ phraseLength density
               pause   <- lift pauseLength
               phrase <- diatonicPhrase
                 len density key scale (fst $ head tl)
               r <- f (remainder tl (len + pause)) (dur - (len + pause))
               return $ phrase :+: (Rest pause) :+: r

                   where remainder []       _ = []
                         remainder [x]      _ = []
                         remainder (x:y:xs) d | d < snd y = (y:xs)
                                              | otherwise = remainder (y:xs) d

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

  pauseLength :: IO Duration
  pauseLength = do
    aux <- generate $ oneof
      (map (elements . (\x -> [x]))
        [1..8]
      )
    return $ aux * en

  phraseDensity :: IO Density
  phraseDensity = generate $ frequency
    [ (1, elements [High])
    , (5, elements [Medium])
    , (3, elements [Low])
    ]

  chordalTimeline :: Music SemiChord -> [(SemiChord, Duration)]
  chordalTimeline chords = getTimeline (toListM chords) 0

  getTimeline :: [(Maybe a, Duration)] -> Duration -> [(a, Duration)]
  getTimeline []     _ = []
  getTimeline ((x, y):xs) p =
    case x of
      Nothing  -> getTimeline xs (p + y)
      (Just v) -> (v, p):getTimeline xs (p + y)

  trimToLength :: Duration -> [Duration] -> [Duration]
  trimToLength d [] = []
  trimToLength d (x:xs) | d - x <= 0 = [d]
  trimToLength d (x:xs) | otherwise  = x:(trimToLength (d - x) xs)

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
    TODO: generate larger structures with higher level parameters, which may
    be instantiated to a concrete piece of music
  -}
