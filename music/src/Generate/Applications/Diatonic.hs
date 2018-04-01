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

  -- | Denotes the global note density in a piece of music
  data Density = High | Medium | Low

  -- | Sample weights for note durations during a cerain density
  densityToDurations :: Density -> [(Weight, Duration)]
  densityToDurations High =
    [ (0.35, 1%16)
    , (0.45, 1%8)
    , (0.10, 1%4)
    , (0.05, 1%2)
    , (0.05, 1%1)
    ]
  densityToDurations Medium =
    [ (0.10, 1%16)
    , (0.35, 1%8)
    , (0.35, 1%4)
    , (0.10, 1%2)
    , (0.05, 1%1)
    ]
  densityToDurations Low =
    [ (0.10, 1%16)
    , (0.20, 1%8)
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
  instantiateWeights :: PitchClass -> [Interval]
                                   -> [(Weight, PitchClass)]
  instantiateWeights key scale =
    map (\(a, b) -> (a, instantiate key b)) $
      filter (\(a, b) -> b `elem` scale) relativeWeights

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
  getDistributions el k xs = (map (\(w, v) -> (getWeight v w, v)) xs)
    where strip            = map snd xs
          idx              = fromJust (elemIndex el strip)
          getWeight el' ow = ow * k^^(0 - abs(idx - (fromJust (elemIndex el' strip))))

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
  genAspect accessor initial n k options = do
    lift $ runGenerator initial $
      do putOptions accessor options
         putSelector accessor (beamSelector k accessor)
         replicateM n (accessor??)

  -- | Generate a diatonic melody. Strictly speaking, the generated
  --   melodies don't have to be diatonic, as any possible scale can be
  --   given to function as the generator's basis
  diatonicMelody :: PitchClass -> [Interval]
                               -> MusicGenerator () MusicCore
  diatonicMelody key scale = do
    octaves <- genAspect octave 4
      60 3.0 [(0.4, 3), (0.4, 4), (0.4, 5)]

    pitches <- genAspect pitchClass key
      60 1.75 (instantiateWeights key scale)

    durations <- concatM
      [ genAspect duration (1%4) 30  2.0 (densityToDurations Medium)
      , genAspect duration (1%4) 30 2.0 (densityToDurations High)
      ]

    addConstraint interval (`elem` [P1, P5, P8])
    finalPitch <- (interval??)
    let pitches' = pitches ++ [instantiate key finalPitch]
    let fullPitches = ((flip (<:) $ []) <$> (zipWith (#) pitches' octaves))
    return $ line (zipWith (<|) fullPitches durations)

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
