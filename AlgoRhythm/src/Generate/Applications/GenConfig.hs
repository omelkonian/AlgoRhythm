-- | Used by the Diatonic generator to steer the generation process
module Generate.Applications.GenConfig (
    GenConfig (..)
  , Density (..)
  , defaultGenConfig
) where

import Music


-- | Denotes the global note density in a piece of music
data Density = High | Medium | Low

data GenConfig = GenConfig { key                :: PitchClass
                           , baseScale          :: [Interval]
                           , chords             :: Music SemiChord
                           , phraseDistribution :: [(Int, Density)]
                           , octaveDistribution :: [(Int, Octave)]
                           }

defaultGenConfig :: GenConfig
defaultGenConfig = undefined
