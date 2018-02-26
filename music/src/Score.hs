module Score (writeToScoreFile) where

import Control.Arrow ((>>>))
import qualified Data.Music.Lilypond as Ly
import Data.Music.Lilypond.IO (writeMusic)
import Music

-- | Write `Music` to score.
writeToScoreFile :: (ToMusicCore a) => FilePath -> Music a -> IO ()
writeToScoreFile path = toMusicCore >>> musicToLilypond >>> writeMusic path

-- | Convert `MusicCore` to `Data.Music.Lilypond.Music`.
musicToLilypond :: MusicCore -> Ly.Music
musicToLilypond = undefined
