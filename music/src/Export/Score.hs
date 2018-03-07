module Export.Score (writeToLilypondFile) where

import           Control.Arrow                ((>>>))
import           Data.Maybe
import qualified Data.Music.Lilypond          as Ly
import qualified Data.Music.Lilypond.Dynamics as LyD
import           Data.Music.Lilypond.IO       (writeMusic)
import           Music
import           System.IO
import           Text.Pretty


-- | Write 'Music' to Lilypond file.
writeToLilypondFile :: (ToMusicCore a) => FilePath -> Music a -> IO ()
writeToLilypondFile path = musicToLilypondString >>> writeFile path
  where musicToLilypondString =
          toMusicCore >>> musicToLilypond >>> pretty >>> runPrinter

-- | Convert `MusicCore` to `Data.Music.Lilypond.Music`.
musicToLilypond :: MusicCore -> Ly.Music
musicToLilypond (m :+: m') =
  Ly.sequential (musicToLilypond m) (musicToLilypond m')
musicToLilypond (m :=: m') =
  Ly.simultaneous (musicToLilypond m) (musicToLilypond m')
musicToLilypond (Note d m) =
  Ly.Note (Ly.NotePitch (toLilypondPitch m) Nothing)
    (Just $ toDuration d) (getPostModifiers m)
musicToLilypond (Rest d  ) = Ly.Rest (Just $ toDuration d) []

-- | Convert a 'FullPitch' to it's corresponding
--   'Data.Music.Lilypond.Pitch'
toLilypondPitch :: FullPitch -> Ly.Pitch
toLilypondPitch ((p, oc), _) =
  Ly.Pitch { Ly.getPitch = (toName p, getAccidental p, fromEnum $ oc + 1) }

-- | Convert a 'Rational' to it's corresponding
--   'Data.Music.Lilypond.Duration'
toDuration :: Rational -> Ly.Duration
toDuration ratio = Ly.Duration { Ly.getDuration = ratio }

-- | Convert the 'PitchAttribute' list of a 'FullPitch' to
--   list of 'Data.Music.Lilypond.PostEvent' representing
--   the same dynamics and articulation
getPostModifiers :: FullPitch -> [Ly.PostEvent]
getPostModifiers (_, xs) = map attrToPost xs

-- | Convert 'PitchClass' it's corresponding 'Data.Music.Lilypond.PitchName'
toName :: PitchClass -> Ly.PitchName
toName pc = findMatch pc nameMap
  where nameMap =
          [ ([C, Cs], Ly.C)
          , ([D, Ds], Ly.D)
          , ([E], Ly.E)
          , ([F, Fs], Ly.F)
          , ([G, Gs], Ly.G)
          , ([A, As], Ly.A)
          , ([B], Ly.B)
          ]

-- | Get the 'Data.Music.Lilypond.Accidental' for a 'PitchClass'
getAccidental :: PitchClass -> Ly.Accidental
getAccidental pc = findMatch pc accMap
  where accMap =
          [ ([C, D, E, F, G, A, B], 0)
          , ([Cs, Ds, Fs, Gs, As], 1)
          ]

-- | Convert a 'PitchAttribute' to it's corresponding
--   'Data.Music.Lilypond.PostEvent'
attrToPost :: PitchAttribute -> Ly.PostEvent
attrToPost (Dynamic d)      = Ly.Dynamics Ly.Default (toLilyPondDynamics d)
attrToPost (Articulation a) = Ly.Articulation Ly.Default (toLilyPondArticulation a)

toLilyPondArticulation :: Articulation -> Ly.Articulation
toLilyPondArticulation a = fromJust $ lookup a m
  where m = [
              (Staccato, Ly.Staccato),
              (Staccatissimo, Ly.Staccatissimo),
              (Marcato, Ly.Marcato),
              (Tenuto, Ly.Tenuto)
            ]

toLilyPondDynamics :: Dynamic -> LyD.Dynamics
toLilyPondDynamics d = fromJust $ lookup d m
  where m = [
              (PPPPP, LyD.PPPPP),
              (PPPP, LyD.PPPP),
              (PPP, LyD.PPP),
              (PP, LyD.PP),
              (P, LyD.P),
              (MP, LyD.MP),
              (MF, LyD.MF),
              (F_, LyD.F),
              (FF, LyD.FF),
              (FFF, LyD.FFF),
              (FFFF, LyD.FFFF)
            ]

-- | Find a match in a structure which maps list of keys to elements
findMatch :: Eq a => a -> [([a], b)] -> b
findMatch el = snd . head . filter (elem el. fst)
