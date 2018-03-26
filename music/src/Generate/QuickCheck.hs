module Generate.QuickCheck where

import Music
import Generate.Generate
import Test.QuickCheck.Gen --(generate, frequency, elements)
import Export

quickCheckSelector :: Selector s
quickCheckSelector s as =
  let conv (x, a) = ((round . (*) 100) x, elements [a]) in
    generate $ frequency (map conv as) >>= \a -> return (a,s)

quickCheckEntry :: (Enum a, Bounded a) => s -> Entry s a
quickCheckEntry _ = Entry { values      = zip (repeat 1) [minBound ..]
                          , constraints = []
                          , selector    = quickCheckSelector
                          }

quickCheckState :: s -> GenState s
quickCheckState st = GenState { state = st
                              , pc    = quickCheckEntry st
                              , oct   = quickCheckEntry st
                              , dur   = Entry { values      =
                                                  zip (repeat 1) [1%1,1%2,1%4,1%8,1%16]
                                              , constraints = []
                                              , selector    = quickCheckSelector
                                              }
                              , itv   = quickCheckEntry st
                              , dyn   = quickCheckEntry st
                              , art   = quickCheckEntry st
                              }

-- | Runs a generator on the quickCheck state.
runGenerator :: s -> MusicGenerator s a -> IO a
runGenerator = runGenerator' . quickCheckState

clean :: s -> MusicGenerator s a -> MusicGenerator s a
clean s = modified (const $ quickCheckState s)

playGen :: ToMusicCore a => s -> MusicGenerator s (Music a) -> IO ()
playGen s music = do
  m <- runGenerator s music
  playDev 4 defaultMIDIConfig m
