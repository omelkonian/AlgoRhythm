import Test.Framework                 (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TGrammar  (grammarTests)
import TMidi     (midiTests)
import TMusic    (musicTests)
import TScore    (scoreTests)
import TVec      (vecTests)
import TGenerate (genTests)
import TChaos    (chaosTests)

main = defaultMain [ musicTests
                   , scoreTests
                   , midiTests
                   , vecTests
                   , grammarTests
                   , genTests
                   , chaosTests
                   ]
