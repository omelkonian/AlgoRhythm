import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

import           TMidi                          (midiTests)
import           TMusic                         (musicTests)
import           TScore                         (scoreTests)

main = defaultMain [ musicTests
                   , scoreTests
                   , midiTests
                   ]
