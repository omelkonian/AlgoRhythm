import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit

import           TMidi                          (midiTests)
import           TMusic                         (musicTests)
import           TScore                         (scoreTests)
import           TVec                           (vecTests)

main = defaultMain [ musicTests
                   , scoreTests
                   , midiTests
                   , vecTests
                   ]
