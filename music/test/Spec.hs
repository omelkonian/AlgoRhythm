import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TMusic
import TScore

main = defaultMain [ musicTests, scoreTests ]
