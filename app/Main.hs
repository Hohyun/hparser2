module Main (main) where

import Lib
import HotIbs (ibsHotParseTest)
import HotBsp
import Text.Megaparsec


main :: IO ()
main = ibsHotParseTest
