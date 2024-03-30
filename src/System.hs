module System where

import System.Info (os)

data System = Windows | Macos | Linux | Other

currentOs :: System
currentOs = case os of
  "darwin" -> Macos
  "mingw32" -> Windows
  "linux" -> Linux
  _ -> Other
