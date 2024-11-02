module Text where

import Data.Text qualified as Text

cleanupEscapes :: Text.Text -> Text.Text
cleanupEscapes = Text.map (\a -> if a `elem` ("\n\t\v\a" :: String) then ' ' else a)
