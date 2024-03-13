module Type.MaxWidth where

newtype MaxWidth = MaxWidth {rawMaxWidth :: Int}
  deriving (Eq, Ord, Show)
