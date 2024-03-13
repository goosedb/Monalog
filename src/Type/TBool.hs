{-# LANGUAGE PatternSynonyms #-}

module Type.TBool where

newtype TBool (symbol :: k) = TBool Bool
  deriving (Eq)

pattern Do :: TBool symbol
pattern Do = TBool True

pattern Don't :: TBool symbol
pattern Don't = TBool False

{-# COMPLETE Do, Don't #-}

pattern Is :: TBool symbol
pattern Is = TBool True

pattern Isn't :: TBool symbol
pattern Isn't = TBool False

{-# COMPLETE Is, Isn't #-}
