module Type.Event where

import Type.Log (Log)

data Event = NewLog Log | FilteredLog Log | SearchEnded | Tick

instance Show Event where
  show = \case
    NewLog _ -> "NewLog"
    FilteredLog _ -> "FilteredLog"
    Tick -> "Tick"
    SearchEnded -> "SearchEnded"
