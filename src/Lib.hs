{-# LANGUAGE LambdaCase #-}

module Lib where

import Streaming
import Euterpea.Music


main :: IO ()
main = runStream $
  \case
    NoteOn _ n _ -> Just $ pitch n
    _ -> Nothing


