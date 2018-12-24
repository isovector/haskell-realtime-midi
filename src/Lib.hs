{-# LANGUAGE LambdaCase #-}

module Lib where

import Control.Monad.IO.Class
import qualified Data.Set as S (Set, insert, delete, null)
import Euterpea.Music
import Streaming
import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)


main :: IO ()
main = S.print . keysDown $ midiStream 20


keysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (S.Set Pitch)) m r
keysDown = S.scan add mempty id
  where
    add s (NoteOn  _ n _) = S.insert (pitch n) s
    add s (NoteOff _ n _) = S.delete (pitch n) s
    add s _               = s

