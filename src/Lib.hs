{-# LANGUAGE LambdaCase #-}

module Lib where

import qualified Data.Set as S (Set, insert, delete, null)
import Euterpea.Music
import Streaming
import qualified Streaming.Prelude as S
import Streaming.Prelude (Stream, Of)


main :: IO ()
main = runStream keysDown


keysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (S.Set Pitch)) m r
keysDown = S.scan add mempty id
  where
    add s (NoteOn  _ n _) = S.insert (pitch n) s
    add s (NoteOff _ n _) = S.delete (pitch n) s
    add s _               = s


chordsPlayed
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (S.Set Pitch)) m r
chordsPlayed = S.scan add mempty id
  where
    add s (NoteOn  _ n _) = S.insert (pitch n) s
    add s (NoteOff _ n _) = mempty
    add s _               = s

