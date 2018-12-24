{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Monad.IO.Class
import           Data.List (minimumBy)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)
import           Euterpea.Music
import           Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude as S
import           StreamingMidi


data Chord
  = Maj  PitchClass
  | Min  PitchClass
  | Maj7 PitchClass
  | Min7 PitchClass
  | Dom7 PitchClass
  | Over Chord PitchClass
  deriving (Eq, Ord, Show)


matches :: Chord -> Set Pitch -> Bool
matches (Maj  r) ps = matchSemitones r [0, 4, 7] ps
matches (Maj7 r) ps = matchSemitones r [0, 4, 7, 11] ps
matches (Dom7 r) ps = matchSemitones r [0, 4, 7, 10] ps
matches (Min  r) ps = matchSemitones r [0, 3, 7] ps
matches (Min7 r) ps = matchSemitones r [0, 3, 7, 10] ps
matches (Over c b) ps =
  let bp  = minimumBy (comparing swap) ps
      ps' = Set.delete bp ps
   in fst bp == b && matches c ps'


matchSemitones :: PitchClass -> [Int] -> Set Pitch -> Bool
matchSemitones r ts ps = (== Set.map fst ps)
                       . Set.fromList
                       $ fmap (fst . flip trans (r, 4)) ts


main :: IO ()
main = S.print
     . S.filter (matches $ Maj7 D `Over` A)
     . S.filter (not . Set.null)
     . keysDown
     $ midiStream 20


keysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (Set Pitch)) m r
keysDown = S.scan (flip add) mempty id
  where
    add (NoteOn  _ n _) = Set.insert $ pitch n
    add (NoteOff _ n _) = Set.delete $ pitch n
    add _               = id

