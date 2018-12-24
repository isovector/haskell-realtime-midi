{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Lib where

import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.State (evalStateT)
import           Data.List (minimumBy)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)
import           Euterpea.Music
import           Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude as S
import           StreamingMidi


data Chord a
  = Maj  a
  | Min  a
  | Maj7 a
  | Min7 a
  | Dom7 a
  | Over (Chord a) a
  deriving (Eq, Ord, Show, Functor)


rotate5 :: PitchClass -> PitchClass
rotate5 pc = fst $ trans 7 (pc, 4)


matches :: Chord PitchClass -> Set Pitch -> Bool
matches (Maj  r) ps = matchSemitones r [0, 4, 7] ps
matches (Maj7 r) ps = matchSemitones r [0, 4, 7, 11] ps
matches (Dom7 r) ps = matchSemitones r [0, 4, 7, 10] ps
matches (Min  r) ps = matchSemitones r [0, 3, 7] ps
matches (Min7 r) ps = matchSemitones r [0, 3, 7, 10] ps
matches (Over c b) ps
  | Set.null ps = False
  | otherwise =
      let bp  = minimumBy (comparing swap) ps
          ps' = Set.delete bp ps
       in fst bp == b && matches c ps'


matchSemitones :: PitchClass -> [Int] -> Set Pitch -> Bool
matchSemitones r ts ps = (== Set.map fst ps)
                       . Set.fromList
                       $ fmap (fst . flip trans (r, 4)) ts


main :: IO ()
main = flip evalStateT (Min C)
     . S.print
     . S.mapMaybeM onChord
     . keysDown
     $ midiStream 20


onChord :: MonadState (Chord PitchClass) m => Set Pitch -> m (Maybe (Chord PitchClass))
onChord ps = do
  c <- get
  case matches c ps of
    True  -> do
      let c' = fmap (canonicalize . rotate5) c
      put c'
      pure $ Just c'
    False -> pure Nothing


canonicalize :: PitchClass -> PitchClass
canonicalize As = Bf
canonicalize Cs = Df
canonicalize Ds = Ef
canonicalize Fs = Gf
canonicalize Gs = Af
canonicalize z  = z


keysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (Set Pitch)) m r
keysDown = S.scan (flip add) mempty id
  where
    add (NoteOn  _ n _) = Set.insert $ pitch n
    add (NoteOff _ n _) = Set.delete $ pitch n
    add _               = id

