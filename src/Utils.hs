{-# LANGUAGE DeriveFunctor #-}

module Utils
  ( module Utils
  , Cofree (..)
  , Identity (..)
  , coerce
  , Comonad (..)
  ) where

import           Control.Comonad
import           Control.Comonad.Cofree
import           Data.Coerce
import           Data.Functor.Identity
import           Data.List (minimumBy, uncons)
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)
import           Types
import qualified Streaming.Prelude as S
import           StreamingMidi


keysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (Set Pitch)) m r
keysDown = S.scan (flip add) mempty id
  where
    add (NoteOn  _ n _) = Set.insert $ pitch n
    add (NoteOff _ n _) = Set.delete $ pitch n
    add _               = id

consecutiveKeysDown
    :: Monad m
    => Stream (Of Message) m r
    -> Stream (Of (Set Pitch)) m r
consecutiveKeysDown = S.scan (flip add) mempty id
  where
    add (NoteOn  _ n _) = Set.insert $ pitch n
    add (NoteOff _ n _) = mempty
    add _               = id


majorPentatonic :: PitchClass -> Set PitchClass
majorPentatonic = Set.fromList . take 5 . iterate rotate5


circleOfFifths :: PitchClass -> [PitchClass]
circleOfFifths = take 12 . iterate rotate5


matchSemitones
    :: (Set PitchClass -> Set PitchClass -> Bool)
    ->  PitchClass
    -> [Int]
    -> Set Pitch
    -> Bool
matchSemitones f r ts ps
  = (f $ Set.map fst ps)
  . Set.fromList
  $ fmap (fst . flip trans (r, 4)) ts


getRoot :: Chord a -> a
getRoot (Maj  r)   = r
getRoot (Maj7 r)   = r
getRoot (Dom7 r)   = r
getRoot (Min  r)   = r
getRoot (Min7 r)   = r
getRoot (Over c b) = getRoot c


showChord :: Chord PitchClass -> String
showChord (Maj  r)   = showPitch r
showChord (Maj7 r)   = showPitch r ++ "M7"
showChord (Dom7 r)   = showPitch r ++ "7"
showChord (Min  r)   = showPitch r ++ "m"
showChord (Min7 r)   = showPitch r ++ "m7"
showChord (Over c b) = showChord c ++ "/" ++ showPitch b


rotate5 :: PitchClass -> PitchClass
rotate5 pc = fst $ trans 7 (pc, 4)


isCompleteChord :: Chord PitchClass -> Set Pitch -> Bool
isCompleteChord = matches (==)

isIncompleteChord :: Chord PitchClass -> Set Pitch -> Bool
isIncompleteChord = matches Set.isSubsetOf

getN :: Int -> Cofree Identity a -> [a]
getN 0 _         = []
getN n (a :< as) = a : getN (n - 1) (coerce as)


coiterCycle :: [a] -> Cofree Identity a
coiterCycle = unfold (coerce . fromJust . uncons) . cycle


matches
    :: (Set PitchClass -> Set PitchClass -> Bool)
    -> Chord PitchClass
    -> Set Pitch
    -> Bool
matches f (Maj  r) ps = matchSemitones f r [0, 4, 7] ps
matches f (Maj7 r) ps = matchSemitones f r [0, 4, 7, 11] ps
matches f (Dom7 r) ps = matchSemitones f r [0, 4, 7, 10] ps
matches f (Min  r) ps = matchSemitones f r [0, 3, 7] ps
matches f (Min7 r) ps = matchSemitones f r [0, 3, 7, 10] ps
matches f (Over c b) ps
  | Set.null ps = False
  | otherwise =
      let bp  = minimumBy (comparing swap) ps
          ps' = Set.delete bp ps  -- TODO: only delete if not in the chord
       in fst bp == b && matches f c ps'


showPitch :: PitchClass -> String
showPitch Ass = "A##"
showPitch As  = "A#"
showPitch A   = "A"
showPitch Af  = "Ab"
showPitch Aff = "Abb"
showPitch Bss = "B##"
showPitch Bs  = "B#"
showPitch B   = "B"
showPitch Bf  = "Bb"
showPitch Bff = "Bbb"
showPitch Css = "C##"
showPitch Cs  = "C#"
showPitch C   = "C"
showPitch Cf  = "Cb"
showPitch Cff = "Cbb"
showPitch Dss = "D##"
showPitch Ds  = "D#"
showPitch D   = "D"
showPitch Df  = "Db"
showPitch Dff = "Dbb"
showPitch Ess = "E##"
showPitch Es  = "E#"
showPitch E   = "E"
showPitch Ef  = "Eb"
showPitch Eff = "Ebb"
showPitch Fss = "F##"
showPitch Fs  = "F#"
showPitch F   = "F"
showPitch Ff  = "Fb"
showPitch Fff = "Fbb"
showPitch Gss = "G##"
showPitch Gs  = "G#"
showPitch G   = "G"
showPitch Gf  = "Gb"
showPitch Gff = "Gbb"

canonicalize :: PitchClass -> PitchClass
canonicalize As = Bf
canonicalize Cs = Df
canonicalize Ds = Ef
canonicalize Fs = Gf
canonicalize Gs = Af
canonicalize z  = z

