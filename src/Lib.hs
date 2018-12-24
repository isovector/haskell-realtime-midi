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


showChord :: Chord PitchClass -> String
showChord (Maj  r)   = showPitch r
showChord (Maj7 r)   = showPitch r ++ "M7"
showChord (Dom7 r)   = showPitch r ++ "7"
showChord (Min  r)   = showPitch r ++ "m"
showChord (Min7 r)   = showPitch r ++ "m7"
showChord (Over c b) = showChord c ++ "/" ++ showPitch b


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


matchSemitones
    :: PitchClass
    -> [Int]
    -> Set Pitch
    -> Bool
matchSemitones r ts ps
  = (== Set.map fst ps)
  . Set.fromList
  $ fmap (fst . flip trans (r, 4)) ts


main :: IO ()
main = flip evalStateT (Min C)
     . S.mapM_ (liftIO . putStrLn . showChord)
     . S.map (fmap canonicalize)
     . S.mapMaybeM onChord
     . keysDown
     $ midiStream 20


onChord
    :: MonadState (Chord PitchClass) m
    => Set Pitch
    -> m (Maybe (Chord PitchClass))
onChord ps = do
  c <- get
  case matches c ps of
    True  -> do
      let c' = fmap rotate5 c
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

