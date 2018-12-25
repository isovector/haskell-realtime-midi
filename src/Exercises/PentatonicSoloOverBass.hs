{-# LANGUAGE FlexibleContexts #-}

module Exercises.PentatonicSoloOverBass where

import Data.Tuple (swap)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.State (evalStateT)
import           Data.Bifunctor
import           Data.Set (Set)
import qualified Data.Set as Set
import           Euterpea.Music
import qualified Streaming.Prelude as S
import           StreamingMidi
import           Utils

main :: IO ()
main = flip evalStateT (False, cycle [Maj C, Min A, Maj F, Maj G])
     . S.mapM_ (liftIO . putStrLn)
     . S.mapMaybeM onChord
     . merge ( S.filter (\z -> z == Whole || z == Quarter || z == Half)
             $ clockStream 60
             )
     $ liftIO (putStrLn "C") >> midiStream 20


onChord
    :: MonadState (Bool, [Chord PitchClass]) m
    => Either Duration Message
    -> m (Maybe String)
onChord (Left Whole) = do
  (f, t) <- get
  modify $ first not
  case f of
    True -> do
      modify $ second tail
      pure $ Just $ showChord $ head $ tail t
    False -> pure $ Just "."
onChord (Left _) = pure $ Just "."

onChord (Right (NoteOn _ n _)) = do
  r <- gets $ getRoot . head . snd

  let p@(pc, o) = pitch n
  case (n <= absPitch (E, 3), canonicalize pc == canonicalize r) of
    (True, True) -> pure Nothing
    (True, False) -> pure Nothing -- pure $ Just "BAD ROOT NOTE"
    (False, _) ->
      case Set.member pc $ majorPentatonic C of
        True  -> pure Nothing
        False -> pure $ Just "BAD PENTATONIC"

onChord _ = pure Nothing


-- onChord ps = do
--   ( <- get
--   case matches c ps of
--     True  -> do
--       let c' = fmap rotate5 c
--       put c'
--       pure $ Just c'
--     False -> pure Nothing
