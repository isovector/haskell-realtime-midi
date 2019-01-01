{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Lib where

import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Control.Monad.Trans.State (evalStateT)
import           Data.Bifunctor (first)
import           Data.Ratio ((%))
import           Data.Set (Set)
import qualified Data.Set as Set
import           Streaming.Prelude (Stream, Of)
import qualified Streaming.Prelude as S
import           StreamingMidi
import           Utils
import           Types


-- main :: IO ()
-- main = flip evalStateT (Dom7 C `Over` E)
--      . S.print
--      . merge (keysDown $ midiStream 20)
--      . S.filter ((== 1 % 4) . timeValue)
--      $ clockStream 60

-- main :: IO ()
-- main = flip evalStateT (Dom7 C `Over` E)
--      . S.mapM_ (liftIO . putStrLn . showChord)
--      . S.map (fmap canonicalize)
--      . S.mapMaybeM onChord
--      . keysDown
--      $ midiStream 20


-- onChord
--     :: MonadState (Chord PitchClass) m
--     => Set Pitch
--     -> m (Maybe (Chord PitchClass))
-- onChord ps = do
--   c <- get
--   case matches c ps of
--     True  -> do
--       let c' = fmap rotate5 c
--       put c'
--       pure $ Just c'
--     False -> pure Nothing



