{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module Exercises.Chords where

import           Control.Comonad.Cofree (unwrap)
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State.Class
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import           Data.Generics.Product
import           Data.List (inits)
import           Data.Set (Set)
import           GHC.Generics
import           Streaming (hoist, distribute)
import qualified Streaming.Prelude as S
import           StreamingMidi
import           Types
import           Utils


data Output
  = Experience Int
  | Message String
  | Error
  deriving (Eq, Ord, Show)


data Exercise s i = Exercise
  { exName :: String
  , exState :: s
  , exStream
      :: forall m
       . MonadIO m
      => i
      -> Stream (Of Output) (StateT s m) ()
  }


data ExerciseState s i = ExerciseState
  { esCurrent :: Exercise s i
  , esLater   :: [Exercise s i]
  , esExpToGo :: Int
  , esState   :: s
  } deriving Generic


difficulty :: Int
difficulty = 5


runExercises
    :: MonadIO m
    => [Exercise s i]
    -> Stream (Of i) m ()
    -> Stream (Of Output) m ()
runExercises (ex : exs) s = do
  let estate = ExerciseState ex exs difficulty $ exState ex
  flip evalStateT estate
    . distribute
    . S.mapM ( \e -> do
          case e of
            Experience i -> do
              modify $ field @"esExpToGo" -~ i
            z -> pure ()
          es <- get
          let next = head $ esLater es
          when (esExpToGo es <= 0)
            . put
            . ExerciseState next
                            (tail $ esLater es)
                            difficulty
            $ exState next
          pure e
      )
    . flip S.for ( \i -> do
          ex <- gets esCurrent
          hoist (zoom $ field @"esState") $ exStream ex i
      )
    . hoist lift
    $ s


genExercises
    :: Chord PitchClass
    -> [Exercise (Cofree Identity (Chord PitchClass))
                 (Set Pitch)
       ]
genExercises ch0 = do
  ch <- tail
      . fmap reverse
      . inits
      . fmap (<$ ch0)
      . circleOfFifths
      $ getRoot ch0
  id
    [ mkStreamFunc ("just " ++ show (head ch)) $ take 1 ch
    , mkStreamFunc ("all "  ++ show ch) ch
    ]


mkStreamFunc
    :: String
    -> [Chord PitchClass]
    -> Exercise (Cofree Identity (Chord PitchClass)) (Set Pitch)
mkStreamFunc name chords =
  Exercise name (coiterCycle chords) $ \ns -> do
    ch <- gets extract
    case (isIncompleteChord ch ns, isCompleteChord ch ns) of
      (False, _) -> do
        S.yield $ Experience $ -1
        S.yield Error
      (_, True) -> do
        S.yield $ Experience 1
        modify $ runIdentity . unwrap
        gets (show . extract) >>= S.yield . Message
      _ -> pure ()


main :: IO ()
main = do
  S.print
   . runExercises (genExercises $ Maj C)
   . consecutiveKeysDown
   $ midiStream 20

