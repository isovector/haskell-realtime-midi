{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( module Types
  , PitchClass (..)
  , Pitch
  , AbsPitch
  , trans
  , pitch
  , module Streaming
  ) where

import Euterpea.Music
import Streaming (Stream (..), Of (..))


newtype Difficulty = Difficulty
  { getDifficulty :: Int
  } deriving (Eq, Ord, Num, Show, Enum, Bounded)


data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Dotted Duration
  deriving (Eq, Ord, Show)


data Inversion
  = InvRoot
  | InvFirst
  | InvSecond
  deriving (Eq, Ord, Show, Enum, Bounded)


data Chord a
  = Maj  a
  | Min  a
  | Maj7 a
  | Min7 a
  | Dom7 a
  | Over (Chord a) a
  deriving (Eq, Ord, Show, Functor)

