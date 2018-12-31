{-# LANGUAGE DeriveFunctor #-}

module Types
  ( module Types
  , PitchClass (..)
  , Pitch
  , AbsPitch
  , trans
  , pitch
  ) where

import Euterpea.Music


data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Dotted Duration
  deriving (Eq, Ord, Show)


data Chord a
  = Maj  a
  | Min  a
  | Maj7 a
  | Min7 a
  | Dom7 a
  | Over (Chord a) a
  deriving (Eq, Ord, Show, Functor)

