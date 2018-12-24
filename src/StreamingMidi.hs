{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall  #-}

module StreamingMidi
  ( midiStream
  , timeValue
  , getBeat
  , Duration (..)
  , clock
  , Message (..)
  ) where

import           Codec.Midi (Message (..))
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming.Char8 as C8
import           Data.Foldable
import           Data.Ratio
import qualified Streaming.Prelude as S
import           System.Clock
import           System.IO
import           System.Process (createProcess, shell, CreateProcess (..), StdStream (..))


data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Dotted Duration
  deriving (Eq, Ord, Show)


timeValue :: Duration -> Rational
timeValue Whole      = 1
timeValue Half       = 1 % 2
timeValue Quarter    = 1 % 4
timeValue Eighth     = 1 % 8
timeValue Sixteenth  = 1 % 16
timeValue (Dotted d) = timeValue d * (3 % 2)


spaces :: Parser ()
spaces = void $ A.takeWhile (== ' ')


eventParse :: Parser Message
eventParse = do
  spaces
  _ <- A.takeWhile (/= ' ')
  spaces
  e <- asum
        [ A.string "Note on"  *> pure True
        , A.string "Note off" *> pure False
        ]
  spaces
  c <- A.decimal
  _ <- A.string ","
  spaces
  _ <- A.string "note"
  spaces
  n <- A.decimal
  _ <- A.string ","
  spaces
  _ <- A.string "velocity"
  spaces
  v <- A.decimal
  _ <- A.string "\n"
  pure $
    case e of
      True  -> NoteOn c n v
      False -> NoteOff c n v


unknownParse :: Parser (Maybe a)
unknownParse = do
  _ <- A.takeWhile (/= '\n')
  _ <- A.string "\n"
  pure Nothing


midiStream :: MonadIO m => Int -> S.Stream (S.Of Message) m ()
midiStream dev
  = void
  . S.mapMaybe id
  . A.parsed (fmap Just eventParse <|> unknownParse)
  . (C8.hGetContents =<<)
  . liftIO $ do
      (_, Just pout, _, _) <-
        createProcess $
          (shell $ "aseqdump -p " ++ show dev)
            { std_out = CreatePipe }
      hSetBinaryMode pout True
      pure pout


clock
    :: MonadIO m
    => S.Stream (S.Of a) m r
    -> S.Stream (S.Of (TimeSpec, a)) m r
clock s = do
  start <- liftIO $ getTime Monotonic
  flip S.mapM s $ \a -> do
    now <- liftIO $ getTime Monotonic
    let diff = diffTimeSpec now start
    pure (diff, a)


getBeat :: Integer -> TimeSpec -> Rational
getBeat bpm (TimeSpec s ns) =
  let time  = fromIntegral s + fromIntegral ns % 1000000000  -- in seconds
      beats = bpm % 60  -- in bpm
   in (round @Float . fromRational $ time * beats * 32) % 32


