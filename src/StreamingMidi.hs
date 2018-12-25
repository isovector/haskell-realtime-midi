{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall            #-}

module StreamingMidi
  ( midiStream
  , clockStream
  , merge
  , timeValue
  , getBeat
  , beatToDuration
  , Duration (..)
  , clock
  , Message (..)
  ) where

import           Codec.Midi (Message (..))
import           Control.Applicative
import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming.Char8 as C8
import           Data.Foldable
import           Data.Ratio
import           Streaming (Stream, Of)
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


clockStream :: MonadIO m => Integer -> S.Stream (S.Of Duration) m ()
clockStream bpm = S.delay dur
                $ S.map beatToDuration $ S.iterate (+ 1 % 16) 0
  where
    dur = fromRational $ bpm % (60 * 8)


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
  let dq = ns `div` 1000000                            -- in ms
      time  = fromIntegral s + fromIntegral dq % 1000  -- in seconds
      beats = bpm % 60                                 -- in bps
      result = time * beats
      den    = denominator result
   in case den  <= 16 of
        True  -> result
        False ->
          let r = fromIntegral den / 16
           in round (fromIntegral @_ @Double (numerator result) / r) % 16


beatToDuration :: Rational -> Duration
beatToDuration z =
  case denominator z of
    1  -> Whole
    2  -> Half
    4  -> Quarter
    8  -> Eighth
    16 -> Sixteenth
    _  -> error "i hope this doesn't happen"


merge
    :: forall m a b. MonadIO m
    => Stream (Of a) IO ()
    -> Stream (Of b) IO ()
    -> Stream (Of (Either a b)) m ()
merge sa sb = do
  mvar <- liftIO $ do
    mvar <- newEmptyMVar
    void . forkIO $ S.mapM_ (putMVar mvar . Left) sa
    void . forkIO $ S.mapM_ (putMVar mvar . Right) sb
    pure mvar

  S.repeatM . liftIO $ takeMVar mvar

