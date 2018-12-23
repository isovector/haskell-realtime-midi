{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Streaming
  ( runStream
  , Message (..)
  ) where

import           Codec.Midi (Message (..))
import           Control.Monad
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming.Char8 as C8
import           Data.Foldable
import qualified Streaming.Prelude as S
import           System.IO
import           System.Process (createProcess, shell, CreateProcess (..), StdStream (..))


headerParse :: Parser ()
headerParse = void $ do
  _ <- A.string "Waiting for data. Press Ctrl+C to end.\n"
  _ <- A.string "Source"
  _ <- A.takeWhile (/= '\n')
  A.string "\n"


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


runStream :: Show b => (S.Stream (S.Of Message) IO () -> S.Stream (S.Of b) IO ()) -> IO ()
runStream f = do
  (_, Just pout, _, _) <-
    createProcess $
      (shell "aseqdump -p 20") { std_out = CreatePipe }
  hSetBinaryMode pout True

  S.print
    . f
    . void
    . A.parsed (eventParse)
    . C8.drop (fromIntegral . length @[] $ "Waiting for data. Press Ctrl+C to end.\nSource  Event                  Ch  Data\n")
    $ C8.hGetContents pout

