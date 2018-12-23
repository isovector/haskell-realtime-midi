{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Lib where

import qualified Streaming.Prelude as S
import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Streaming as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Streaming.Char8 as C8
import           Data.Foldable
import           Data.Word (Word8)
import           System.Process (createProcess, shell, CreateProcess (..), StdStream (..))
import System.IO


ord :: Char -> Char
ord = id


headerParse :: Parser ()
headerParse = void $ do
  A.string "Waiting for data. Press Ctrl+C to end.\n"
  A.string "Source"
  A.takeWhile (/= ord '\n')
  A.string "\n"

spaces :: Parser ()
spaces = void $ A.takeWhile (== ord ' ')


data Event = Event
  { evSource  :: ByteString
  , evEvent   :: Bool
  , evChannel :: Int
  , evNote    :: Int
  , evVel     :: Int
  }
  deriving (Eq, Ord, Show)


eventParse :: Parser Event
eventParse = do
  spaces
  s <- A.takeWhile (/= ord ' ')
  spaces
  e <- asum
        [ A.string "Note on"  *> pure True
        , A.string "Note off" *> pure False
        ]
  spaces
  c <- A.decimal
  A.string ","
  spaces
  A.string "note"
  spaces
  n <- A.decimal
  A.string ","
  spaces
  A.string "velocity"
  spaces
  v <- A.decimal
  A.string "\n"
  pure $ Event s e c n v

-- Waiting for data. Press Ctrl+C to end.
-- Source  Event                  Ch  Data
--  20:0   Note on                 0, note 82, velocity 76


main :: IO ()
main = do
  (_, Just pout, _, _) <- createProcess $ (shell "aseqdump -p 20") { std_out = CreatePipe }

  hSetBinaryMode pout True

  S.print
    $ void
    $ A.parsed (eventParse)
    $ C8.drop (fromIntegral . length @[] $ "Waiting for data. Press Ctrl+C to end.\nSource  Event                  Ch  Data\n")
    $ C8.hGetContents pout

