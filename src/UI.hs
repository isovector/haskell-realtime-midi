{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module UI where

import Control.Concurrent (forkIO)
import           Brick.AttrMap
import           Brick.BChan
import           Brick.Main
import           Brick.Types
import           Brick.Util (fg, bg, on, clamp)
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.Core
import           Brick.Widgets.ProgressBar
import           Control.Comonad.Cofree (Cofree (..), unwrap, unfold)
import           Control.Lens hiding ((:<))
import           Control.Monad (void)
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import           Data.Generics.Product
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import           Data.Ratio
import           GHC.Generics (Generic)
import           Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
import qualified Streaming.Prelude as S
import           StreamingMidi
import           Types
import           Utils


theBaseAttr :: AttrName
theBaseAttr = attrName "theBase"


xDoneAttr, xToDoAttr :: AttrName
xDoneAttr = theBaseAttr <> attrName "X:done"
xToDoAttr = theBaseAttr <> attrName "X:remaining"


bar :: Chord PitchClass -> Float -> Widget n
bar c p = vBox
  [ center $ str $ showChord c
  , hBorder
  , vLimit 10 $ vBox
      [ hBox
          . intersperse (fill ' ')
          . ( ++ [str "  "])
          . replicate 4
          . vCenter
          $ str " o"
      , hBorder
      , center
          . updateAttrMap
            ( mapAttrNames
              $ pure
                (xDoneAttr, progressCompleteAttr))
          $ progressBar Nothing p
      ]
  ]


data ChordAppState = CAS
  { casBeat   :: Int
  , casChords :: Cofree Identity (Chord PitchClass)
  } deriving (Eq, Ord, Show, Generic)


data ChordAppEvent = TickBeat
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)



getN :: Int -> Cofree Identity a -> [a]
getN 0 _         = []
getN n (a :< as) = a : getN (n - 1) (coerce as)


coiterCycle :: [a] -> Cofree Identity a
coiterCycle = unfold (coerce . fromJust . uncons) . cycle


stateToBars :: ChordAppState -> [Widget n]
stateToBars s =
  zipWith (flip bar)
  [realToFrac $ casBeat s % 4, 0, 0, 0]
  $ getN 4 (casChords s)




chordApp :: App ChordAppState ChordAppEvent String
chordApp = App
  { appDraw
      = pure
      . withBorderStyle unicode
      . joinBorders
      . border
      . hBox
      . intersperse vBorder
      . stateToBars
  , appChooseCursor
      = const
      $ const Nothing
  , appHandleEvent = \s e ->
      case e of
        AppEvent TickBeat ->
          continue $ s & field @"casBeat" .~ mod (casBeat s + 1) 4
                       & field @"casChords" %~
                             if casBeat s == 3
                                then runIdentity . unwrap
                                else id
        _ -> halt s
  , appStartEvent
      = pure
  , appAttrMap
      = const
      . attrMap V.defAttr
      $ pure (theBaseAttr, bg V.white)
  }


main :: IO ()
main = do
  eventChan <- newBChan 10

  forkIO $
    S.mapM_ (const $ writeBChan eventChan TickBeat)
      . S.filter (<= Quarter)
      $ clockStream 60

  void
     . customMain
         (V.mkVty V.defaultConfig)
         (Just eventChan)
         chordApp
     . CAS 0
     $ coiterCycle
       [ Maj C
       , Min A
       , Maj F
       , Maj G
       ]

