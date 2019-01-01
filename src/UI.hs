{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module UI where

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
import           Control.Concurrent (forkIO)
import           Control.Lens hiding ((:<))
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (..))
import           Data.Generics.Product
import           Data.List (intersperse)
import           Data.Maybe (fromJust)
import           Data.Ratio
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
import qualified Streaming.Prelude as S
import           StreamingMidi
import           Types
import           Utils


theBaseAttr :: AttrName
theBaseAttr = attrName "theBase"

theErrorAttr :: AttrName
theErrorAttr = attrName "theError"


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
  , casError  :: Bool
  , casChords :: Cofree Identity (Chord PitchClass)
  } deriving (Eq, Ord, Show, Generic)


data ChordAppEvent
  = TickBeat
  | Error
  | Redraw
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)




stateToBars :: ChordAppState -> [Widget n]
stateToBars s =
  zipWith (flip bar)
  [realToFrac $ casBeat s % 4, 0, 0, 0]
  $ getN 4 (casChords s)



chordApp :: App ChordAppState ChordAppEvent String
chordApp = App
  { appDraw
      = \s ->
        (
        if casError s
           then ([withAttr theErrorAttr $ fill 'x'] ++)
           else id
        )
      . pure
      . withBorderStyle unicode
      . joinBorders
      . border
      . hBox
      . intersperse vBorder
      $ stateToBars s
  , appChooseCursor
      = const
      $ const Nothing
  , appHandleEvent = \s e ->
      case e of
        AppEvent Error ->
          continue $ s & field @"casError" .~ True
        AppEvent Redraw ->
          continue $ s & field @"casError" .~ False
        AppEvent TickBeat ->
          continue $ s & field @"casBeat" .~ mod (casBeat s + 1) 4
                       & field @"casError" .~ False
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
      $ [ (theBaseAttr,  bg V.white)
        , (theErrorAttr, V.red `on` V.red)
        ]
  }




keysDown
    :: Monad m
    => S.Stream (S.Of Message) m r
    -> S.Stream (S.Of Pitch) m r
keysDown = S.mapMaybe add
  where
    add (NoteOn  _ n _) = Just $ pitch n
    add _               = Nothing


runUI :: S.Stream (S.Of (Either Bool Message)) IO () -> IO ()
runUI s = do
  eventChan <- newBChan 10

  let foo (Left True) = pure ()
      foo (Left False) = writeBChan eventChan Error
      foo (Right _) = writeBChan eventChan TickBeat

  forkIO $ liftIO $ S.mapM_ foo s

  void
     . customMain
         (V.mkVty V.defaultConfig)
         (Just eventChan)
         chordApp
     . CAS 0 False
     $ coiterCycle
       [ Maj C
       , Min A
       , Maj F
       , Maj G
       ]

