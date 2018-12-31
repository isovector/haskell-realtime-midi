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
import           Data.List (intersperse)
import           Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
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


chordApp :: App () e String
chordApp = App
  { appDraw
      = pure
      . pure
      . withBorderStyle unicode
      . joinBorders
      . border
      . hBox
      . intersperse vBorder
      . replicate 4
      $ bar (Maj7 A `Over` D) 0.25
  , appChooseCursor
      = const
      $ const Nothing
  , appHandleEvent
      = const
      . const
      $ halt @_ @String ()
  , appStartEvent
      = pure
  , appAttrMap
      = const
      . attrMap V.defAttr
      $ pure (theBaseAttr, bg V.white)
  }

