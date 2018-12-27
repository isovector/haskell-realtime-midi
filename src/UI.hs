{-# LANGUAGE TypeApplications #-}

module UI where

import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.ProgressBar
import Brick.Widgets.Center
import Brick.AttrMap
import Brick.Types
import Data.List (intersperse)
import Graphics.Vty (defAttr)
import qualified Graphics.Vty as V
import Brick.BChan
import Utils
import Euterpea.Music
import Brick.Util (fg, bg, on, clamp)


theBaseAttr :: AttrName
theBaseAttr = attrName "theBase"

xDoneAttr, xToDoAttr :: AttrName
xDoneAttr = theBaseAttr <> attrName "X:done"
xToDoAttr = theBaseAttr <> attrName "X:remaining"


bar :: Chord PitchClass -> Widget n
bar c = vBox
  [ center $ str $ showChord c
  , hBorder
  , vBox
      [ hBox
          . ( ++ [center $ str " "])
          . replicate 4
          . center
          $ str "o"
      , hBorder
      , updateAttrMap
             (mapAttrNames [ (xDoneAttr, progressCompleteAttr)
                           , (xToDoAttr, progressIncompleteAttr)
                           ]
             ) $ progressBar Nothing 0.5
      ]
  ]


main :: IO ()
main = do
  flip defaultMain  () $ App
    { appDraw
        = pure
        . pure
        . withBorderStyle unicode
        . joinBorders
        . border
        . hBox
        . intersperse vBorder
        . replicate 4
        . bar
        $ Maj7 A `Over` D
    , appChooseCursor = const $ const $ Nothing
    , appHandleEvent = const $ const $ halt @_ @String ()
    , appStartEvent = pure
    , appAttrMap = const $ theMap
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (progressIncompleteAttr,  fg V.yellow)
         ]

