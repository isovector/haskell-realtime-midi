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
      , center $ updateAttrMap
             (mapAttrNames [ (xDoneAttr, progressCompleteAttr)
                           ])   $ progressBar Nothing p
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
        $ bar (Maj7 A `Over` D) 0.25
    , appChooseCursor = const $ const $ Nothing
    , appHandleEvent  = const $ const $ halt @_ @String ()
    , appStartEvent   = pure
    , appAttrMap      = const $ theMap
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
         [ (theBaseAttr,               bg V.white)
         ]

