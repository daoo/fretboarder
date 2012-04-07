--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Arrow
import Control.Monad.IO.Class
import Graphics.UI.Gtk

import Extensions.List

import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.Color
import Fretboarder.Drawing.Helper
import Fretboarder.Guitar.Fretboard
import Fretboarder.Parser.Parser

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

setupWindow :: IO Window
setupWindow = do
  window <- windowNew
  set window [ windowTitle := "Fretboarder" ]

  vbox <- vBoxNew False 0
  set window [ containerChild := vbox ]

  canvas <- drawingAreaNew
  boxPackEnd vbox canvas PackGrow 0

  editor1 <- editorNew (draw canvas)
  boxPackStart vbox editor1 PackNatural 0

  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  return window

editorNew :: (String -> IO ()) -> IO HBox
editorNew render = do
  hbox <- hBoxNew False 1
  entry <- entryNew
  boxPackStart hbox entry PackGrow 0

  _ <- entry `on` keyReleaseEvent $ do
    liftIO $ entryGetText entry >>= render
    return False

  return hbox

draw :: DrawingArea -> String -> IO ()
draw canvas str = do
  win <- widgetGetDrawWindow canvas
  size <- widgetGetSize canvas

  case parse str of
    Ok expr -> renderWithDrawable win (render expr size)
    _       -> return ()
  where 
    render expr size =
      drawFretboard ((realToFrac *** realToFrac) size) $
      map2 f $ markList marks $ takeFrets 23 ebgdae
        where
          marks = zip tangoColors $ makeList $ makeScales expr

          f (Fret n c) = Fret n $ headOrEmpty c

