--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Arrow
import Control.Monad.IO.Class
import Graphics.UI.Gtk

import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
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

  entry <- entryNew
  _ <- entry `on` keyReleaseEvent $
    liftIO $ draw canvas entry >> return False

  boxPackStart vbox entry PackNatural 0

  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
  _ <- window `on` configureEvent $ liftIO (draw canvas entry) >> return False

  return window

draw :: DrawingArea -> Entry -> IO ()
draw canvas entry = do
  win <- widgetGetDrawWindow canvas
  size <- widgetGetSize canvas
  str <- entryGetText entry

  case parseExpr str of
    Left _     -> return ()
    Right expr -> renderWithDrawable win $ render defaultSettings ((realToFrac *** realToFrac) size) expr

