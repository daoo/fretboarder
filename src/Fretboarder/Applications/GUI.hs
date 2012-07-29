--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Monad.IO.Class
import Extensions.Tuple
import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
import Fretboarder.Parser.Parser
import Graphics.UI.Gtk

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
  size <- fmap (mapBoth fromIntegral) $ widgetGetSize canvas
  str <- entryGetText entry

  case parseExpr str of
    Left _     -> return ()
    Right expr -> renderWithDrawable win $ render defaultSettings size expr
