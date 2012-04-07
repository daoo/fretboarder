--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Monad.IO.Class

import Graphics.UI.Gtk

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

setupWindow :: IO Window
setupWindow = do
  window <- windowNew
  set window [ windowTitle := "Fretboarder" ]

  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  return window
