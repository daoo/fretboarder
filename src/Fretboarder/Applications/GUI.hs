--
-- Copyright (c) 2011-2012 Daniel Oom, see license.txt for more info.
--

module Main where

import Control.Monad.IO.Class
import Extensions.Tuple
import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
import Fretboarder.Parser.Expr
import Fretboarder.Parser.Parser
import Graphics.UI.Gtk
import Reactive.Banana

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

parseExprMaybe :: String -> Maybe (Expr PScale)
parseExprMaybe str = case parseExpr str of
  Left _     -> Nothing
  Right expr -> Just expr

setupNetwork :: (Expr PScale -> IO ()) -> AddHandler String -> IO EventNetwork
setupNetwork draw esTextChange = compile $ do
  eText <- fromAddHandler esTextChange
  reactimate $ fmap draw $ filterJust $ parseExprMaybe <$> eText

setupWindow :: IO Window
setupWindow = do
  window <- windowNew
  set window [ windowTitle := "Fretboarder" ]

  vbox   <- vBoxNew False 0
  canvas <- drawingAreaNew
  entry  <- entryNew

  set window [ containerChild := vbox ]
  boxPackEnd vbox canvas PackGrow 0
  boxPackStart vbox entry PackNatural 0

  esTextChange <- newAddHandler
  network <- setupNetwork (drawScale canvas) (fst esTextChange)
  actuate network

  _ <- on window deleteEvent $ liftIO mainQuit >> return False
  _ <- on entry editableChanged $ do
    str <- entryGetText entry
    snd esTextChange str

  return window

drawScale :: DrawingArea -> Expr PScale -> IO ()
drawScale canvas expr = do
  win  <- widgetGetDrawWindow canvas
  size <- fmap (mapBoth fromIntegral) $ widgetGetSize canvas
  renderWithDrawable win $ render defaultSettings size expr
