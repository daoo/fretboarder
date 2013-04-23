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
import Reactive.Banana.Frameworks

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

colorGreen, colorRed :: Color
colorGreen = Color 35328 57856 13312
colorRed   = Color 61184 10496 10496

setupNetwork :: (Maybe (Expr PScale) -> IO ()) -> (Color -> IO ()) -> AddHandler String -> AddHandler () -> IO EventNetwork
setupNetwork draw color esTextChange esConfigure = compile $ do
  eText      <- fromAddHandler esTextChange
  eConfigure <- fromAddHandler esConfigure
  let (eError, eExpr) = split $ parseExpr <$> eText
      bExpr = stepper Nothing $ Just <$> eExpr

  reactimate $ draw <$> Just <$> eExpr
  reactimate $ draw <$> (bExpr <@ eConfigure)
  reactimate $ const (color colorGreen) <$> eExpr
  reactimate $ const (color colorRed) <$> eError

setWidgetBg :: WidgetClass self => self -> Color -> IO ()
setWidgetBg e = widgetModifyBase e StateNormal

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
  esConfigure  <- newAddHandler

  network <- setupNetwork (drawScale canvas) (setWidgetBg entry) (fst esTextChange) (fst esConfigure)
  actuate network

  _ <- on window deleteEvent $ liftIO mainQuit >> return False
  _ <- on window configureEvent $ liftIO (snd esConfigure ()) >> return False
  _ <- on entry editableChanged $ entryGetText entry >>= snd esTextChange

  return window

drawScale :: DrawingArea -> Maybe (Expr PScale) -> IO ()
drawScale _ Nothing          = return ()
drawScale canvas (Just expr) = do
  win  <- widgetGetDrawWindow canvas
  size <- fmap (mapBoth fromIntegral) $ widgetGetSize canvas
  renderWithDrawable win $ render defaultSettings size expr
