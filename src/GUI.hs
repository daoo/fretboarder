module Main where

import Control.Monad.IO.Class
import Fretboarder.Drawing.Backend
import Fretboarder.Drawing.Cairo ()
import Fretboarder.Drawing.Helper
import Fretboarder.Parser.Expr
import Fretboarder.Parser.Parser
import Fretboarder.Utility
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

colorGreen, colorRed :: Color
colorGreen = Color 35328 57856 13312
colorRed   = Color 61184 10496 10496

setupNetwork :: (Maybe (Expr PScale) -> IO ()) -> (Color -> IO ()) -> AddHandler String -> AddHandler () -> IO EventNetwork
setupNetwork draw color estext esconf = compile $ do
  etext <- fromAddHandler estext
  econf <- fromAddHandler esconf
  let (err, expr) = split $ parseExpr <$> etext

      edraw = accumE Nothing $ union
        (id <$ econf)
        ((const . Just) <$> expr)

      ecolor = accumE colorRed $ union
        (const colorGreen <$ expr)
        (const colorRed   <$ err)

  reactimate $ draw <$> edraw
  reactimate $ color <$> ecolor

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
  size@(w, h) <- widgetGetSize canvas
  drawWindowBeginPaintRect win $ Rectangle 0 0 w h
  renderWithDrawable win $ render defaultSettings (mapBoth fromIntegral size) expr
  drawWindowEndPaint win
