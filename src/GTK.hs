module Main (main) where

import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Data.IORef
import Fretboarder.Drawing.Cairo
import Fretboarder.Drawing.Expr
import Fretboarder.Music.Fretboard
import Fretboarder.Parser
import Graphics.UI.Gtk hiding (Scale)
import qualified Data.Text as T

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

green, red :: Color
green = Color 35328 57856 13312
red   = Color 61184 10496 10496

setWidgetBg :: WidgetClass self => self -> Color -> IO ()
setWidgetBg e = widgetModifyBase e StateNormal

type State = [Expr]

setupWindow :: IO Window
setupWindow = do
  window <- windowNew
  set window [ windowTitle := "Fretboarder" ]

  state <- newIORef []

  vbox   <- vBoxNew False 0
  canvas <- drawingAreaNew
  entry  <- entryNew

  set window [ containerChild := vbox ]
  boxPackEnd vbox canvas PackGrow 0
  boxPackStart vbox entry PackNatural 0

  _ <- on window deleteEvent $ liftIO mainQuit >> return False
  _ <- on window configureEvent $ liftIO (readIORef state >>= draw canvas) >> return False
  _ <- on entry editableChanged $ entryGetText entry >>= text (draw canvas) (setWidgetBg entry) state

  return window

text :: ([Expr] -> IO ()) -> (Color -> IO ()) -> IORef State -> String -> IO ()
text d c s t = case parseOnly parseExprs (T.pack t) of
  Left _      -> writeIORef s []    >> c red   >> d []
  Right exprs -> writeIORef s exprs >> c green >> d exprs

draw :: DrawingArea -> [Expr] -> IO ()
draw da exprs = do
  win    <- widgetGetDrawWindow da
  (w, h) <- widgetGetSize da
  drawWindowBeginPaintRect win $ Rectangle 0 0 w h
  renderWithDrawable win $ drawFretboard (realToFrac w) (realToFrac h) ebgdae exprs
  drawWindowEndPaint win
