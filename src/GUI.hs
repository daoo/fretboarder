module Main (main) where

import Control.Monad.IO.Class
import Data.Attoparsec.Text
import Fretboarder.Drawing.Cairo
import Fretboarder.Music.Fretboard
import Fretboarder.Music.Scale
import Fretboarder.Parser
import Graphics.UI.Gtk hiding (Scale)
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Data.Text as T

main :: IO ()
main = initGUI >> setupWindow >>= widgetShowAll >> mainGUI

colorGreen, colorRed :: Color
colorGreen = Color 35328 57856 13312
colorRed   = Color 61184 10496 10496

setupNetwork :: (Maybe Scale -> IO ()) -> (Color -> IO ()) -> AddHandler String -> AddHandler () -> IO EventNetwork
setupNetwork draw color estext esconf = compile $ do
  etext <- fromAddHandler estext
  econf <- fromAddHandler esconf
  let (err, scale) = split $ (parseOnly parseScale . T.pack) <$> etext

      edraw = accumE Nothing $ union
        (id <$ econf)
        ((const . Just) <$> scale)

      ecolor = accumE colorRed $ union
        (const colorGreen <$ scale)
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

drawScale :: DrawingArea -> Maybe Scale -> IO ()
drawScale _ Nothing          = return ()
drawScale canvas (Just scale) = do
  win  <- widgetGetDrawWindow canvas
  (w, h) <- widgetGetSize canvas
  drawWindowBeginPaintRect win $ Rectangle 0 0 w h
  renderWithDrawable win $ drawFretboard (realToFrac w) (realToFrac h) ebgdae scale
  drawWindowEndPaint win
