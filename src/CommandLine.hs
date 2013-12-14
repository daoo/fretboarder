module Main (main) where

import Data.Char
import Fretboarder.Drawing.Cairo
import Fretboarder.Guitar.Fretboard
import Fretboarder.Parser.Parser
import Fretboarder.Utility
import Graphics.Rendering.Cairo hiding (scale)
import System.Environment
import System.FilePath

data ImageType = SVG | PNG

withSurface :: ImageType -> FilePath -> (Int, Int) -> (Surface -> IO ()) -> IO ()
withSurface t file (w, h) r = case t of
  SVG -> withSVGSurface file (fromIntegral w) (fromIntegral h) r
  PNG -> createImageSurface FormatARGB32 w h >>= (\s -> r s >> surfaceWriteToPNG s file)

main :: IO ()
main = do
  (w : h : file : rest) <- getArgs

  case parseExprScale (concat rest) of
    Left err   -> print err
    Right expr -> let ft = case map toLower $ takeExtension file of
                        ".png" -> PNG
                        ".svg" -> SVG
                        _      -> error "Unknown file type."
                      sizei = mapBoth read (w, h)
                      sized = mapBoth fromIntegral sizei
                   in withSurface ft file sizei $ flip renderWith $ drawFretboard sized ebgdae (makeList expr)
