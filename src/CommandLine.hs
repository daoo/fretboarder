module Main (main) where

import Data.Attoparsec.Text
import Data.Char
import Fretboarder.Drawing.Cairo
import Fretboarder.Guitar.Fretboard
import Fretboarder.Parser
import Graphics.Rendering.Cairo hiding (scale)
import System.Environment
import System.FilePath
import qualified Data.Text as T

data ImageType = SVG | PNG

withSurface :: ImageType -> FilePath -> (Int, Int) -> (Surface -> IO ()) -> IO ()
withSurface t file (w, h) r = case t of
  SVG -> withSVGSurface file (fromIntegral w) (fromIntegral h) r
  PNG -> createImageSurface FormatARGB32 w h >>= (\s -> r s >> surfaceWriteToPNG s file)

findImageType :: FilePath -> ImageType
findImageType path = case map toLower $ takeExtension path of
  ".png" -> PNG
  ".svg" -> SVG
  _      -> error "Unknown file type."

main :: IO ()
main = do
  (w : h : path : rest) <- getArgs

  case parseOnly parseScale (T.pack $ concat rest) of
    Left err    -> print err
    Right scale -> let ft = findImageType path
                       wi = read w :: Int
                       hi = read h :: Int
                       wd = realToFrac wi
                       hd = realToFrac hi
                    in withSurface ft path (wi, hi) $ flip renderWith $ drawFretboard wd hd ebgdae scale
