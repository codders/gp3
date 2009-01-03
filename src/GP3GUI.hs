{-# LANGUAGE ForeignFunctionInterface,CPP #-}

module GP3GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Pixbuf

import Control.Concurrent
import Data.Maybe
import Data.Array.MArray
import qualified Data.ByteString.Lazy as B
import Foreign

import qualified BPackReader as BPCK

foreign import ccall unsafe "gtk_docker.h do_gnome_init"
      c_gnome_init :: IO ()

data GUI = GUI {
    mainApp :: Window,
    canvas :: DrawingArea
  }

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from

-- Turns a BPCK Gliph in to a GTK GDK Pixbuf:
--   http://library.gnome.org/devel/gdk-pixbuf/2.14/gdk-pixbuf-gdk-pixbuf.html
-- Emulates the code from the FastDraw example:
--   http://darcs.haskell.org/gtk2hs/demo/fastdraw/
buildTile :: [BPCK.PaletteEntry] -> BPCK.Gliph -> IO Pixbuf
buildTile palette g = do let gData = BPCK.gliphData g
                         buf <- pixbufNew ColorspaceRgb False 8 gliphX gliphY
                         pbData <- (pixbufGetPixels buf :: IO (PixbufData Int Word8))
                         rowStride <- pixbufGetRowstride buf
                         chan <- pixbufGetNChannels buf -- Hopefully this is 3 (R,G,B)
                         bits <- pixbufGetBitsPerSample buf -- Hopefully this is 8
                         --putStrLn ("Row Bytestride: " ++ show rowStride ++ ", CPP: " ++ show chan ++ ", BPS: " ++ show bits)
                         doFromTo 0 (gliphX - 1) $ \y ->
                           doFromTo 0 (gliphY - 1) $ \x -> do
                               let pixbufoffset = x*chan + y*rowStride
                               let gliphOffset = fromIntegral $ x + y*gliphX
                               let paletteIndex = B.index gData gliphOffset
                               --putStrLn $ "Palette: " ++ (show $ length palette) ++ ", Colour: " ++ (show paletteIndex)
                               let thiscolor = palette !! fromIntegral paletteIndex
                               writeArray pbData (pixbufoffset) (fromIntegral $ BPCK.red thiscolor)
                               writeArray pbData (1 + pixbufoffset) (fromIntegral $ BPCK.green thiscolor)
                               writeArray pbData (2 + pixbufoffset) (fromIntegral $ BPCK.blue thiscolor)
                         return buf
                      where gliphX = BPCK.gliphWidth g
                            gliphY = BPCK.gliphHeight g

tilesFromImageData :: BPCK.ParsedImage -> IO [Pixbuf]
tilesFromImageData im = sequence $ map (buildTile (BPCK.palette im)) (BPCK.gliphs im)

main :: FilePath -> IO ()
main gladepath = 
  do
    unsafeInitGUIForThreadedRTS
    c_gnome_init
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    map <- BPCK.parseFile "Desert.bmap"
    tiles <- tilesFromImageData $ fromJust map
    gui <- loadGlade gladepath tiles 
    connectGui gui
    windowPresent (mainApp gui)
    mainGUI

loadGlade :: String -> [Pixbuf] -> IO GUI
loadGlade gladepath tiles = 
  do
    Just xml <- xmlNew gladepath
    app <- xmlGetWidget xml castToWindow "MainApp"
    canvas <- xmlGetWidget xml castToDrawingArea "GameCanvas"
    onExpose canvas (\(Expose {eventRegion = region}) -> do 
                              let pb = tiles !! 40
                              drawWin <- widgetGetDrawWindow canvas
                              gc <- gcNew drawWin
                              width <- pixbufGetWidth pb
                              height <- pixbufGetHeight pb
                              pbregion <- regionRectangle (Rectangle 0 0 width height)
                              regionIntersect region pbregion
                              rects <- regionGetRectangles region
                              (flip mapM_) rects $ \(Rectangle x y w h) -> do
                                drawPixbuf drawWin gc pb x y x y w h RgbDitherNone 0 0
                              return True)
    return $ GUI app undefined

connectGui gui = 
  do
    onDestroy (mainApp gui) mainQuit
