{-# LANGUAGE ForeignFunctionInterface,CPP #-}

module GP3GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Pixbuf

import Control.Concurrent
import Data.Maybe
import Data.Array.MArray
import Data.Word
import qualified Data.ByteString.Lazy as B

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
                         doFromTo 0 (gliphX - 1) $ \y ->
                           doFromTo 0 (gliphY - 1) $ \x -> do
                               let pixbufoffset = x*chan + y*rowStride
                               let gliphOffset = fromIntegral $ x + y*gliphX
                               let paletteIndex = B.index gData gliphOffset
                               let thiscolor = palette !! fromIntegral paletteIndex
                               writeArray pbData (pixbufoffset) (fromIntegral $ BPCK.red thiscolor)
                               writeArray pbData (1 + pixbufoffset) (fromIntegral $ BPCK.green thiscolor)
                               writeArray pbData (2 + pixbufoffset) (fromIntegral $ BPCK.blue thiscolor)
                         return buf
                      where gliphX = BPCK.gliphWidth g
                            gliphY = BPCK.gliphHeight g

-- Turns the array of gliphs from the parsed image into an array of Pixbufs ready
-- to be pasted on to the map
tilesFromImageData :: BPCK.ParsedImage -> IO [Pixbuf]
tilesFromImageData im = sequence $ map (buildTile (BPCK.palette im)) (BPCK.blankGliph : BPCK.gliphs im)

-- Create a pixmap using the tileset and the pixmaps
createTiledPixmap :: BPCK.ParsedImage -> BPCK.ParsedTileMap -> IO Pixmap
createTiledPixmap tileSet tileMap = do
              putStrLn $ "Building tile pixmaps"
              tiles <- tilesFromImageData tileSet
              let tileCount = length tiles
              putStrLn $ "Creating new pixmap " ++ show totalWidthPixels ++ " x " ++ show totalHeightPixels
              pixmap <- pixmapNew (Nothing :: Maybe DrawWindow) totalWidthPixels totalHeightPixels (Just 24)
              gc <- gcNew pixmap
              doFromTo 0 (tilesHigh - 1) $ \iy ->
                doFromTo 0 (tilesAcross - 1) $ \ix -> do
                  let tileIndex = ix + (iy * tilesAcross)
                  let tileId = (min (fromIntegral (BPCK.tileMap tileMap !! tileIndex)) tileCount) `mod` tileCount
                  let curX = ix * tileSizePixels
                  let curY = iy * tileSizePixels
                  postGUIAsync $ drawPixbuf pixmap gc (tiles !! tileId) 0 0 curX curY tileSizePixels tileSizePixels RgbDitherNone 0 0
              return pixmap
              where tileSizePixels = BPCK.gliphSize tileSet
                    tilesAcross = BPCK.tilesAcross tileMap
                    tilesHigh = BPCK.tilesHigh tileMap
                    totalWidthPixels = tileSizePixels * tilesAcross
                    totalHeightPixels = tileSizePixels * tilesHigh

main :: FilePath -> IO ()
main gladepath = 
  do
    unsafeInitGUIForThreadedRTS
    c_gnome_init
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    tileSet <- BPCK.parseImageFile "gfx/Metallic.bmap"
    let justTileSet = fromJust tileSet
    tileMap <- BPCK.parseMapFile "levels/MEMechanoid.GFB"
    let justTileMap = fromJust tileMap
    gui <- loadGlade gladepath justTileSet justTileMap
    connectGui gui
    windowPresent (mainApp gui)
    mainGUI

loadGlade :: String -> BPCK.ParsedImage -> BPCK.ParsedTileMap -> IO GUI
loadGlade gladepath tileSet tileMap = 
  do
    Just xml <- xmlNew gladepath
    app <- xmlGetWidget xml castToWindow "MainApp"
    canvas <- xmlGetWidget xml castToDrawingArea "GameCanvas"
    tiledPixmap <- createTiledPixmap tileSet tileMap
    onExpose canvas (\(Expose {eventRegion = region}) -> do 
                              drawWin <- widgetGetDrawWindow canvas
                              gc <- gcNew drawWin
                              (width, height) <- drawableGetSize drawWin
                              dwRegion <- regionRectangle (Rectangle 0 0 width height)
                              regionIntersect region dwRegion
                              rects <- regionGetRectangles region
                              (flip mapM_) rects (\(Rectangle x y w h) -> postGUIAsync $ drawDrawable drawWin gc tiledPixmap x y x y w h)
                              return True)
    return $ GUI app undefined

connectGui gui = 
  do
    onDestroy (mainApp gui) mainQuit
