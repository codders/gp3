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
import Data.IORef
import Data.Char
import Data.String.Utils
import System.Directory
import Debug.Trace
import qualified Data.ByteString.Lazy as B

import qualified BPackReader as BPCK

foreign import ccall unsafe "gtk_docker.h do_gnome_init"
      c_gnome_init :: IO ()

#define LEVELS_FOLDER     "levels"
#define TILESETS_FOLDER   "gfx"
#define LEVEL_SUFFIX      ".gfb"
#define TILESET_SUFFIX     ".bmap"

data GUI = GUI {
    mainApp :: Window,
    canvas :: DrawingArea
  }

data MapInfo = MI {
    tileSetFile :: String,
    mapFile :: String
  } deriving (Show)

data MapState = MS {
    mapList :: [MapInfo],
    renderedMap :: Pixmap
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

hasSuffix :: String -> String -> Bool
hasSuffix suffix = \s -> endswith suffix (map toLower s)

hasPrefix :: String -> String -> Bool
hasPrefix prefix = \s -> startswith prefix (map toLower s)

-- Gets a list of the tile sets from disk
getTileSets :: IO [String]
getTileSets = do 
           files <- getDirectoryContents TILESETS_FOLDER
           return $ filter (hasSuffix TILESET_SUFFIX) files

-- Creates MapInfos by matching maps with TileSets
matchUpFiles :: [String] -> [String] -> [MapInfo]
matchUpFiles mapFiles tileSets = catMaybes $ map mapToMapInfo mapFiles
           where mapToMapInfo file = case matchTileSet file of
                                       Just mapName -> Just $ MI (TILESETS_FOLDER ++ "/" ++ mapName) (LEVELS_FOLDER ++ "/" ++ file)
                                       Nothing -> Nothing
                 matchTileSet file = case filter (hasPrefix (map toLower (take 2 file))) tileSets of
                                       (res:_) -> Just res
                                       _       -> Nothing

-- Gets a list of Maps / Levels from disk
getMaps :: IO [MapInfo]
getMaps = do
     mapFiles <- getDirectoryContents LEVELS_FOLDER
     tileSets <- getTileSets
     return $ matchUpFiles (filter (hasSuffix LEVEL_SUFFIX) mapFiles) tileSets

-- Loads a Pixmap from a MapInfo
loadPixmap :: MapInfo -> IO (Pixmap)
loadPixmap info = do
    tileSet <- BPCK.parseImageFile (tileSetFile info)
    let justTileSet = fromJust tileSet
    tileMap <- BPCK.parseMapFile (mapFile info)
    let justTileMap = fromJust tileMap
    tiledPixmap <- createTiledPixmap justTileSet justTileMap
    return tiledPixmap

-- Creates a fresh blank MapState
newMapState :: IO (MapState)
newMapState = do
    allMaps <- getMaps
    thisMap <- loadPixmap $ head allMaps    
    return $ MS allMaps thisMap

-- Advances MapState to the next map
nextMapState :: MapState -> IO (MapState)
nextMapState state = do
    let newMapList = (tail oldMapList) ++ [head oldMapList]
    thisMap <- loadPixmap $ head newMapList
    return $ MS newMapList thisMap
    where oldMapList = mapList state

main :: FilePath -> IO ()
main gladepath = 
  do
    unsafeInitGUIForThreadedRTS
    c_gnome_init
    timeoutAddFull (yield >> return True) priorityDefaultIdle 100
    mapState <- newMapState
    mapStateRef <- newIORef mapState
    gui <- loadGlade gladepath mapStateRef
    connectGui gui
    windowPresent (mainApp gui)
    mainGUI

loadGlade :: String -> IORef (MapState) -> IO GUI
loadGlade gladepath mapStateRef = 
  do
    Just xml <- xmlNew gladepath
    app <- xmlGetWidget xml castToWindow "MainApp"
    canvas <- xmlGetWidget xml castToDrawingArea "GameCanvas"
    onExpose canvas (\(Expose {eventRegion = region}) -> do 
                              drawWin <- widgetGetDrawWindow canvas
                              gc <- gcNew drawWin
                              (width, height) <- drawableGetSize drawWin
                              dwRegion <- regionRectangle (Rectangle 0 0 width height)
                              regionIntersect region dwRegion
                              rects <- regionGetRectangles region
                              mapState <- readIORef mapStateRef
                              (flip mapM_) rects (\(Rectangle x y w h) -> postGUIAsync $ drawDrawable drawWin gc (renderedMap mapState) x y x y w h)
                              return True)
    onKeyPress app (\x@(Key { eventKeyName = name,
                              eventKeyChar = char }) -> do 
                              case char of
                                 Just ' ' -> do 
                                      putStrLn $ "Switching map"
                                      currentState <- readIORef mapStateRef
                                      nextState <- nextMapState currentState
                                      writeIORef mapStateRef nextState
                                      drawWin <- widgetGetDrawWindow canvas
                                      gc <- gcNew drawWin
                                      (width, height) <- drawableGetSize drawWin
                                      postGUIAsync $ drawDrawable drawWin gc (renderedMap nextState) 0 0 0 0 width height
                                 Just c  -> putStrLn $ "Press " ++ name ++ "('" ++ [c] ++ "')"
                                 Nothing -> putStrLn $ "weird key: " ++ name
                              return (eventSent x))
    return $ GUI app undefined

connectGui gui = 
  do
    onDestroy (mainApp gui) mainQuit
