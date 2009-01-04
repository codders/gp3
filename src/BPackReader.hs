{-# LANGUAGE CPP #-}
module BPackReader 
               (parseImageFile,
                parseMapFile,
                ParsedImage,
                gliphs,
                palette,
                gliphSize,
                PaletteEntry,
                Gliph,
                gliphData,
                gliphWidth,
                gliphHeight,
                gliphDepth,
                blankGliph,
                red,
                green,
                blue,
                ParsedTileMap,
                tileMap,
                tilesHigh,
                tilesAcross)
                where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal as LI
import qualified Data.Word as DW
import Data.List
import Data.Bits
import Data.Maybe
import qualified GHC.Word as W
import Control.Exception (bracket, handle)
import Control.Monad
import System.IO
import Text.Printf
import Debug.Trace

-- For Level Maps
#define LEVELMAP_OFFSET_BYTES 130

-- For Tile Sets
#define BITMAP_OFFSET_BYTES 54
#define TILE_DIMENSION_PIXELS 16
#define TILE_BITPLANE_BYTES (TILE_DIMENSION_PIXELS * TILE_DIMENSION_PIXELS `div` 8)
#define BITPLANES 4
#define TILE_DATA_BYTES (TILE_BITPLANE_BYTES * BITPLANES)
#define IMAGE_HEIGHT_TILES 63
#define IMAGE_WIDTH_TILES 21

data BPackedFile = BPF { 
                          bit8Marker :: W.Word8,
                          bit16Marker :: W.Word8,
                          size :: Integer,
                          fileData :: L.ByteString
                        }

data FileType = CompressedFile
              | UncompressedFile

data UnpackedFile = UPF {
                           rawFileData :: L.ByteString
                         }

data Gliph = GL {
                  gliphData :: L.ByteString,
                  gliphWidth :: Int,
                  gliphHeight :: Int,
                  gliphDepth :: Int
                } 

data PaletteEntry = PE {
                         red :: Integer,
                         green :: Integer,
                         blue :: Integer,
                         index :: Integer
                       } deriving (Show)

data ParsedImage = PI {
                      gliphs :: [Gliph],
                      palette :: [PaletteEntry],
                      gliphSize :: Int
                   }

data ParsedTileMap = PTM {
                       tileMap :: [W.Word8],
                       tilesAcross :: Int,
                       tilesHigh :: Int
                         }

instance Show Gliph where
     show g = "Gliph:\n"++(dumpImage (gliphData g) 0)

instance Show ParsedTileMap where
     show tm = "Parsed Tile map has " ++ show (length $ tileMap tm) ++ " tiles in the map"

instance Show ParsedImage where
     show im = "Parsed Image has " ++ show (length $ gliphs im) ++ " shapes and " ++ show (length $ palette im) ++ " colours."

instance Show BPackedFile where
     show im = "Packed image, compressed size: " ++ show (L.length $ fileData im) ++ " (decompressed: " ++ show (size im) ++ ") with markers " ++ show (bit8Marker im) ++ " and " ++ show (bit16Marker im)

instance Show UnpackedFile where
     show im = "Unpacked image, size: " ++ show (L.length $ rawFileData im)

matchHeader :: L.ByteString -> Maybe L.ByteString
matchHeader fileData = do (head, tail) <- getBytes 4 fileData
                          if (head == L8.pack("BPCK"))
                            then return tail
                            else (fail "failed")

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count         = fromIntegral n 
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

getByte :: L.ByteString -> Maybe (W.Word8, L.ByteString)
getByte = L.uncons

skipBytes :: Int -> L.ByteString -> Maybe L.ByteString
skipBytes n str = do if L.length prefix < count
                       then Nothing
                       else Just tail
                  where count = fromIntegral n
                        (prefix, tail) = L.splitAt count str

decompressBytes :: L.ByteString -> W.Word8 -> W.Word8 -> Maybe L.ByteString
decompressBytes fileData bit8marker bit16marker = 
             case L.uncons fileData of
                Just (top, tail) -> 
                   if (top == bit8marker)
                     then do (n, rest) <- getByte tail
                             (v, rest) <- getByte rest
                             result <- (decompressBytes rest bit8marker bit16marker)
                             return $ L.append (L.replicate (fromIntegral n) v) result
                     else if (top == bit16marker)
                            then do (n255, rest) <- getByte tail
                                    (n, rest) <- getByte rest
                                    (v, rest) <- getByte rest
                                    result <- decompressBytes rest bit8marker bit16marker
                                    return $ L.append (L.replicate ((fromIntegral n255) * 256 + (fromIntegral n)) v) result
                            else do result <- (decompressBytes tail bit8marker bit16marker)
                                    return $ L.cons top result
                Nothing -> Just L.empty

unpackAsFile :: BPackedFile -> Maybe UnpackedFile
unpackAsFile bpi = do decompressed <- decompressBytes (fileData bpi) (bit8Marker bpi) (bit16Marker bpi)
                      return $ UPF decompressed

buildParsedImage :: L.ByteString -> FileType -> Maybe ParsedImage
buildParsedImage content filetype = do 
                              paletteBytes <- paletteData content
                              let parsedPalette = parsePalette paletteBytes
                              shapes <- case filetype of CompressedFile -> loadCompressedShapes content
                                                         UncompressedFile -> loadUncompressedShapes content
                              return $ PI shapes parsedPalette TILE_DIMENSION_PIXELS

unpackData :: L.ByteString -> Maybe UnpackedFile
unpackData fileData = do content <- matchHeader fileData
                         (bit8, content) <- getByte content
                         (bit16, content) <- getByte content
                         content <- skipBytes 4 content
                         (size_255, content) <- getByte content
                         (size, content) <- getByte content
                         unpackAsFile $ BPF bit8 bit16 ((fromIntegral size_255) * 255 + (fromIntegral size)) content

parseAsTileMap :: L.ByteString -> FileType -> Maybe ParsedTileMap
parseAsTileMap fileData filetype = do 
                             tileArray <- loadTileMap fileData
                             return $ PTM tileArray IMAGE_WIDTH_TILES IMAGE_HEIGHT_TILES

parseCompressedFile :: Show a => (L.ByteString -> FileType -> Maybe a) -> String -> IO (Maybe a)
parseCompressedFile parseFunction fileName = do
        putStrLn $ "Loading from " ++ fileName
        handle (\e -> do putStrLn $ "Error loading file: " ++ (show e); return Nothing) $
          bracket (openFile fileName ReadMode) hClose $ \h -> do
            fileData <- L.hGetContents h
            let unpacked = unpackData fileData
            let object = case unpacked of Just unpackedFileData -> parseFunction (rawFileData unpackedFileData) CompressedFile
                                          Nothing -> parseFunction fileData UncompressedFile 
            case object of Just parsedObject -> putStrLn $ "Loaded: " ++ (show parsedObject)
                           Nothing -> error "Unable to parse file"
            return object


parseImageFile :: String -> IO (Maybe ParsedImage)
parseImageFile = parseCompressedFile buildParsedImage

parseMapFile :: String -> IO (Maybe ParsedTileMap)
parseMapFile = parseCompressedFile parseAsTileMap

dumpDecompressed :: String -> IO ()
dumpDecompressed fileName = do
        putStrLn $ "Loading from " ++ fileName
        handle (\e -> do putStrLn $ "Error loading file: " ++ (show e); return ()) $
          bracket (openFile fileName ReadMode) hClose $ \h -> do
            fileData <- L.hGetContents h
            let image = fromJust $ unpackData fileData
            putStrLn $ dumpImage (rawFileData image) 0
            return ()


-- Utility function for hexdumping
asciiof :: W.Word8 -> String
asciiof x = if (x > 20 && x < 127)
              then [(LI.w2c x)]
              else "."

-- Generates the hex string corresponding to 16 bytes
dumpLine :: L.ByteString -> String
dumpLine l = dumpRest "" "" l
             where dumpRest hex ascii l = 
                      case (L.uncons l) of
                         Just (head, tail) -> dumpRest (hex ++ (printf "%02X " head)) (ascii ++ (asciiof head)) tail
                         Nothing -> hex ++ ascii

-- Hexdumps the image data
dumpImage :: L.ByteString -> Int -> String
dumpImage imagedata off = printf "%08X " off ++ remainder
                          where remainder = case (getBytes 16 imagedata) of
                                               Just (line, rest) -> dumpLine line ++ "\n" ++ dumpImage rest (off + 16)
                                               Nothing -> ""

-- Reads the palette bytes from the end of the file
paletteData :: L.ByteString -> Maybe L.ByteString
paletteData imdata = do (header, rest) <- getBytes (fromIntegral $ L.length imdata - 32) imdata
                        (result, tail) <- getBytes 30 rest
                        return result

-- Creates a new Palette entry by shifting red, green and blue nibbles from the value provided
genColour :: Integer -> Integer -> PaletteEntry
genColour value index = PE ((value `shiftR` 8) * 16) (((value .&. 0xF0) `shiftR` 4) * 16) ((value .&. 0x0F) * 16) index

-- Takes 30 bytes of palette data, treating each pair of bytes as a short, and generates 
-- the corresponding palette of 15 colours
parsePalette :: L.ByteString -> [PaletteEntry]
parsePalette palElements = (map (\(a,b) -> genColour b a) $ zip [1..15] cvalues) ++ [PE 0 0 0 0]
                           where odds = filter (odd . fst) $ map (\(a,b) -> (a, fromIntegral b)) tupList
                                 evens = filter (even . fst) $ map (\(a,b) -> (a, (fromIntegral b)*256)) tupList
                                 tupList = L.zip (L.pack [0..fromIntegral ((L.length palElements)-1)]) palElements
                                 tidy = map snd
                                 cvalues = zipWith (+) (tidy odds) (tidy evens)

-- Takes the pixel data from four bitplanes to create a tile
createGliph :: L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString -> Maybe Gliph
createGliph b8 b4 b2 b1 = do bytes <- expandByteStreams b8 b4 b2 b1
                             return $ GL bytes TILE_DIMENSION_PIXELS TILE_DIMENSION_PIXELS BITPLANES

-- Generates an empty Gliph
blankGliph :: Gliph
blankGliph = let bytecount = TILE_DIMENSION_PIXELS * TILE_DIMENSION_PIXELS
             in GL (L.replicate bytecount 0) TILE_DIMENSION_PIXELS TILE_DIMENSION_PIXELS BITPLANES

-- Takes 4 8-bit words and makes 8 4-bit words
expandByte :: W.Word8 -> W.Word8 -> W.Word8 -> W.Word8 -> L.ByteString
expandByte b8 b4 b2 b1 = L.pack $ map pixelise [0..7]
                         where pixelise i = (((b8 `shiftR` (7-i)) .&. 1) `shiftL` 3) .|. 
                                            (((b4 `shiftR` (7-i)) .&. 1) `shiftL` 2) .|.
                                            (((b2 `shiftR` (7-i)) .&. 1) `shiftL` 1) .|.
                                            ((b1 `shiftR` (7-i) .&. 1))

-- Combines the four incoming bitstreams to create a string of 4-bit words
expandByteStreams :: L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString -> Maybe L.ByteString
expandByteStreams b8 b4 b2 b1 = do (byte8, b8rest) <- getByte b8
                                   (byte4, b4rest) <- getByte b4
                                   (byte2, b2rest) <- getByte b2
                                   (byte1, b1rest) <- getByte b1
                                   let thisByte = expandByte byte1 byte2 byte4 byte8
                                   case (expandByteStreams b8rest b4rest b2rest b1rest) of
                                      Just bs -> Just (thisByte `L.append` bs)
                                      Nothing -> Just thisByte

-- Gets the number of shapes in an image data block
numberOfShapes :: L.ByteString -> Int
numberOfShapes imdata = fromIntegral $ ((L.length imdata) - BITMAP_OFFSET_BYTES - TILE_BITPLANE_BYTES) `quot` TILE_DATA_BYTES

-- In an uncompressed gliph stream, the bitplanes are interleaved pairs of bytes
readGliphStream :: L.ByteString -> Maybe [Gliph]
readGliphStream stream = blocksForBitplanes (pairs 0 sEights) (pairs 1 sEights) (pairs 2 sEights) (pairs 3 sEights)
                         where sEights = eights stream
                               eights astream | L.length astream > 8 = let (head, tail) = L.splitAt 8 astream
                                                                       in (L.unpack head) : eights tail
                                              | otherwise            = [L.unpack astream]
                               pairs n (h:t) = (L.pack $ take 2 (drop (2*n) h)) `L.append` (pairs n t)
                               pairs _ []    = L.empty

-- Skip the head then parse the remaining bytes
loadUncompressedShapes :: L.ByteString -> Maybe [Gliph]
loadUncompressedShapes imdata = do
                         (head, rest) <- getBytes BITMAP_OFFSET_BYTES imdata
                         readGliphStream rest

-- Takes a list of tuples of bytes from four bitplanes and returns a series of tiles
blocksForBitplanes :: L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString -> Maybe [Gliph]
blocksForBitplanes b8 b4 b2 b1 = do (tb8, b8rest) <- getBytes TILE_BITPLANE_BYTES b8
                                    (tb4, b4rest) <- getBytes TILE_BITPLANE_BYTES b4
                                    (tb2, b2rest) <- getBytes TILE_BITPLANE_BYTES b2
                                    (tb1, b1rest) <- getBytes TILE_BITPLANE_BYTES b1
                                    gliphData <- createGliph tb8 tb4 tb2 tb1
                                    case (blocksForBitplanes b8rest b4rest b2rest b1rest) of
                                      Just x -> Just (gliphData : x)
                                      Nothing -> Just [gliphData]

-- Reads in the tile pixel data, turning 4 bitplanes of pixels into an array of tiles of 
-- 4-bit mapped-palette values
loadCompressedShapes :: L.ByteString -> Maybe [Gliph]
loadCompressedShapes imdata = do 
                       (head, rest)      <- getBytes BITMAP_OFFSET_BYTES imdata
                       (bitplane8, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane4, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane2, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane1, rest) <- getBytes bitplaneSeparationBytes rest
                       blocksForBitplanes bitplane8 bitplane4 bitplane2 bitplane1
                    where bitplaneSeparationBytes = (numberOfShapes imdata) * TILE_BITPLANE_BYTES

-- Dumps a list of colours
printPalette :: [PaletteEntry] -> IO()
printPalette [] = return ()
printPalette (x:xs) = do putStrLn $ show x
                         printPalette xs

-- Dumps the palette corresponding to an unpacked image
showPalette :: UnpackedFile -> IO ()
showPalette image = do putStrLn $ "Data: " ++ (show $ L.length content)
                       case (paletteData content) of 
                          Just palElements -> printPalette $ parsePalette palElements
                          Nothing -> putStrLn "Error getting palette"
                    where content = rawFileData image

-- Reads the list of tiles from the image data
loadTileMap :: L.ByteString -> Maybe [W.Word8]
loadTileMap imdata = do (head, rest) <- getBytes LEVELMAP_OFFSET_BYTES imdata
                        (tilemap, rest) <- getBytes (IMAGE_WIDTH_TILES * IMAGE_HEIGHT_TILES) rest
                        return $ L.unpack tilemap

