{-# LANGUAGE CPP #-}
module BPackReader 
               (parseFile,
                ParsedImage,
                gliphs,
                palette,
                PaletteEntry,
                Gliph,
                gliphData,
                gliphWidth,
                gliphHeight,
                gliphDepth,
                red,
                green,
                blue)
                where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal as LI
import qualified Data.Word as DW
import Data.List
import Data.Bits
import qualified GHC.Word as W
import Control.Exception (bracket, handle)
import Control.Monad
import System.IO
import Text.Printf

#define BITMAP_OFFSET_BYTES 54
#define TILE_DIMENSION_PIXELS 16
#define TILE_BITPLANE_BYTES (TILE_DIMENSION_PIXELS * TILE_DIMENSION_PIXELS `div` 8)
#define BITPLANES 4
#define TILE_DATA_BYTES (TILE_BITPLANE_BYTES * BITPLANES)

data BPackedImage = BPI { 
                          bit8Marker :: W.Word8,
                          bit16Marker :: W.Word8,
                          size :: Integer,
                          imageData :: L.ByteString
                        }

data UnpackedImage = UPI {
                           rawImageData :: L.ByteString
                         }

data Gliph = GL {
                  gliphData :: L.ByteString,
                  gliphWidth :: Int,
                  gliphHeight :: Int,
                  gliphDepth :: Int
                } deriving (Show)

data PaletteEntry = PE {
                         red :: Integer,
                         green :: Integer,
                         blue :: Integer,
                         index :: Integer
                       } deriving (Show)

data ParsedImage = PI {
                      gliphs :: [Gliph],
                      palette :: [PaletteEntry]
                   }

instance Show ParsedImage where
     show im = "Parsed Image has " ++ show (length $ gliphs im) ++ " shapes and " ++ show (length $ palette im) ++ " colours"

instance Show BPackedImage where
     show im = "Packed image, compressed size: " ++ show (L.length $ imageData im) ++ " (decompressed: " ++ show (size im) ++ ") with markers " ++ show (bit8Marker im) ++ " and " ++ show (bit16Marker im)

instance Show UnpackedImage where
     show im = "Unpacked image, size: " ++ show (L.length $ rawImageData im)

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
decompressBytes imageData bit8marker bit16marker = 
             case L.uncons imageData of
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

unpackImage :: BPackedImage -> Maybe UnpackedImage
unpackImage bpi = do decompressed <- decompressBytes (imageData bpi) (bit8Marker bpi) (bit16Marker bpi)
                     return $ UPI decompressed

buildParsedImage :: UnpackedImage -> Maybe ParsedImage
buildParsedImage ui = do paletteBytes <- paletteData content
                         let parsedPalette = parsePalette paletteBytes
                         shapes <- loadShapes content
                         return $ PI shapes parsedPalette
                      where content = rawImageData ui

parseImage :: L.ByteString -> Maybe ParsedImage
parseImage fileData = do content <- matchHeader fileData
                         (bit8, content) <- getByte content
                         (bit16, content) <- getByte content
                         content <- skipBytes 4 content
                         (size_255, content) <- getByte content
                         (size, content) <- getByte content
                         unpacked <- unpackImage $ BPI bit8 bit16 ((fromIntegral size_255) * 255 + (fromIntegral size)) content
                         parsedImage <- buildParsedImage unpacked
                         return parsedImage

parseFile :: String -> IO (Maybe ParsedImage)
parseFile fileName = do
        putStrLn $ "Loading from " ++ fileName
        handle (\e -> do putStrLn $ "Error loading file: " ++ (show e); return Nothing) $
          bracket (openFile fileName ReadMode) hClose $ \h -> do
            fileData <- L.hGetContents h
            let image = parseImage fileData 
            case image of Just parsedImage -> putStrLn $ "Loaded image: " ++ (show parsedImage)
            return image

-- Utility function for hexdumping
asciiof :: W.Word8 -> String
asciiof x = if (x > 20 && x < 127)
              then [(LI.w2c x)]
              else "."

-- Generates the hex string corresponding to 16 bytes
dumpLine :: L.ByteString -> IO ()
dumpLine l = putStrLn $ dumpRest "" "" l
             where dumpRest hex ascii l = 
                      case (L.uncons l) of
                         Just (head, tail) -> dumpRest (hex ++ (printf "%02X " head)) (ascii ++ (asciiof head)) tail
                         Nothing -> hex ++ ascii

-- Hexdumps the image data
dumpImage :: L.ByteString -> Int -> IO()
dumpImage imagedata off = do putStr $ printf "%08X " off
                             case (getBytes 16 imagedata) of
                               Just (line, rest) -> do dumpLine line
                                                       dumpImage rest (off + 16)
                               Nothing -> return ()

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
parsePalette palElements = (PE 0 0 0 0) : (map (\(a,b) -> genColour b a) $ zip [1..15] cvalues)
                           where odds = filter (odd . fst) $ map (\(a,b) -> (a, fromIntegral b)) tupList
                                 evens = filter (even . fst) $ map (\(a,b) -> (a, (fromIntegral b)*256)) tupList
                                 tupList = L.zip (L.pack [0..fromIntegral ((L.length palElements)-1)]) palElements
                                 tidy = map snd
                                 cvalues = zipWith (+) (tidy odds) (tidy evens)

-- Takes the pixel data from four bitplanes to create a tile
createGliph :: L.ByteString -> L.ByteString -> L.ByteString -> L.ByteString -> Maybe Gliph
createGliph b8 b4 b2 b1 = do bytes <- expandByteStreams b8 b4 b2 b1
                             return $ GL bytes TILE_DIMENSION_PIXELS TILE_DIMENSION_PIXELS BITPLANES

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
                                   let thisByte = expandByte byte8 byte4 byte2 byte1
                                   case (expandByteStreams b8rest b4rest b2rest b1rest) of
                                      Just bs -> Just (thisByte `L.append` bs)
                                      Nothing -> Just thisByte

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
loadShapes :: L.ByteString -> Maybe [Gliph]
loadShapes imdata = do (head, rest)      <- getBytes BITMAP_OFFSET_BYTES imdata
                       (bitplane8, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane4, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane2, rest) <- getBytes bitplaneSeparationBytes rest
                       (bitplane1, rest) <- getBytes bitplaneSeparationBytes rest
                       blocksForBitplanes bitplane8 bitplane4 bitplane2 bitplane1
                    where numshapes = ((L.length imdata) - BITMAP_OFFSET_BYTES - TILE_BITPLANE_BYTES) `quot` TILE_DATA_BYTES
                          bitplaneSeparationBytes = fromIntegral $ numshapes * TILE_BITPLANE_BYTES

-- Dumps a list of colours
printPalette :: [PaletteEntry] -> IO()
printPalette [] = return ()
printPalette (x:xs) = do putStrLn $ show x
                         printPalette xs

-- Dumps the palette corresponding to an unpacked image
showPalette :: UnpackedImage -> IO ()
showPalette image = do putStrLn $ "Data: " ++ (show $ L.length content)
                       case (paletteData content) of 
                          Just palElements -> printPalette $ parsePalette palElements
                          Nothing -> putStrLn "Error getting palette"
                    where content = rawImageData image
