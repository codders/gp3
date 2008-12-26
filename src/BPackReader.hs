module BPackReader where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal as LI
import qualified Data.Word as DW
import Data.Bits
import qualified GHC.Word as W
import Control.Exception (bracket, handle)
import Control.Monad
import System.IO
import Text.Printf

data BPackedImage = BPI { 
                          bit8Marker :: W.Word8,
                          bit16Marker :: W.Word8,
                          size :: Integer,
                          imageData :: L.ByteString
                        }

data UnpackedImage = UPI {
                           rawImageData :: L.ByteString
                         }

data PaletteEntry = PE {
                         red :: Integer,
                         green :: Integer,
                         blue :: Integer,
                         index :: Integer
                       } deriving (Show)

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

parseImage :: L.ByteString -> Maybe UnpackedImage
parseImage fileData = do content <- matchHeader fileData
                         (bit8, content) <- getByte content
                         (bit16, content) <- getByte content
                         content <- skipBytes 4 content
                         (size_255, content) <- getByte content
                         (size, content) <- getByte content
                         unpacked <- unpackImage $ BPI bit8 bit16 ((fromIntegral size_255) * 255 + (fromIntegral size)) content
                         return unpacked

parseFile :: String -> IO (Maybe UnpackedImage)
parseFile fileName = do
        putStrLn $ "Loading from " ++ fileName
        handle (\_ -> return Nothing) $
          bracket (openFile fileName ReadMode) hClose $ \h -> do
            fileData <- L.hGetContents h
            let image = parseImage fileData 
            case image of Just parsedImage -> putStrLn $ "Loaded image: " ++ (show parsedImage)
            return image

asciiof :: W.Word8 -> String
asciiof x = if (x > 20 && x < 127)
              then [(LI.w2c x)]
              else "."

dumpLine :: L.ByteString -> IO ()
dumpLine l = putStrLn $ dumpRest "" "" l
             where dumpRest hex ascii l = 
                      case (L.uncons l) of
                         Just (head, tail) -> dumpRest (hex ++ (printf "%02X " head)) (ascii ++ (asciiof head)) tail
                         Nothing -> hex ++ ascii

dumpImage :: L.ByteString -> Int -> IO()
dumpImage imagedata off = do putStr $ printf "%08X " off
                             case (getBytes 16 imagedata) of
                               Just (line, rest) -> do dumpLine line
                                                       dumpImage rest (off + 16)
                               Nothing -> return ()
                    
paletteData :: L.ByteString -> Maybe L.ByteString
paletteData imdata = do (header, rest) <- getBytes (fromIntegral $ L.length imdata - 32) imdata
                        (result, tail) <- getBytes 30 rest
                        return result

genColour :: Integer -> Integer -> PaletteEntry
genColour value index = PE ((value `shiftR` 8) * 16) (((value .&. 0xF0) `shiftR` 4) * 16) ((value .&. 0x0F) * 16) index

parsePalette palElements = map (\(a,b) -> genColour b a) $ zip [1..15] cvalues
                           where odds = filter (odd . fst) $ map (\(a,b) -> (a, fromIntegral b)) tupList
                                 evens = filter (even . fst) $ map (\(a,b) -> (a, (fromIntegral b)*256)) tupList
                                 tupList = L.zip (L.pack [0..fromIntegral ((L.length palElements)-1)]) palElements
                                 tidy = map snd
                                 cvalues = zipWith (+) (tidy odds) (tidy evens)

printPalette :: [PaletteEntry] -> IO()
printPalette [] = return ()
printPalette (x:xs) = do putStrLn $ show x
                         printPalette xs

showPalette :: UnpackedImage -> IO ()
showPalette image = do putStrLn $ "Data: " ++ (show $ L.length content)
                       case (paletteData content) of 
                          Just palElements -> printPalette $ parsePalette palElements
                          Nothing -> putStrLn "Error getting palette"
                    where content = rawImageData image
