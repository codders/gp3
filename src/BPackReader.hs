module BPackReader where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified GHC.Word as W
import Control.Exception (bracket, handle)
import Control.Monad
import System.IO

data BPackedImage = BPI { 
                          bit8Marker :: W.Word8,
                          bit16Marker :: W.Word8,
                          size :: Integer,
                          imageData :: L.ByteString
                        }

data UnpackedImage = UPI {
                           rawImageData :: L.ByteString
                         }

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

interpret :: BPackedImage -> W.Word8 -> L.ByteString
interpret bpi byte = if (byte == bit8Marker bpi)
                       then L.singleton byte
                       else if (byte == bit16Marker bpi)
                         then L.singleton byte
                         else L.singleton byte

decompressBytes :: BPackedImage -> L.ByteString
decompressBytes bpi = L.concat $ map (interpret bpi) (L.unpack (imageData bpi))

unpackImage :: BPackedImage -> Maybe UnpackedImage
unpackImage bpi = return $ UPI (decompressBytes bpi)

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

