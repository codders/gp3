module BPackReader where

import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception (bracket, handle)
import System.IO

data BPackedImage = BPI { imageData :: L.ByteString }

instance Show BPackedImage where
     show im = "Image, size: " ++ show (L.length $ imageData im)

matchHeader :: L.ByteString -> Maybe L.ByteString
matchHeader fileData = case getBytes 4 fileData of
                         Just (head, tail) -> if (head == L8.pack("BPCK"))
                                              then Just tail
                                              else Nothing
                         Nothing -> Nothing

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count         = fromIntegral n 
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

parseImage :: L.ByteString -> Maybe BPackedImage
parseImage fileData = case matchHeader fileData of
                        Just content -> Just $ BPI content
                        Nothing -> Nothing

parseFile :: String -> IO (Maybe BPackedImage)
parseFile fileName = do
        putStrLn $ "Loading from " ++ fileName
        handle (\_ -> return Nothing) $
          bracket (openFile fileName ReadMode) hClose $ \h -> do
            fileData <- L.hGetContents h
            let image = parseImage fileData 
            case image of Just parsedImage -> putStrLn $ "Loaded image: " ++ (show parsedImage)
            return image

