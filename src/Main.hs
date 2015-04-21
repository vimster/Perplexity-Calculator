module Main where

import Control.Applicative
import System.Directory(getCurrentDirectory, getDirectoryContents)
import System.IO()
import Text.XML.Light
import qualified Data.Map as Map
import Debug.Trace

type Word = String
type Sentence = [Word]
type Frequencies = Map (Word, Word) Integer

-- import Network.HTTP

corpusPath :: FilePath
corpusPath = "corpus/"

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".."

-- | read dir
readDir :: String -> IO [FilePath]
readDir path = do
  directory <- getCurrentDirectory
  filter isRegularFile <$> getDirectoryContents (directory ++ "/" ++ path)

parseXml :: String -> [Sentence]
parseXml source = 
  let contents = parseXML source
      sentenceValues = concatMap (findElements $ simpleName "sentence") (onlyElems contents)
      sentences = map (findElements $ simpleName "tok") sentenceValues
      nestedWords = map (map strContent) sentences
      simpleName s = QName s Nothing Nothing
  in
    nestedWords

createFrequencies :: [Sentence] -> Frequencies
createFrequencies sentences = 

lookupFrequency :: Frequencies -> Word -> Word -> Integer
lookupFrequency frequencies w1 w2 = 

calculatePerplexity :: Sentence -> Double
calculatePerplexity sentence = 0.9

main :: IO ()
main = do
  files <- readDir corpusPath
  let filePaths = map (corpusPath++) files
  contents <- mapM readFile filePaths
  let sentences = map parseXml contents
  print sentences
  putStrLn "hello"

