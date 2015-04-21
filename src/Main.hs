module Main where

import Control.Applicative
import System.Directory(getCurrentDirectory, getDirectoryContents)
import System.IO()
import Text.XML.Light
import Debug.Trace

type Word = String
type Sentence = [Word]

-- import Network.HTTP

corpusPath :: FilePath
corpusPath = "corpus/"

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

isRegularFile :: FilePath -> Bool
isRegularFile f = f /= "." && f /= ".."

-- 1. Perform a basic HTTP get request and return the body
-- getContent :: String -> IO String
-- getContent url = simpleHTTP (getRequest url) >>= getResponseBody

addNumbers :: Integer -> Integer -> Integer
addNumbers x y = x + y

-- vorVier :: [Num] -> [Num]
-- vorVier [] = [0]
-- vorVier (x:xs) = filter (<4)

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

calculatePerplexity :: Sentence -> Double
calculatePerplexity sentence = 0.9

main :: IO ()
main = do
  files <- readDir corpusPath
  print files
  let filePaths = map (corpusPath++) files
  print filePaths
  contents <- mapM readFile filePaths
  let sentences = map parseXml contents
  print sentences
  putStrLn "hello"

