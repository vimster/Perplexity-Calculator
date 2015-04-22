module Main where

import System.Random
import Control.Applicative
import System.Directory(getCurrentDirectory, getDirectoryContents)
import System.IO()
import Text.XML.Light
import qualified Data.Map as Map
import qualified Data.List as List
-- import Debug.Trace

type Word = String
type Sentence = [Word]
type Frequencies = Map.Map (Word, Word) Integer

testSentence = ["Ein", "Haus", "im", "Wald"]
testSentence1 = ["Alles", "Klar", "Ein", "Haus"]
testSentences = [testSentence, testSentence1]

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

buildFrequencies :: [Sentence] -> Frequencies
buildFrequencies sentences =
  foldl (Map.unionWith (+)) Map.empty $ map buildFrequency sentences

buildFrequency :: Sentence -> Frequencies
buildFrequency sentence =
  let
    list = "<s>" : sentence
    pairs = zip list $ tail list
    bigramFrequencies = foldl (\ f x -> Map.insertWith (+) x 1 f) Map.empty pairs
    unigramFrequencies = foldl (\ f x -> Map.insertWith (+) ("_", x) 1 f) Map.empty sentence
  in
    Map.unionWith (+) bigramFrequencies unigramFrequencies
  

-- lookupFrequency :: Frequencies -> Word -> Word -> Integer
-- lookupFrequency frequencies w1 w2 = 

calculatePerplexity :: Sentence -> Double
calculatePerplexity sentence = 0.9

main :: IO ()
main = do
  files <- readDir corpusPath
  gen <- newStdGen
  let filePaths = map (corpusPath++) files
  contents <- mapM readFile filePaths
  let sentences = map parseXml contents
  print sentences
  putStrLn "hello"

