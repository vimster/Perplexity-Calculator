module Main where

import System.Random(newStdGen)
import Control.Applicative
import System.Directory(getCurrentDirectory, getDirectoryContents)
import System.IO()
import Text.XML.Light
import qualified Data.Map as Map
import qualified Data.List as List
-- import Debug.Trace

type Word = String
type Sentence = [Word]
type Bigram = (Word, Word)
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
    map ("<s>":) nestedWords

buildFrequencies :: [Sentence] -> Frequencies
buildFrequencies sentences =
  foldl (Map.unionWith (+)) Map.empty $ map buildFrequency sentences

bigrams :: Sentence -> [(Word, Word)]
bigrams sentence = zip sentence $ tail sentence

buildFrequency :: Sentence -> Frequencies
buildFrequency sentence =
  let
    unigramFrequencies = foldl (\ f x -> Map.insertWith (+) (x, "_") 1 f) Map.empty sentence
    bigramFrequencies = foldl (\ f x -> Map.insertWith (+) x 1 f) Map.empty $ bigrams sentence
  in
    Map.unionWith (+) bigramFrequencies unigramFrequencies

lookupFrequency :: Frequencies -> Bigram -> Integer
lookupFrequency frequencies bigram = 
  Map.findWithDefault 0 bigram frequencies + 1

calculatePerplexity :: Fractional f => frequencies -> Sentence -> f
calculatePerplexity frequencies sentence =
  let
    b1 bigram = fromInteger $ lookupFrequency frequencies bigram
    calc bigram@(w1, w2) = b1 bigram / b1 (w2, "_")
  in
    foldl (*) 1.0 $ map calc $ bigrams sentence
  

split :: [a] -> ([a], [a]) 
split x = (take modelSize x, drop modelSize x) 
   where len = fromIntegral $ length x 
         modelSize = truncate $ len * 0.9 

main :: IO ()
main = do
  files <- readDir corpusPath
  gen <- newStdGen
  let filePaths = map (corpusPath++) files
  contents <- mapM readFile filePaths
  let (model, test) = split contents
      modelSentences = concatMap parseXml model
      testSentences = concatMap parseXml test
      frequencies = buildFrequencies modelSentences
      perplexities = map (calculatePerplexity frequencies) testSentences
  print perplexities
  
  putStrLn "hello"

