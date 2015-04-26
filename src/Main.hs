module Main where

import           Control.Applicative
import qualified Data.List             as List ()
import qualified Data.Map              as Map
import           System.Directory      (getCurrentDirectory,
                                        getDirectoryContents)
import           System.IO             ()
import           System.Random         (newStdGen)
import qualified System.Random.Shuffle as Shuffler
import           Text.XML.Light
-- import Debug.Trace
--

------------------------------------------------------------------------
--  Constants
------------------------------------------------------------------------
modelTestRatio :: Double
modelTestRatio = 0.9


------------------------------------------------------------------------
--  types
------------------------------------------------------------------------
type Word = String
type Sentence = [Word]
type Bigram = (Word, Word)
type Frequencies = Map.Map (Word, Word) Integer


------------------------------------------------------------------------
--  test data
------------------------------------------------------------------------
testSentence :: Sentence
testSentence = ["Ein", "Haus", "im", "Wald"]
testSentence1 :: Sentence
testSentence1 = ["Alles", "Klar", "Ein", "Haus"]
testSentences :: [Sentence]
testSentences = [testSentence, testSentence1]
testFrequencies :: Frequencies
testFrequencies = Map.fromList [(("a", "a"), 0),(("b", "b"), 2), (("c", "c"), 1)]


------------------------------------------------------------------------
--  IO
------------------------------------------------------------------------
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


------------------------------------------------------------------------
--  perplexity calculation
------------------------------------------------------------------------

bigrams :: Sentence -> [(Word, Word)]
bigrams sentence = zip sentence $ tail sentence

frequencPerCorups :: [Sentence] -> Frequencies
frequencPerCorups = foldl (Map.unionWith (+)) Map.empty . map frequenciesPerSentence

frequenciesPerSentence :: Sentence -> Frequencies
frequenciesPerSentence sentence =
  let
    unigramFrequencies = foldl (\ f x -> Map.insertWith (+) (x, "_") 1 f) Map.empty sentence
    bigramFrequencies = foldl (\ f x -> Map.insertWith (+) x 1 f) Map.empty $ bigrams sentence
  in
    Map.unionWith (+) bigramFrequencies unigramFrequencies

lookupFrequency :: Frequencies -> Bigram -> Integer
lookupFrequency frequencies bigram =
  Map.findWithDefault 0 bigram frequencies + 1

perplexity :: Frequencies -> Sentence -> Double
perplexity frequencies sentence =
  let
    probability = fromInteger . lookupFrequency frequencies
    calc bigram@(_, w2) = probability bigram / probability (w2, "_")
    p = product $ map calc $ bigrams sentence
    size = fromIntegral $ length sentence - 1
  in
    p ** (-1/size)


split :: [a] -> ([a], [a])
split x = (take modelSize x, drop modelSize x)
   where len = fromIntegral $ length x
         modelSize = truncate $ len * modelTestRatio

shuffle :: [a] -> IO [a]
shuffle list = do
  gen <- newStdGen
  return $ Shuffler.shuffle' list (length list) gen


------------------------------------------------------------------------
--  main
------------------------------------------------------------------------
main :: IO ()
main = do
  files <- readDir corpusPath
  filePaths <- shuffle $ map (corpusPath++) files
  putStrLn "Reading files ---------------------------------"
  contents <- mapM readFile filePaths
  putStrLn "Calculating   ---------------------------------"
  let (model, test) = split contents
      modelSentences = concatMap parseXml model
      testModelSentences = concatMap parseXml test
      frequencies = frequencPerCorups modelSentences
      perplexities = map (perplexity frequencies) testModelSentences
  -- print perplexities
  putStr "Average perplexity: "
  print $ (sum perplexities) / (fromIntegral $ length perplexities)


