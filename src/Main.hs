module Main where
import Text.XML.Light()

-- import Network.HTTP

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

-- 1. Perform a basic HTTP get request and return the body
-- getContent :: String -> IO String
-- getContent url = simpleHTTP (getRequest url) >>= getResponseBody

addNumbers :: Integer -> Integer -> Integer
addNumbers x y = x + y

-- vorVier :: [Num] -> [Num]
-- vorVier [] = [0]
-- vorVier (x:xs) = filter (<4)

readDir :: String -> String
readDir _ = "asdf"
-- getDirectoryContents -> readFile
--

parseXml :: String -> String
parseXml _ = "asdf"
-- let contents = parseXML source
--     quotes   = concatMap (findElements $ simpleName "StockQuote") (onlyElems contents)
--     symbols  = map (findAttr $ simpleName "Symbol") quotes
--     simpleName s = QName s Nothing Nothing
-- print symbols

main :: IO ()
main = putStrLn "hello"

