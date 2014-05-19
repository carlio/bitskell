import Control.Monad
import Data.Char

data BValue = BString String | BInt Int deriving (Show)


extractInt ('e':xs) = []
extractInt (x:xs) = x:(extractInt xs)


parseInt :: String -> BInt
parseInt x = BInt (read (extractInt x) :: Int)


extractChars 0 (x:xs) = []
extractChars count [] = []
extractChars count (x:xs) = x:(extractChars (count-1) xs)


readString :: Int -> String -> String
readString accum (x:xs)
  | isDigit x = readString ((10 * accum) + (read [x] :: Int)) xs
  | otherwise = extractChars accum (x:xs)


parseString :: String -> BString
parseString x = BString $ readString 0 x

-- parseList x = "list"


discoverNext :: String -> (String -> BValue)
discoverNext ('i':xs) = parseInt
discoverNext (x:xs) = parseString


parseNext :: String -> [BValue]
parseNext [] = []
parseNext xs = 
    let f = discoverNext xs
    in
        

-- parse :: String -> BValue
-- parse ('i':xs) = BInt (read (parseInt xs) :: Int)
-- parse (x:xs) = parseString (x:xs)

parse_ :: String -> String
parse_ x = show (parse x)


-- main = print (extractLen 0 "11eeeeohergiuherg")
-- main = print (extractChars 5 "bananananana")

main = interact parse_

