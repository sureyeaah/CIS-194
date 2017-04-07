import Data.Char
import Data.List
import Data.Function
{-# LANGUAGE OverloadedStrings #-}
-- Exercise 1: Lists, lists, lists
halveEvens :: [Integer] -> [Integer]
halveEvens = map (\c -> c `quot` 2) . filter (\c -> c `mod` 2 == 0)
ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]
safeString :: String -> String
safeString = map go
    where
        go c
          | (isAscii c) && (not $ isControl c) = c
          | otherwise = '_'
ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]
holes :: [a] -> [[a]]
holes xs = zipWith (++) (init $ inits xs) (map (tail) $ init $ tails xs)
ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]
longestText :: Show a => [a] -> a
longestText xs = foldl go (head xs) xs
    where
        go a b
            | (length $ show a) > (length $ show b) = a
            | otherwise = b
ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
    ]
adjacents :: [a] -> [(a,a)]
adjacents xs = zip xs (tail xs)
ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]
commas :: [String] -> String
commas = intercalate ", "
ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]
addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1  (zipWith (+))
ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]
sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter (isDigit.head) . groupBy ((==) `on` isDigit)
sumNumbers2 = head . foldl go [0::Integer,1::Integer] . reverse
    where
        go :: [Integer] -> Char -> [Integer]
        go prev x
            | isDigit x = [(head prev) + (last prev)*(toInteger $ digitToInt x), (last prev)*10]
            | otherwise = [head prev, 1::Integer]
ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]
-- Exercise 2: Word count
wordCount :: String -> String
wordCount xs = unlines[numLines , emptyLines , numWords , uniqueWords , wordsFollowed , longestLine]
    where
        numLines = "Number of lines: " ++ (show $ length $ lines xs)
        emptyLines = "Number of empty lines:" ++ (show $ length $ filter (\c->length c == 0) $ lines xs)
        numWords = "Number of words: " ++ (show $ length $ words xs)
        uniqueWords = "Number of unique words: " ++ (show $ length $ nub $ words xs)
        wordsFollowed = "Number of words followed by themselves: " ++ (show $ length $ filter (\c -> fst c == snd c) $ adjacents $ words xs)
        longestLine = "Length of the longest line: " ++ (show $ maximum $ map length $ lines xs)
-- exercise 3
testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens)
              , ("safeString",      ex_safeString)
              , ("holes",           ex_holes)
              , ("longestText",     ex_longestText)
              , ("adjacents",       ex_adjacents)
              , ("commas",          ex_commas)
              , ("addPolynomials",  ex_addPolynomials)
              ]
formatTests :: [(String, [Bool])] -> String
formatTests xs = unlines $ map (\c -> fst c ++ ": " ++ (show $ length $ filter (\x  -> x) $ snd c) ++ "/" ++ (show $ length $ snd c) ++ "successful tests") xs
main :: IO()
main = putStr $ formatTests testResults