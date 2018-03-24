import Data.Char
import System.IO
import System.Environment
import System.Exit
import Data.Maybe
-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

instance Functor (ComplicatedA a) where
    fmap f (Con1 a b) = Con1 a (f b)
    fmap f (Con2 xs) = Con2 $ map (fmap (f.)) xs

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance (Functor g) => Functor (ComplicatedB f g a) where
    fmap _ (Con3 x) = Con3 x
    fmap f (Con4 x) = Con4 $ fmap f x
    fmap f (Con5 x) = Con5 $ fmap (fmap (map f)) x

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a, a)
func1' xs = (\a -> (a, a)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a, a)
func2' xs = (,) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a, a)
func3' xs = (\x _ -> (x, x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (+) <$> ((+1) <$> xs) <*> ((+1) <$> ys)

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)
func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = (\x -> if x > 0 then (x, 0) else (0, x)) <$> xs

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

--func7' :: Monad f => f Integer -> f (Integer,Integer)
--func7' xs = 

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = fmap (+x) xs 

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs =  (\x -> (x*x) + 10) <$> xs

-- Exercise 3
data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> (String -> Maybe (a, String))
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = runParser p s >>= (\(a, s') -> if s' == "" then Just a else Nothing)

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser a = P (\x -> Just (a, x))

instance Functor Parser where
    fmap f p = P $ \xs -> (\(a, xs') -> (f a, xs')) <$> runParser p xs 

instance Applicative Parser where
    pure = pureParser
    fp <*> p = P $ \s -> runParser fp s >>= (\(f, s') -> runParser (f <$> p) s')

instance Monad Parser where
    return = pureParser
    p >>= f = P $ \s -> runParser p s >>= (\(a, s') -> runParser (f a) s')

anyChar :: Parser Char
anyChar = P $ \s -> case s of "" -> Nothing
                              c:s' -> Just (c, s')

char :: Char -> Parser ()
char c = do
    c' <- anyChar
    if c == c' then return ()
    else noParser

anyCharBut :: Char -> Parser Char
anyCharBut c = do
    c' <- anyChar
    if c == c' then noParser
    else return c'

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P $ \s -> case runParser p1 s of Nothing -> runParser p2 s
                                                x       -> x
many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many $ p2 >> p1)) `orElse` return []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
    where 
        parseLine = parseCell `sepBy` char ',' <* char '\n'
        parseCell = do
            char '"'
            contents <- many $ anyCharBut '"'
            char '"'
            return contents

-- Exercise 4
type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]
parseINI :: Parser INIFile
parseINI = many $ parseSection
    where 
        parseSection = do
            header <- parseHeader
            declarations <- many $ parseDeclaration `orElse` parseEmpty `orElse` parseComment
            return (header, catMaybes declarations)

        parseIdentifier = many1 $ anyChar >>= \c -> if isAlphaNum c then return c else noParser

        parseHeader = do
            char '['
            id <- parseIdentifier
            char ']'
            char '\n'
            return id
        
        parseComment = char '#' >> (many $ anyCharBut '\n') >> parseEmpty

        parseEmpty = char '\n' >> return Nothing

        parseDeclaration = do
            id <- parseIdentifier
            many $ char ' '
            char '='
            many $ char ' '
            value <- many1 $ anyCharBut '\n'
            char '\n'
            return $ Just (id, value)
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

main :: IO ()
main = do
    args <- getArgs
    input <- case args of [] -> getContents
                          [fileName] -> readFile fileName
                          _ -> hPutStrLn stderr "Too many arguments" >> exitFailure
    case parse parseINI input of 
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI"
            exitFailure
