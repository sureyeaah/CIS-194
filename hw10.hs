import Test.QuickCheck
import Control.Monad
-- Exercise 1
data Tree a = Node (Tree a) (Tree a) | Leaf a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree
genTree :: Arbitrary a => Gen (Tree a)
genTree = sized $ \size ->
    frequency [(1, genLeaf), (size, genNode)]

genLeaf :: Arbitrary a => Gen (Tree a)
genLeaf = Leaf <$> arbitrary

genNode :: Arbitrary a => Gen (Tree a)
genNode = sized $ \size -> do
    x <- choose(0, size)
    Node <$> resize x arbitrary <*> resize (size - x) arbitrary
labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Node t1 t2) = Node <$> go t1 <*> go t2
    go (Leaf _) = Leaf <$> get
-- Exercise 2
size :: Tree a -> Int
size (Leaf _) = 1
size (Node t1 t2) = size t1 + size t2

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node t1 t2) = toList t1 ++ toList t2

-- Exercise 3

prop_lengthToList :: Tree Integer -> Bool
prop_lengthToList t = length (toList t) == size t

prop_sizeLabelTree :: Tree Integer -> Bool
prop_sizeLabelTree t = size t == size (labelTree t)

prop_labelTree :: Tree Integer -> Bool
prop_labelTree t = toList (labelTree t) == take (size t) [0..]

prop_labelTreeIdempotent :: Tree Integer -> Bool
prop_labelTreeIdempotent t = labelTree (labelTree t) == labelTree t
-----
-- Stream
data Stream a = Cons a (Stream a)
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:streamToList xs

instance Show a => Show (Stream a) where
    show s = show (take 20 $ streamToList s)

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) $ streamMap f as

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed $ streamIterate f $ f seed

streamIntersperse :: Stream a -> Stream a -> Stream a
streamIntersperse (Cons a as) bs = Cons a $ streamIntersperse bs as

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = streamIntersperse (streamRepeat 0) (streamMap (+1) ruler)
-- Supply
data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply a = S (\xs -> (a, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S t) = S (\xs -> let (a, ys) = t xs in (f a, ys))

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S t1) (S t2) = S go
    where go xs = (f a b, xs'')
            where (a, xs') = t1 xs
                  (b, xs'') = t2 xs'

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S t) f = S go
        where go xs = t2 xs'
                where (a, xs') = t xs
                      S t2 = f a

runSupply :: Stream s -> Supply s a -> a
runSupply s (S t) = fst . t $ s

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

