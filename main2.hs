-- stack runghc
module Main where

-- This is the main function

-- | Type signature
main :: IO ()
main = do
    putStrLn "hello world"

-- 'a' : [] == ['a']
-- 'b' : ['a'] == ['b', 'a']

-- peek :: [a] -> a
-- peek [] = error "Nothing here"
-- peek (x:_) = x

safePeek :: [a] -> Optional a 
safePeek [] = None
safePeek (x:_) = Some x

plus1 :: Int -> Int
plus1 = (+ 1)

some1 :: Optional Int
some1 = Some 1


{-
| Law         | definition                       |
|-------------|----------------------------------|
| identity    | `fmap id = id`                   |
| composition | `fmap (f . g) = fmap f . fmap g` |

class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}

data Identity a = 
    Identity a
     deriving (Eq, Show)

f :: Identity a -> a
f (Identity a) = a

f' :: a -> Identity a
f' a = Identity a

f1 :: Int  -> Int 
f1 i1 = div (i1 * (i1 - 1)) 2

f2 :: Int -> (Int -> Int)
f2 = (+)

f3 :: Int -> Int -> Bool
f3 i = \i2 -> i == 1 && i2 == 2

f4 :: a -> (a, a)
f4 i = (i, i)

-- inf :: a -> [a]
-- inf i = i : inf i

n1 :: Identity Int
n1 = Identity 1
n2 :: Identity Int
n2 = Identity 2
n3 :: Identity Int
n3 = Identity 3
v2 = Identity "Hi"
v3 = Identity True

data Optional a = Some a | None deriving (Eq, Show)

instance Functor Optional where
    fmap f (Some a) = Some (f a)
    fmap f None = None 

data List a = Nil | Cons a (List a) deriving (Eq, Show)

l3 = Cons 1 (Cons 2 (Cons 3 Nil))
l4 = Cons '1' (Cons '2' (Cons '3' Nil))

{-
class Monoid a where
    mempty :: a
    (<>) :: a -> a -> a

instance Monoid String where
    mempty = ""
    s1 <> s2 = s1 ++ s2

newtype Sum = Sum { getInt :: Int }

instance Monoid Sum where
    mempty = Sum 0
    s1 <> s2 = Sum (getInt s1 + getInt s2)
-}

{-
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b

Law 	       | definition
------------------------------------------
left identity  |	return a >>= f = f a
right identity | 	m >>= return = m
associativity  |	(m >>= f) >>= g = m >>= (\x -> fx >>= g)
-}

data Reader r a = Reader { runReader :: (r -> a) }

instance Functor (Reader r) where
    -- fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f reader = Reader $ f . (runReader reader)

instance Applicative (Reader r) where
    pure a = Reader (const a)
    Reader f <*> Reader a = Reader $ \r -> let fi = f r
                                               ai = a r
                                           in  fi ai

instance Monad (Reader r) where
    Reader ra >>= af = Reader $ \r -> let a = ra r
                                          Reader rb = af a
                                          b = rb r
                                      in  b

instance Functor Identity where
    fmap f (Identity i) = Identity $ f i
    
data Five a b c d e = Five a b c d e

instance (Show a, Show b, Show c, Show d, Show e) => Show (Five a b c d e) where
    show (Five a b c d e) = "Five" <> (show a)
                                   <> (show b)
                                   <> (show c)
                                   <> (show d)
                                   <> (show e)

instance Functor (Five a b c d) where
    fmap f (Five a b c d e) = Five a b c d (f e)
    
data Phantom a = Phantom

instance Functor Phantom where
    fmap f (Phantom ) = Phantom
    
data Triple a b c = Triple a b c

instance Functor (Triple a b) where
    fmap f (Triple a b c) = Triple a b (f c)

