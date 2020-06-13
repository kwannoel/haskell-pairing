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
-}

data Identity a = 
    Identity a
     deriving (Eq, Show)

f :: Identity a -> a
f (Identity a) = a

f' :: a -> Identity a
f' a = Identity a

instance Functor Identity where
    -- fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity (f a)  

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

f1' = fmap f1 n1

data Optional a = Some a | None deriving (Eq, Show)

instance Functor Optional where
    fmap f (Some a) = Some (f a)
    fmap f None = None 

data List a = Nil | Cons a (List a) deriving (Eq, Show)

l3 = Cons 1 (Cons 2 (Cons 3 Nil))
l4 = Cons '1' (Cons '2' (Cons '3' Nil))

instance Functor List where
    fmap f (Cons a as ) 