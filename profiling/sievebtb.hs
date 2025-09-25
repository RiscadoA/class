module Main where

import Prelude hiding (filter, map, concatMap, reverse, append, take)

data List a = Nil | Cons a (List a)
    deriving Show

filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

reverse :: List a -> List a 
reverse Nil = Nil
reverse (Cons x xs) = reverse xs `append` Cons x Nil

sieve :: List Int -> List Int
sieve (Cons x xs) = Cons x (sieve (filter (\y -> rem y x > 0) xs))

natsFrom2 :: List Int
natsFrom2 = Cons 2 (map (+1) natsFrom2)
primes :: List Int
primes = sieve natsFrom2

take :: Int -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

printAllp xs k   =
          if (k==0)
             then do
                  print "DONE."
                  return ()
             else case xs of
                      Nil -> return ()
                      Cons x xs -> do
                               print x
                               printAllp xs (k-1)

main :: IO ()
main = do
  input <- getLine
  let n = read input :: Int
  printAllp primes n
