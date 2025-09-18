module Main where

data MyList a = Empty | LCons a (MyList a)
  deriving (Show)

intsfm k = LCons k (intsfm (k+1))

filterp (LCons n xs) p = if (rem n p == 0)
                     then filterp xs p
                     else LCons n (filterp xs p)

sievep (LCons p xs) = LCons p (sievep (filterp xs p))

sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]
primes = sieve [2..]

primesp = sievep (intsfm 2)

printAllp :: Show a => (MyList a) -> Int -> IO ()
printAllp xs k   =
          if (k==0)
             then do
                  print "DONE."
                  return ()
             else case xs of
                       Empty -> return ()
                       LCons x xs -> do
                                print x
                                printAllp xs (k-1)


main :: IO ()
main = printAllp (primesp) 100000
