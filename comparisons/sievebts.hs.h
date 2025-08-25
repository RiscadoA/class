module Main where

sieve (p:xs) = p : sieve [x | x <- xs, rem x p > 0]
primes = sieve [2..]

printAllp xs k   =
          if (k==0)
             then do
                  print "DONE."
                  return ()
             else case xs of
                      [] -> return ()
		       x:xs -> do
                               print x
                               printAllp xs (k-1)

main :: IO ()
main = printAllp (primes) 100000
