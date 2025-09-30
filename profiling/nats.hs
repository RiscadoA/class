module Main where

data Nat = Zero | Succ (Nat)

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add n m)

one = (Succ Zero)

mul :: Nat -> Nat -> Nat
mul Zero n = Zero
mul (Succ n) m = add n (mul n m)

expn :: Nat -> Nat -> Nat
expn Zero n = one
expn (Succ n) m = mul n (expn n m)

mknat :: Int -> Nat
mknat 0 = Zero
mknat n = Succ (mknat (n-1))

printnat :: Nat  -> IO ()
printnat Zero   = do
                  print "0"
                  return ()
printnat (Succ n)  =
               do
                print "S"
                printnat n

main :: IO ()
main = printnat (expn (mknat 100) (mknat 6))



