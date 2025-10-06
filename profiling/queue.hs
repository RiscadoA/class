module Main where

import Control.Concurrent
import Control.Monad
import System.IO

data CList a = Empty | LCons (a,(LList a))
type LList a = MVar (CList a)
data Opt a = No | One a

data Queue a = Queue (MVar (LList a), MVar (LList a))

newQueue :: IO (Queue a)
newQueue = do
    q <- newMVar (Empty)
    h <- newMVar (q)
    t <- newMVar (q)
    return (Queue (h, t))

enqueue :: (Queue a) -> a -> IO ()
enqueue (Queue (head, tail)) v = do
    ts <- takeMVar tail
    tnone <- takeMVar ts
    ntail <-  newMVar (tnone)
    putMVar ts (LCons (v, ntail))
    putMVar tail ntail    
    return ()

dequeue :: Queue a -> IO (Opt a)
dequeue (Queue (head, tail)) = do
    ts <- takeMVar head
    cc <- takeMVar ts
    case cc of
         Empty -> do
               putMVar ts cc
               putMVar head ts
               return (No)
         LCons (v, next) -> do
                putMVar ts Empty
                putMVar head next
                return (One v)
                
printv ::  Show a => Opt a -> IO ()
printv No = do
           print "NONE"
           return ()
printv (One v) = do
           print v
           return ()
           

producer :: Queue Int -> Int -> IO ()
producer q n = do
        if (n==0)
           then do
                print "DONE"
                return ()
           else do
                enqueue q n
                print n
                producer q (n-1)
                
consumer ::  Queue Int -> Int -> IO ()
consumer q n = do
        if (n==0)
           then do
                print "DONE"
                return ()
           else do
                v <- dequeue q
                printv v
                consumer q (n-1)

mains ::Int -> IO ()
mains n =
     do
        q <- newQueue @Int
        (forkIO (producer q n) >>
            (consumer q n))

main ::  IO()
main = do
     input <- getLine
     let n = read input :: Int
     mains n
