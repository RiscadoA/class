{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
type Tile = Int

data Board = Board
  { readBoard :: Int -> Int -> Tile
  , writeBoard :: Int -> Int -> Tile -> Board
  }

writtenBoard :: Board -> Int -> Int -> Tile -> Board
writtenBoard b x y t =
  Board
    { readBoard = \rx ry -> if rx == x && ry == y then t else readBoard b rx ry
    , writeBoard = \wx wy wt ->
        if wx == x && wy == y
          then writtenBoard b x y wt
          else writtenBoard (writeBoard b wx wy wt) x y t
    }

emptyBoard :: Board
emptyBoard =
  Board
    { readBoard = \_ _ -> 0
    , writeBoard = \x y t -> if t == 0 then emptyBoard else writtenBoard emptyBoard x y t
    }

tickTile :: Board -> Board -> Int -> Int -> Board
tickTile pb nb x y =
  let prev = readBoard pb x y
      liveNeighbors =
        sum [
          readBoard pb (x - 1) (y - 1),
          readBoard pb (x - 1) y,
          readBoard pb (x - 1) (y + 1),
          readBoard pb x (y - 1),
          readBoard pb x (y + 1),
          readBoard pb (x + 1) (y - 1),
          readBoard pb (x + 1) y,
          readBoard pb (x + 1) (y + 1)
        ]
      new =
        case (prev, liveNeighbors) of
          (1, n) | n < 2 -> 0
          (1, n) | n > 3 -> 0
          (1, _) -> 1
          (0, 3) -> 1
          (0, _) -> 0
    in writeBoard nb x y new

tickBoard :: Board -> Board -> Int -> Int -> Int -> Board
tickBoard pb nb side x y
  | x >= side = nb
  | y >= side = tickBoard pb nb (side) (x + 1) 0
  | otherwise = tickBoard pb (tickTile pb nb x y) side x (y + 1)

tickBoardGens :: Board -> Int -> Int -> Board
tickBoardGens b side 0 = b
tickBoardGens b side gens = tickBoardGens (tickBoard b emptyBoard side 0 0) side (gens - 1)

main :: IO ()
main = do
  input <- getContents
  integers <- return $ map read $ words input :: IO [Int]
  let gens = integers !! 0
  let side = integers !! 1
  let tiles = drop 2 integers

  -- Write all tiles to the input board
  let inputBoard = foldl (\b (i, t) -> writeBoard b (i `mod` side) (i `div` side) t) emptyBoard (zip [0 ..] tiles)
  
  -- Compute the output board and print it
  let outputBoard = tickBoardGens inputBoard side gens
  putStrLn $ unlines [unwords [show (readBoard outputBoard x y) | x <- [0 .. side - 1]] | y <- [0 .. side - 1]]
