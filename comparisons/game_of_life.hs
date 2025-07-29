import Data.List ((!?))

type Table a = [[a]]

indexTable :: Table a -> Table (Int, Int, a)
indexTable t =
    [[(i, j, x) | (j, x) <- zip [0..] xs] | (i, xs) <- zip [0..] t]

mapTable :: (a -> b) -> Table a -> Table b
mapTable _ [] = []
mapTable f (xs : xss) = map f xs : mapTable f xss

tableAt :: Table a -> (Int, Int) -> Maybe a
tableAt t (i, j) = t !? i >>= \row -> row !? j 

-- Game of life data structures and operations

data Cell = Live | Dead deriving (Eq)

instance Show Cell where
    show Live = "1"
    show Dead = "0"

neighborPositions :: (Int, Int) -> [(Int, Int)]
neighborPositions (i, j) =
  [ (i - 1, j - 1),
    (i - 1, j),
    (i - 1, j + 1),
    (i, j - 1),
    (i, j + 1),
    (i + 1, j - 1),
    (i + 1, j),
    (i + 1, j + 1)
  ]

countLive :: Maybe Cell -> Int
countLive Nothing = 0
countLive (Just Live) = 1
countLive (Just Dead) = 0

countLiveNeighbors :: Table Cell -> (Int, Int) -> Int
countLiveNeighbors t (i, j) =
  sum [countLive (tableAt t pos) | pos <- neighborPositions (i, j)]

tickCell :: Cell -> Int -> Cell
tickCell Live n = if n < 2 || n > 3 then Dead else Live
tickCell Dead n = if n == 3 then Live else Dead

tickBoard :: Table Cell -> Table Cell
tickBoard t =
    mapTable (\(i, j, cell) -> tickCell cell (countLiveNeighbors t (i, j))) (indexTable t)

main :: IO ()
main = do
    input <- getContents
    integers <- return $ map read $ words input :: IO [Int]
    let gens = head integers
    let width = integers !! 1
    let height = integers !! 2
    let cells = drop 3 integers

    let initial = [[if cells !! (i * width + j) == 1 then Live else Dead | j <- [0..width-1]] | i <- [0..height-1]]
    let final = iterate tickBoard initial !! gens

    let output = map (map (\cell -> if cell == Live then 1 else 0)) final
    putStrLn $ unlines $ map (unwords . map show) output
