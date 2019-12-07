module Connect4
    ( playMoves
    ) where

import Data.List
import Data.Maybe

data Cell = Red | Black | Empty
     deriving (Eq)

instance Show Cell where
    show Red = "R"
    show Black = "B"
    show Empty = "."

type Cells = [Cell]

type Board = [Cells]

rows :: Board -> [Cells]
rows board = transpose (map reverse board)

diagonals :: Board -> [Cells]
diagonals = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        [] -> transpose ts
        e:es -> go (e:ts) es
        where ts = [t | _:t <- b]

sublists :: (Eq a) => [a] -> [[a]] -> [[a]]
sublists [] xss = xss
sublists xs [] = sublists (tail xs) ([[head xs]])
sublists xs xss
    | (head xs) == (head (head xss)) = sublists (tail xs) (((head xs) : head xss) : tail xss)
    | otherwise = sublists (tail xs) ([head xs] : xss)

hasContiguous :: Int -> Cells -> Maybe Cell
hasContiguous n cells =
    let hasN = filter (\xs -> length xs >= n) (sublists cells [])
    in case hasN of
        [] -> Nothing
        x:xs -> Just (head x)

cellsWinner :: Cells -> Maybe Cell
cellsWinner cells =
    let has4 = hasContiguous 4 cells
    in case has4 of
        Just Empty -> Nothing
        has4 -> has4

winner :: Board -> Maybe Cell
winner board =
    let winners = (catMaybes (map cellsWinner (board ++ rows board ++ diagonals board ++ diagonals (reverse board))))
    in case winners of
        [] -> Nothing
        x:_ -> Just x


putInBottom :: Cell -> Int -> Board -> Board
putInBottom token col board =
    let (left, right) = splitAt col board
    in case uncons right of
        Just (headRight, tailRight) -> left ++ [init (token : headRight)] ++ tailRight
        Nothing -> board

movesFrom :: Cell -> [Int] -> [[Cells] -> [Cells]]
movesFrom _ [] = []
movesFrom Empty _ = []
movesFrom Red cols = putInBottom Red (head cols) : movesFrom Black (tail cols)
movesFrom Black cols = putInBottom Black (head cols) : movesFrom Red (tail cols)

moves :: [Int] -> [[Cells] -> [Cells]]
moves cols = movesFrom Red cols

showRow :: Cells -> String
showRow row = intercalate "  " (map show row)

showBoard :: Board -> String
showBoard board = unlines (map showRow (rows board))

showWinner :: Board -> String
showWinner board =
    let winningToken = winner board
    in case winningToken of
        Nothing -> "No winner."
        Just cell -> (show cell) ++ " wins!"

emptyColumn :: Cells
emptyColumn = replicate 6 Empty

emptyBoard :: Board
emptyBoard = replicate 7 (emptyColumn)

playMoves :: [Int] -> String
playMoves m =
    let board = foldr ($) emptyBoard (moves m)
    in (showBoard board) ++ (showWinner board)
