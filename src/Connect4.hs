module Connect4
    ( playMoves
    ) where

import Data.List
import Data.Maybe

data Token = Red | Black
     deriving (Eq)

type Cell = Maybe Token

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

hasN :: Int -> Int -> Cell -> Cells -> Cell
hasN _ _ _ [] = Nothing
hasN n soFar current cells = case (soFar >= n, current, cells) of
    (_, _, []) -> Nothing
    (_, Nothing, x:xs) -> hasN n 0 x xs
    (True, cell, _) -> cell
    (False, cell, x:xs) ->
        if x == current
        then hasN n (soFar + 1) x xs
        else hasN n 1 x xs

cellsWinner :: Cells -> Cell
cellsWinner [] = Nothing
cellsWinner cells = case (head cells, tail cells) of
    (Nothing, xs) -> cellsWinner xs
    (x, xs) -> hasN 4 1 x xs

winner :: Board -> Cell
winner board =
    let winners = (catMaybes (map cellsWinner (board ++ rows board ++ diagonals board ++ diagonals (reverse board))))
    in case winners of
        [] -> Nothing
        x:_ -> Just x


putInBottom :: Token -> Int -> Board -> Board
putInBottom token col board =
    let (left, right) = splitAt col board
    in case uncons right of
        Just (headRight, tailRight) -> left ++ [init (Just token : headRight)] ++ tailRight
        Nothing -> board

movesFrom :: Token -> [Int] -> [[Cells] -> [Cells]]
movesFrom _ [] = []
movesFrom Red cols = putInBottom Red (head cols) : movesFrom Black (tail cols)
movesFrom Black cols = putInBottom Black (head cols) : movesFrom Red (tail cols)

moves :: [Int] -> [[Cells] -> [Cells]]
moves cols = movesFrom Red cols

showCell :: Cell -> String
showCell (Just Red) = "R"
showCell (Just Black) = "B"
showCell Nothing = "."

showRow :: Cells -> String
showRow row = intercalate "  " (map showCell row)

showBoard :: Board -> String
showBoard board = unlines (map showRow (rows board))

showWinner :: Board -> String
showWinner board =
    let winningToken = winner board
    in case winningToken of
        Nothing -> "No winner."
        Just Black -> "Black wins!"
        Just Red -> "Red Wins!"

emptyColumn :: Cells
emptyColumn = replicate 6 Nothing

emptyBoard :: Board
emptyBoard = replicate 7 (emptyColumn)

playMoves :: [Int] -> String
playMoves m =
    let board = foldr ($) emptyBoard (moves m)
    in (showBoard board) ++ (showWinner board)
