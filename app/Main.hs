module Main where

import Connect4

basicVertical = [0, 3, 0, 3, 0, 3, 0]
basicHorizontal = [1, 1, 2, 2, 3, 3, 5, 4, 5, 4]
basicDiagonal = [6, 0, 1, 1, 2, 2, 3, 2, 3, 3, 0, 3]
basicDiagonal2 = [5, 4, 4, 3, 2, 3, 3, 2, 2, 1, 2]

main :: IO ()
main = putStrLn (playMoves basicDiagonal2)
