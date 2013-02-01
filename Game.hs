module Main where

import System.Random
import TicTacToe

-- | Game entry point
main :: IO ()
main = do
   -- get the rnd for the game
   gen <- getStdGen
   -- make the initial field
   let board = readField initialField
   -- play the game
   playGame gen Cross board
