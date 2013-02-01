module TicTacToe where

import System.Random

import Data.List
import Data.List.Split
import Data.Maybe

import Test.QuickCheck

initialField = ["..."
               ,"..."
               ,"..."]

-- | A piece is what a player places in a cell
data Piece = Naught | Cross
   deriving (Eq, Show, Read)

-- | A cell is empty or it contains a piece
type Cell = Maybe Piece

-- | A field is a 3x3 array of cells
type Field = [[Cell]]

-- | Converts a piece into the board representation
showPiece :: Piece -> Char
showPiece Naught = 'o'
showPiece Cross  = 'x'

-- | Converts the board representation back into a piece
readPiece :: Char -> Piece
readPiece 'o' = Naught
readPiece 'x' = Cross

-- | Shows the value of a cell
showCell :: Cell -> Char
showCell Nothing  = '.'
showCell (Just p) = showPiece p

-- | Reads a board cell and transforms
readCell :: Char -> Cell
readCell '.' = Nothing
readCell p   = Just $ readPiece p

-- | Shows a field of cells
showField :: Field -> [String]
showField = map showCellRow
   where showCellRow = map showCell

-- | Reads a field
readField :: [String] -> Field
readField = map readCellRow
   where readCellRow = map readCell

-- | Tests a prepared row for a winner
testRow :: [Cell] -> Maybe Piece
testRow [Just Naught, Just Naught, Just Naught] = Just Naught
testRow [Just Cross, Just Cross, Just Cross]    = Just Cross
testRow _                                       = Nothing

-- | Flattens the winnable view of a field into testable rows
makeRows :: Field -> [[Cell]]
makeRows f = horiz ++ vert ++ diag
   where horiz = [r | r <- f]
         col1  = [r | r <- head f]
         col2  = [r | r <- head $ drop 1 f]
         col3  = [r | r <- head $ drop 2 f]
         vert  = [col1, col2, col3]
         diag  = [[head col1, head $ drop 1 col2, head $ drop 2 col3]
                 ,[head $ drop 2 col1, head $ drop 1 col2, head col3]]

-- | Tests all rows in a field and returns the first result
hasWinner :: Field -> Maybe Piece
hasWinner f = head $ res ++ [Nothing]
   where rs  = makeRows f
         res = filter (/= Nothing) $ map (\r -> testRow r) rs

-- | Places a piece on the board
placePiece :: Field -> Piece -> Int -> Field
placePiece f p n = chunksOf 3 $ prior ++ [cell] ++ after
   where flat = concat f
         prior = take n $ flat
         after = drop (n + 1) $ flat
         cell = (Just p) :: Cell

-- | Gathers all of the positions that are free
getOptions :: Field -> [Int]
getOptions f = [snd x | x <- zip (concat f) [0..], fst x == Nothing]

-- | Takes a random turn on a field
takeRandomTurn :: StdGen -> Piece -> Field -> (StdGen, Field)
takeRandomTurn gen p f = (seed, placePiece f p (opts !! idx))
   where opts        = getOptions f
         (idx, seed) = randomR(0, (length opts) - 1) gen :: (Int, StdGen)

-- | Acts like a boolean "not" for tic-tac-toe pieces
otherPiece :: Piece -> Piece
otherPiece Cross = Naught
otherPiece Naught = Cross

-- | Plays a game between two random cpu players
playGame :: StdGen -> Piece -> Field -> IO ()
playGame gen p f = if spaces == 0
                      then putStrLn "Game was a tie!"
                      else if winner /= Nothing
                              then do
                                 putStrLn $ (unlines fstr)
                                 putStrLn $ (show winner) ++ " won the game!"
                              else do
                                 putStrLn $ (unlines fstr)
                                 playGame ng (otherPiece p) nf
   where winner   = hasWinner f
         spaces   = length $ getOptions f
         (ng, nf) = takeRandomTurn gen p f
         fstr     = showField f

allCrosses :: [Cell]
allCrosses = take 3 $ repeat (Just Cross)

allNaughts :: [Cell]
allNaughts = take 3 $ repeat (Just Naught)

allNothing :: [Cell]
allNothing = take 3 $ repeat (Nothing :: Cell)

emptyField :: Field
emptyField = readField initialField

diagonalWinner :: Piece -> Field
diagonalWinner p = [[Just p, Nothing, Nothing]
                   ,[Nothing, Just p, Nothing]
                   ,[Nothing, Nothing, Just p]]

runTests = do
   quickCheck (readPiece (showPiece Naught) == Naught)
   quickCheck (readPiece (showPiece Cross) == Cross)
   quickCheck (readCell (showCell Nothing) == Nothing)
   quickCheck (readCell (showCell (Just Cross)) == (Just Cross))
   quickCheck (readCell (showCell (Just Naught)) == (Just Naught))
   quickCheck (showField (readField initialField) == initialField)
   quickCheck (testRow allCrosses == (Just Cross))
   quickCheck (testRow allNaughts == (Just Naught))
   quickCheck (testRow allNothing == Nothing)
   quickCheck (allNothing `elem` (makeRows emptyField))
   quickCheck (allCrosses `elem` (makeRows (diagonalWinner Cross)))

-- main :: IO ()
-- main = runTests
