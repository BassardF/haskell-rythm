import Data.List
import System.IO
import Control.Monad.Random

addSilencesToBars :: [String] -> [Int] -> Int -> [String]
addSilencesToBars [] [] _ = []
addSilencesToBars (x:xs) (x2:xs2) percent
  | x2 <= percent = x : addSilencesToBars xs xs2 percent
  | otherwise = "x" : addSilencesToBars xs xs2 percent

replaceHalfTheXs :: [String] -> [Int] -> [String]
replaceHalfTheXs [] [] = []
replaceHalfTheXs (x:xs) (x2:xs2)
  | x == "x" && x2 >= 50 = x : replaceHalfTheXs xs xs2
  | x == "x" && x2 < 50 = "-" : replaceHalfTheXs xs xs2
  | otherwise = x : replaceHalfTheXs xs xs2

-- Random function generator
die :: RandomGen g => Rand g Int
die = getRandomR (1,100)

dice :: RandomGen g => Int -> Rand g [Int]
dice n = sequence (replicate n die)

main = do
  -- Number of notes per bar
  putStrLn "How many notes per bar ?"
  notesPerBar <- getLine
  let notesPerBarInt = (read notesPerBar :: Int)
  -- Number of bars total
  putStrLn "How many bars ?"
  barCount <- getLine
  let barCountInt = (read barCount :: Int)
  -- Density of strokes
  putStrLn "Percent of stroke density ?"
  density <- getLine
  let densityInt = (read density :: Int)
  -- User output
  putStrLn ("=> Generating...")
  putStrLn (barCount ++ " bars")
  putStrLn ("with " ++ density ++ "% stroke density")
  -- Calculatin random rolls for xs
  rollsX <- evalRandIO $ dice (barCountInt * notesPerBarInt)
  -- Calculatin random rolls for xs to scratches
  rollsXtoScratch <- evalRandIO $ dice (barCountInt * notesPerBarInt)
  -- Generating the bars
  let multiBars = take (barCountInt*notesPerBarInt) (cycle(["D", "U"]))
  let resultWithXs = addSilencesToBars multiBars rollsX densityInt
  let resultWithXAndScratches = replaceHalfTheXs resultWithXs rollsXtoScratch
  -- User display
  putStrLn (show(resultWithXAndScratches))
