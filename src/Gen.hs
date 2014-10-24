module Gen
where

import qualified Data.List as L
import qualified System.Random as R

import qualified Func as F

type Size = (Integer,Integer)

data Difficulty = Difficulty { size :: Size
                             , mineCount :: Integer
                             } deriving (Eq, Show)

beginnerDifficulty :: Difficulty
beginnerDifficulty = Difficulty (9,9) 10
intermediateDifficulty :: Difficulty
intermediateDifficulty = Difficulty (16,16) 40
advancedDifficulty :: Difficulty
advancedDifficulty = Difficulty (16,30) 99

-- Return the chosen element and the remainder of the list in a tuple.
chooseIndex :: [a] -> Integer -> (a,[a])
chooseIndex xs index
  | index < 0 = error "Indexes below zero are invalid."
  | pred len < index = error "Index too large for the length of the List."
  | otherwise = (element,rest)
  where
    len = L.genericLength xs
    element = L.genericIndex xs index
    rest = concat [before, after]
    before = L.genericTake index xs
    after = L.genericDrop (succ index) xs

chooseRandom :: (R.RandomGen r) => r -> [a] -> (a,[a],r)
chooseRandom gen xs = (randomItem,otherItems,newGen)
  where
    (randomIndex,newGen) = R.randomR (0, pred $ L.genericLength xs) gen
    (randomItem,otherItems) = chooseIndex xs randomIndex

shuffle :: (R.RandomGen r) => r -> [a] -> [a]
shuffle _ [] = []
shuffle _ [x] = [x]
shuffle gen xs = chosen:shuffledRest
  where
    (chosen,rest,newGen) = chooseRandom gen xs
    shuffledRest = shuffle newGen rest

unshuffledGeneratedMines :: Difficulty -> [F.MineCell]
unshuffledGeneratedMines (Difficulty (x,y) n) = concat [(L.genericReplicate paddingCount False), (L.genericReplicate n True)]
  where
    paddingCount :: Integer
    paddingCount = x*y - n

shuffledGeneratedMines :: (R.RandomGen r) => r -> Difficulty -> [F.MineCell]
shuffledGeneratedMines gen = (shuffle gen) . unshuffledGeneratedMines

gridify :: Integral i => i -> [a] -> [[a]]
gridify width _
  | width <= 0 = error "Strictly positive width required."
gridify _ [] = []
gridify width xs = firstRow:(gridify width rest)
  where
    (firstRow, rest) = L.genericSplitAt width xs

generateMineGridFromRandomGen :: (R.RandomGen r) => r -> Difficulty -> F.MineGrid
generateMineGridFromRandomGen gen difficulty@(Difficulty (width,_) _) = gridify width minesList
  where minesList = shuffledGeneratedMines gen difficulty

generateMineGrid :: Maybe Int -> Difficulty -> IO F.MineGrid
generateMineGrid mSeed difficulty = do
  seed <- ensureSeed mSeed
  return $ generateMineGridFromRandomGen (makeRandomGeneratorFromSeed seed) difficulty

randomSeed :: IO Int
randomSeed = R.randomIO

ensureSeed :: Maybe Int -> IO Int
ensureSeed mSeed = case mSeed of
                     Just i -> return i
                     Nothing -> randomSeed

makeRandomGeneratorFromSeed :: Int -> R.StdGen
makeRandomGeneratorFromSeed = R.mkStdGen

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."
          putStrLn ""
          putStrLn "Example of a random beginner grid:"
          randomGrid <- generateMineGrid Nothing beginnerDifficulty
          print randomGrid
          putStrLn ""
          putStrLn $ "Example of a beginner grid with seed of '" ++ (show seed) ++ "':"
          seededGrid <- generateMineGrid (Just seed) beginnerDifficulty
          print seededGrid
  where seed = 42
