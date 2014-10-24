module Gen
where

import qualified Data.List as L
import qualified System.Random as R

import qualified Func as F

data Size = Size { width, height :: Integer
                 } deriving (Eq, Show)

data Difficulty = Difficulty { size :: Size
                             , mineCount :: Integer
                             } deriving (Eq, Show)

type Seed = Int

data Settings = Settings { seed :: Seed
                         , difficulty :: Difficulty
                         }

beginnerDifficulty :: Difficulty
beginnerDifficulty = Difficulty (Size 9 9) 10
intermediateDifficulty :: Difficulty
intermediateDifficulty = Difficulty (Size 16 16) 40
advancedDifficulty :: Difficulty
advancedDifficulty = Difficulty (Size 16 30) 99

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
unshuffledGeneratedMines (Difficulty (Size x y) n) = concat [(L.genericReplicate paddingCount False), (L.genericReplicate n True)]
  where
    paddingCount :: Integer
    paddingCount = x*y - n

shuffledGeneratedMines :: (R.RandomGen r) => r -> Difficulty -> [F.MineCell]
shuffledGeneratedMines gen = (shuffle gen) . unshuffledGeneratedMines

gridify :: Integral i => i -> [a] -> [[a]]
gridify w _
  | w <= 0 = error "Strictly positive width required."
gridify _ [] = []
gridify w xs = firstRow:(gridify w rest)
  where
    (firstRow, rest) = L.genericSplitAt w xs

generateMineGridFromRandomGen :: (R.RandomGen r) => r -> Difficulty -> F.MineGrid
generateMineGridFromRandomGen gen d@(Difficulty (Size w _) _) = gridify w minesList
  where minesList = shuffledGeneratedMines gen d

generateMineGridFromDifficulty :: Maybe Seed -> Difficulty -> IO F.MineGrid
generateMineGridFromDifficulty mSeed d = do
  settings <- settingsMaybeSeed mSeed d
  return $ generateMineGrid settings

generateMineGrid :: Settings -> F.MineGrid
generateMineGrid (Settings s d) = generateMineGridFromRandomGen (makeRandomGeneratorFromSeed s) d

randomSeed :: IO Seed
randomSeed = R.randomIO

ensureSeed :: Maybe Seed -> IO Seed
ensureSeed mSeed = case mSeed of
                     Just s -> return s
                     Nothing -> randomSeed

settingsMaybeSeed :: Maybe Seed -> Difficulty -> IO Settings
settingsMaybeSeed mSeed d = do
  s <- ensureSeed mSeed
  return $ Settings s d

makeRandomGeneratorFromSeed :: Seed -> R.StdGen
makeRandomGeneratorFromSeed = R.mkStdGen

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."
          putStrLn ""
          putStrLn "Example of a random beginner grid:"
          randomGrid <- generateMineGridFromDifficulty Nothing beginnerDifficulty
          print randomGrid
          putStrLn ""
          putStrLn $ "Example of a beginner grid with seed of '" ++ (show givenSeed) ++ "':"
          seededGrid <- generateMineGridFromDifficulty (Just givenSeed) beginnerDifficulty
          print seededGrid
  where givenSeed = 42
