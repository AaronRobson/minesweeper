module Gen
where

import qualified Data.List as L
import qualified System.Random as R

import qualified Func as F

type Size = (Integer,Integer)

data Settings = Settings { size :: Size
                         , mineCount :: Integer
                         } deriving (Eq, Show)

beginnerSettings :: Settings
beginnerSettings = Settings (9,9) 10
intermediateSettings :: Settings
intermediateSettings = Settings (16,16) 40
advancedSettings :: Settings
advancedSettings = Settings (16,30) 99

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

unshuffledGeneratedMines :: Settings -> [F.MineCell]
unshuffledGeneratedMines (Settings (x,y) n) = concat [(L.genericReplicate paddingCount False), (L.genericReplicate n True)]
  where
    paddingCount :: Integer
    paddingCount = x*y - n

shuffledGeneratedMines :: (R.RandomGen r) => r -> Settings -> [F.MineCell]
shuffledGeneratedMines gen = (shuffle gen) . unshuffledGeneratedMines

gridify :: Integral i => i -> [a] -> [[a]]
gridify width _
  | width <= 0 = error "Strictly positive width required."
gridify _ [] = []
gridify width xs = firstRow:(gridify width rest)
  where
    (firstRow, rest) = L.genericSplitAt width xs

generateMineGridFromRandomGen :: (R.RandomGen r) => r -> Settings -> F.MineGrid
generateMineGridFromRandomGen gen settings@(Settings (width,_) _) = gridify width minesList
  where minesList = shuffledGeneratedMines gen settings

generateMineGridFromSeed :: Int -> Settings -> F.MineGrid
generateMineGridFromSeed = generateMineGridFromRandomGen . R.mkStdGen

generateMineGrid :: Settings -> IO F.MineGrid
generateMineGrid settings = do gen <- R.newStdGen
                               return $ generateMineGridFromRandomGen gen settings

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."
          putStrLn ""
          putStrLn "Example of a random beginner grid:"
          randomGrid <- generateMineGrid beginnerSettings
          print randomGrid
          putStrLn ""
          putStrLn $ "Example of a beginner grid with seed of '" ++ (show seed) ++ "':"
          print seededGrid
  where seed = 42
        seededGrid = generateMineGridFromSeed seed beginnerSettings
