module Gen
where

import qualified Func as F

type Size = (Integer,Integer)

data Settings = Settings { size :: Size
                         , mineCount :: Integer
                         }

beginnerSettings = Settings (9,9) 10
intermediateSettings = Settings (16,16) 40
advancedSettings = Settings (16,30) 99

generateMineGrid :: Settings -> F.MineGrid
generateMineGrid = undefined

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."
