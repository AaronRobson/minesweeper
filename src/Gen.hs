module Gen
( generateMineGrid
) 
where

import Func (MineGrid)

type Size = (Integer,Integer)

data Settings = Settings { size :: Size
                         , mineCount :: Integer
                         }

generateMineGrid :: MineGrid
generateMineGrid = undefined

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."