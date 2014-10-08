module Gen
where

import qualified Func as F

type Size = (Integer,Integer)

data Settings = Settings { size :: Size
                         , mineCount :: Integer
                         }

generateMineGrid :: F.MineGrid
generateMineGrid = undefined

main :: IO ()
main = do putStrLn "Generator of Minesweeper Grids."
