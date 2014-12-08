module Func
where

import Data.List (genericLength, genericIndex)
import qualified Data.Array.IArray as IA
import qualified Data.Maybe as M
import qualified Data.Default as Def -- cabal install data-default

type Grid = IA.Array (Integer,Integer) GridCell
data GridCell = GridCell { mine :: MineCell
                         , marking :: Marking
                         } deriving (Eq, Show)
-- http://byorgey.wordpress.com/2010/04/03/haskell-anti-pattern-incremental-ad-hoc-parameter-abstraction/
instance Def.Default GridCell where
  def = GridCell False Normal

newGridCell :: MineCell -> GridCell
newGridCell m = Def.def {mine = m}

minesToGrid :: MineGrid -> Grid
minesToGrid = fmap newGridCell

-- Lossy transformation.
gridToMines :: Grid -> MineGrid
gridToMines = fmap mine

type MineGrid = IA.Array (Integer,Integer) MineCell

type MineCell = Bool

data Marking = Flag | QuestionMark | Normal | Revealed deriving (Eq, Show)

data Progress = InProgress | Won | Lost deriving (Eq, Show)

progress :: Grid -> Progress
progress g
  | won g = Won
  | lost g = Lost
  | otherwise = InProgress
  where
    won :: Grid -> Bool 
    won grid = all cellValid $ IA.elems grid
      where
        cellValid :: GridCell -> Bool
        cellValid gc = mine gc /= (Revealed == marking gc)
    lost :: Grid -> Bool
    lost grid = any cellInvalid $ IA.elems grid
      where
        cellInvalid :: GridCell -> Bool
        cellInvalid gc = mine gc == (Revealed == marking gc)

finished :: Grid -> Bool
finished g = InProgress /= progress g

type Location = (Integer,Integer)

numberOfMinesAround :: Grid -> Location -> Integer
numberOfMinesAround grid (x,y) = genericLength . filter (locationIsMine grid) $ locationsAround
  where
    locationsAround :: [Location]
    locationsAround =
      [ (x-1,y-1)
      , (x+0,y-1)
      , (x+1,y-1)

      , (x-1,y+0)
      -- Skip the given Location.
      , (x+1,y+0)

      , (x-1,y+1)
      , (x+0,y+1)
      , (x+1,y+1)
      ]

findCell :: Grid -> Location -> Maybe GridCell
findCell grid (x,y) = (if (x,y) `elem` IA.indices grid
                         then Just $ grid IA.! (x,y)
                         else Nothing)

locationIsMine :: Grid -> Location -> Bool
locationIsMine grid location = maybeCellIsMine cell
  where
    cell = findCell grid location

maybeCellIsMine :: Maybe GridCell -> Bool
maybeCellIsMine = M.maybe False mine

main :: IO ()
main = do putStrLn "The core functionality of the program is in this module."
          putStrLn ""
          putStr "Grid: "
          print mg
          mapM_ (putStrLn . findCellInfo) [ (-1,-1)
                                          , (0,0)
                                          , (1,1)
                                          , (2,2)
                                          , (3,3)
                                          ]
          mapM_ (putStrLn . numMinesAroundInfo) [ (1,1)
                                                , (0,1)
                                                ]
  where
    mg :: MineGrid
    mg = IA.listArray
           ((0,0),(2,2))
           [ True,True,True
           , True,True,True
           , True,True,True
           ]
    g :: Grid
    g = minesToGrid mg
    findCellInfo :: Location -> String
    findCellInfo location = show location ++ " is: " ++ show (findCell g location)
    numMinesAroundInfo :: Location -> String
    numMinesAroundInfo location = show location ++ " has this many mines around it: " ++ show (numberOfMinesAround g location)
