module Func
where

import Data.List (genericLength, genericIndex)
import qualified Data.Maybe as M
import qualified Data.Default as Def -- cabal install data-default

type Grid = [GridRow]
type GridRow = [GridCell]
data GridCell = GridCell { mine :: MineCell
                         , marking :: Marking
                         } deriving (Eq, Show)
-- http://byorgey.wordpress.com/2010/04/03/haskell-anti-pattern-incremental-ad-hoc-parameter-abstraction/
instance Def.Default GridCell where
  def = GridCell False Normal

newGridCell :: MineCell -> GridCell
newGridCell m = Def.def {mine = m}

minesToGrid :: MineGrid -> Grid
minesToGrid = (map . map) newGridCell

-- Lossy transformation.
gridToMines :: Grid -> MineGrid
gridToMines = (map . map) mine

type MineGrid = [MineGridRow]
type MineGridRow = [MineCell]
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
    won grid = all cellValid (concat grid)
      where
        cellValid :: GridCell -> Bool
        cellValid gc = mine gc /= (Revealed == marking gc)
    lost :: Grid -> Bool
    lost grid = any cellInvalid (concat grid)
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
findCell grid (x,y) = findRow >>= findCellInRow
  where
    findRow :: Maybe GridRow
    findRow
      | (0 <= y) && (y < genericLength grid) = Just (genericIndex grid y)
      | otherwise = Nothing
    findCellInRow :: GridRow -> Maybe GridCell
    findCellInRow gridRow
      | (0 <= x) && (x < genericLength gridRow) = Just (genericIndex gridRow x)
      | otherwise = Nothing

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
          putStrLn $ findCellInfo (-1,-1)
          putStrLn $ findCellInfo (0,0)
          putStrLn $ findCellInfo (0,1)
          putStrLn $ findCellInfo (0,1)
          putStrLn $ findCellInfo (2,2)
          putStrLn $ findCellInfo (3,3)
          putStrLn $ numMinesAroundInfo (1,1)
          putStrLn $ numMinesAroundInfo (0,1)
  where
    mg = [ [True,True,True]
         , [True,True,True]
         , [True,True,True]
         ]
    g = minesToGrid mg
    findCellInfo :: Location -> String
    findCellInfo location = show location ++ " is: " ++ show (findCell g location)
    numMinesAroundInfo :: Location -> String
    numMinesAroundInfo location = show location ++ " has this many mines around it: " ++ show (numberOfMinesAround g location)
