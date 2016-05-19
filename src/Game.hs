module Game
where

import qualified Control.Monad.State as S

--https://www.haskell.org/haskellwiki/Arrays#Immutable_arrays_.28module_Data.Array.IArray.29
--https://www.haskell.org/ghc/docs/latest/html/libraries/array/Data-Array-IArray.html
--https://www.haskell.org/ghc/docs/latest/html/libraries/array/Data-Array-IArray.html#t:Array
import qualified Data.Array.IArray as IA

import qualified Gen as G
import qualified Func as F

type GameState = F.Grid
type GameValue = G.Point
type GameResult = F.Progress
{-
playGame :: String -> S.State GameState GameValue
playGame [] = do
  s <- S.get
  return s

step :: F.Grid -> G.Point -> F.Grid
step g p@(G.Point x y)
  | F.progress g /= InProgress = g
  | F.marking item in [QuestionMark, Normal] = g IA.// [((x,y),item')]
  where
    item :: GridCell
    item = IA.! g (x y)
    item' :: GridCell
    item' = F.MineCell (mine item) Revealed
-}
--runGame :: S.State [Int] Int
--runGame s a = a:s

main :: IO ()
main = undefined
