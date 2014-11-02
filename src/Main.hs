module Main
where

import qualified Data.List as L
import qualified Text.Read as R
import qualified Data.Maybe as M

import qualified Gen as G

chooseSettings :: IO G.Settings
chooseSettings = do
  d <- chooseDifficulty
  s <- chooseSeed
  return $ G.Settings s d

validateDifficultyMaybe :: String -> Maybe G.Difficulty
validateDifficultyMaybe s =
  fmap pred (R.readMaybe s :: Maybe Integer) >>= G.validateDifficultyFromIndexMaybe

chooseDifficultyMaybe :: IO (Maybe G.Difficulty)
chooseDifficultyMaybe = do
  putStr $ "Enter " ++ range1BasedString ++ " for the difficulty type: "
  value <- getLine
  return $ validateDifficultyMaybe value
  where
    range0Based :: [Integer]
    range0Based = [0..(pred (L.genericLength G.difficulties))]
    range1Based = map succ range0Based
    range1BasedString :: String
    range1BasedString = show range1Based

chooseDifficulty :: IO G.Difficulty
chooseDifficulty = do
  mDifficulty <- chooseDifficultyMaybe
  return $ M.fromMaybe G.defaultDifficulty mDifficulty

validateSeed :: String -> Maybe G.Seed
validateSeed s = R.readMaybe s :: Maybe G.Seed

chooseSeedMaybe :: IO (Maybe G.Seed)
chooseSeedMaybe = do
  putStr "Enter a number for a seed or blank for a random one: "
  seedStr <- getLine
  return $ validateSeed seedStr

chooseSeed :: IO G.Seed
chooseSeed = chooseSeedMaybe >>= G.ensureSeed

main :: IO ()
main = do putStrLn "Minesweeper --- Terminal Interface."
          settings <- chooseSettings
          putStr "The difficulty is: "
          print $ G.difficulty settings
          putStr "The seed in use is: "
          print $ G.seed settings
