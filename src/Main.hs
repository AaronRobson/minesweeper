module Main
where

import qualified Text.Read as R

import qualified Gen as G

validateDifficultyMaybe :: String -> Maybe G.Difficulty
validateDifficultyMaybe d =
  case d of
    "1" -> Just G.beginnerDifficulty
    "2" -> Just G.intermediateDifficulty
    "3" -> Just G.advancedDifficulty
    _ -> Nothing

chooseDifficultyMaybe :: IO (Maybe G.Difficulty)
chooseDifficultyMaybe = do
  putStr "Enter 1, 2 or 3 for the difficulty type: "
  value <- getLine
  return $ validateDifficultyMaybe value

chooseDifficulty :: IO G.Difficulty
chooseDifficulty = do
  mDifficulty <- chooseDifficultyMaybe
  return $ case mDifficulty of
             Just difficulty -> difficulty
             Nothing -> G.beginnerDifficulty

validateSeed :: String -> Maybe Int
validateSeed s = R.readMaybe s :: Maybe Int

chooseSeedMaybe :: IO (Maybe Int)
chooseSeedMaybe = do
  putStr "Enter a number for a seed or blank for a random one: "
  seedStr <- getLine
  return $ validateSeed seedStr

chooseSeed :: IO Int
chooseSeed = chooseSeedMaybe >>= G.ensureSeed

main :: IO ()
main = do putStrLn "Minesweeper --- Terminal Interface."
          difficulty <- chooseDifficulty
          putStr "The difficulty is: "
          print difficulty
          seed <- chooseSeed
          putStr "The seed in use is: "
          print seed
