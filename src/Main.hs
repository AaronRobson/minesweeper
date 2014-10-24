module Main
where

import qualified Text.Read as R

import qualified Gen as G

validateSettingsMaybe :: String -> Maybe G.Settings
validateSettingsMaybe s =
  case s of
    "1" -> Just G.beginnerSettings
    "2" -> Just G.intermediateSettings
    "3" -> Just G.advancedSettings
    _ -> Nothing

chooseSettingsMaybe :: IO (Maybe G.Settings)
chooseSettingsMaybe = do
  putStr "Enter 1, 2 or 3 for the setting type: "
  value <- getLine
  return $ validateSettingsMaybe value

chooseSettings :: IO G.Settings
chooseSettings = do
  mSettings <- chooseSettingsMaybe
  return $ case mSettings of
             Just settings -> settings
             Nothing -> G.beginnerSettings  

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
          settings <- chooseSettings
          putStr "The settings in use are: "
          print settings
          seed <- chooseSeed
          putStr "The seed in use is: "
          print seed
