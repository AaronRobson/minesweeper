module Main
where

import qualified Text.Read as R

import qualified Gen as G

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
          seed <- chooseSeed
          putStr "The seed in use is: "
          print seed
