module P5.BoardingPassFileReader(
  readBoardingPasses
  ) where

import System.IO

import P5.BoardingPass(
  BoardingPass,
  toBoardingPass
  )

readBoardingPasses :: String -> IO [BoardingPass]
readBoardingPasses fileName = do
  readLines <- lines <$> readFile fileName
  return $ map toBoardingPass readLines
