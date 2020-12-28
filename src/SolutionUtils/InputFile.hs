module SolutionUtils.InputFile where

import Text.Printf

inputFileFor :: Int -> String
inputFileFor = printf "input-files/%d.txt"

