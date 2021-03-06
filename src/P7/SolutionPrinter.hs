module P7.SolutionPrinter(
  printSolution
  ) where

import P7.RulesFileReader(
  readRulesFile
  )

import P7.Rules(
  BagType(..)
  )

import P7.Solution(
  bagsContainingAtLeastOneOf,
  countBagsRequiredInside
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  rules <- readRulesFile $ inputFileFor 7
  let bagType = BagType { attribute = "shiny", color = "gold" }

  print $ bagsContainingAtLeastOneOf bagType rules
  print $ countBagsRequiredInside bagType rules
