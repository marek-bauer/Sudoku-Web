module App.Data.Difficulty
  ( Difficulty(..)
  , allDiffuculties
  , prettyDifficulty
  )
  where

import Prelude

data Difficulty 
  = Easy
  | Medium
  | Hard
  | Nightmare

derive instance difficultyEq :: Eq Difficulty
derive instance difficultyOrd :: Ord Difficulty

instance difficultyShow :: Show Difficulty where
  show Easy = "easy"
  show Medium = "medium"
  show Hard = "hard"
  show Nightmare = "nightmare"

prettyDifficulty :: Difficulty -> String
prettyDifficulty = case _ of 
  Easy -> "Easy"
  Medium -> "Medium"
  Hard -> "Hard"
  Nightmare -> "Nightmare"

allDiffuculties :: Array Difficulty
allDiffuculties= [Easy, Medium, Hard, Nightmare]