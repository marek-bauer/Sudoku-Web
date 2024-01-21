module App.Data.Difficulty
  ( Difficulty(..)
  , allDiffuculties
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

allDiffuculties :: Array Difficulty
allDiffuculties= [Easy, Medium, Hard, Nightmare]