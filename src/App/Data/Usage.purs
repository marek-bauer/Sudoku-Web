module App.Data.Usage
  ( Usage(..)
  , getAvailable
  , parseUsage
  , parseUsages
  , saveUsages
  )
  where

import Prelude

import Data.Array (intercalate, length, replicate, take, uncons)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)

data Usage = Used Int | Empty | Solved

derive instance usageEq :: Eq Usage

parseUsage :: String -> Usage
parseUsage "E" = Empty
parseUsage "S" = Solved
parseUsage x = case fromString x of
  Just u  -> Used u
  Nothing -> Empty

instance usageShow :: Show Usage where
  show (Used u) = show u
  show Empty = "E"
  show Solved = "S"

parseUsages :: Int -> String -> Array Usage
parseUsages size = fillUpTo size <<< take size <<< (map parseUsage) <<< split (Pattern ";")
  where
    fillUpTo :: Int -> Array Usage -> Array Usage
    fillUpTo s array 
      | s > (length array) = array <> (replicate (s - length array) Empty)
      | otherwise = array

saveUsages :: Array Usage -> String
saveUsages = intercalate ";" <<< map show

getAvailable :: Array Usage -> Array Boolean
getAvailable = go criterias
  where
    go :: Array (Usage -> Boolean) -> Array Usage -> Array Boolean
    go p x = case uncons p of 
      Nothing -> map (\u -> u /= Empty) x
      Just ps -> if  count (map ps.head x) >= 10
        then map ps.head x
        else go ps.tail x

    criterias :: Array (Usage -> Boolean)
    criterias =
      [ fromMaybe false <<< map (_ < 10) <<< getUsages 
      , fromMaybe false <<< map (_ < 30) <<< getUsages
      ]

    count :: Array Boolean -> Int 
    count = sum <<< map (\p -> if p then 1 else 0)

    getUsages :: Usage -> Maybe Int 
    getUsages (Used u) = Just u
    getUsages _ = Nothing