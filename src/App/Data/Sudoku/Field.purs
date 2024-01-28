module App.Data.Sudoku.Field
  ( Field(..)
  , Value(..)
  , charToValue
  , fieldToChar
  , fieldToValue
  , isEditable
  , stringToValue
  , unValue
  , valueToChar
  , valueToGiven
  , valueToUserInput
  )
  where

import Prelude

import Data.Char (fromCharCode, toCharCode)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton, toCharArray)

newtype Value = Value Int

derive instance Eq Value
derive instance Ord Value

instance Show Value where
  show v = case valueToChar v of 
    Just c -> singleton c
    Nothing -> "#" 

data Field = Empty | UserInput Value | Given Value

derive instance Eq Field
derive instance Ord Field

stringToValue :: String -> Maybe Value
stringToValue str = case toCharArray str of 
  [c] -> charToValue c
  _ -> Nothing

unValue :: Value -> Int 
unValue (Value v) = v

charToValue :: Char -> Maybe Value
charToValue c = case toCharCode c of
  32 -> Just $ Value 0 -- code of space
  n | toCharCode '0' < n && n <= toCharCode '9' 
      -> Just <<< Value $ n - toCharCode '0'
  n | toCharCode 'A' <= n && n <= toCharCode 'Z' 
      -> Just <<< Value $ n - toCharCode 'A' + 10
  n | toCharCode 'a' <= n && n <= toCharCode 'z' 
      -> Just <<< Value $ n - toCharCode 'a' + 10
  _ -> Nothing

valueToGiven :: Value -> Field
valueToGiven = case _ of
  Value 0 -> Empty
  Value n -> Given (Value n)

valueToUserInput :: Value -> Field
valueToUserInput = case _ of
  Value 0 -> Empty
  Value n -> UserInput (Value n)

valueToChar :: Value -> Maybe Char
valueToChar = case _ of
  (Value n) | n < 10 -> fromCharCode $ toCharCode '0' + n
  (Value n) | n < 36 -> fromCharCode $ toCharCode 'A' - 10 + n
  _ -> Nothing

fieldToChar :: Field -> Maybe Char
fieldToChar = case _ of
  Empty -> Just ' '
  Given v -> valueToChar v
  UserInput v -> valueToChar v

fieldToValue :: Field -> Value
fieldToValue = case _ of
  Empty -> Value 0
  Given v -> v
  UserInput v -> v

isEditable :: Field -> Boolean
isEditable (Given _) = false
isEditable _ = true
