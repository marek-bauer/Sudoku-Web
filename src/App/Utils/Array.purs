module App.Utils.Array
  ( swap
  , get
  , withIndex
  )
  where
import Prelude

import Data.Tuple (Tuple)
import Data.Array (zip, range, length)
  
foreign import _swap :: forall a. Int -> Int -> Array a -> Array a
foreign import _get :: forall a. Int -> Array a -> a

swap :: forall a. Partial => Int -> Int -> Array a -> Array a
swap = _swap

get :: forall a. Partial => Int -> Array a -> a
get = _get

withIndex :: forall a. Array a -> Array (Tuple Int a)
withIndex l = zip (range 0 (length l - 1)) l