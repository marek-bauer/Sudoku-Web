module App.Utils.Undefinedtable
  ( Undefinedtable
  , toMaybe
  )
where

import Data.Maybe (Maybe(..))

foreign import data Undefinedtable :: Type -> Type
foreign import convert_undefined :: forall a m. (forall b. b -> m b) -> (forall b. m b) -> Undefinedtable a -> m a

toMaybe :: forall a. Undefinedtable a -> Maybe a
toMaybe = convert_undefined Just Nothing