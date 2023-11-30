module App.Utils.Test
  ( Marek
  , new
  , next
  )
  where

import Prelude

import Data.Maybe (Maybe)
-- import Data.Undefinable (Undefinable)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

foreign import data Marek :: Type
foreign import _new :: Unit -> EffectFnAff Marek
foreign import _next :: Marek -> Int

new :: Unit -> Aff Marek
new unit = fromEffectFnAff $ _new unit

next :: Marek -> Int
next m = _next m