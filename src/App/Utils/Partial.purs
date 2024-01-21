module App.Utils.Partial where

import Prelude

import App.Utils.Undefinedtable (Undefinedtable, toMaybe)
import Data.Maybe (Maybe)
import Partial.Unsafe (unsafePartial)
  
foreign import _partial :: forall a b. (a -> b) -> a -> Undefinedtable b

runPartial :: forall a b. (Partial => a -> b) -> a -> Maybe b
runPartial f x = toMaybe $ _partial (unsafePartial f) x
