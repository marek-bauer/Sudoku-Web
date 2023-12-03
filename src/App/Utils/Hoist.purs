module App.Utils.Hoist
  ( hoistMaybe
  )
where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Maybe (Maybe)

hoistMaybe :: forall m. Monad m => Maybe ~> MaybeT m
hoistMaybe = MaybeT <<< pure