module App.Utils.RandomMonad
  ( class RandomMonad
  , randomInt
  , randomPermutationRange
  )
  where

import Prelude

import App.Utils.Array (swap)
import Data.Array (range, foldM)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random as RandEff
import Partial.Unsafe (unsafePartial)

class Monad m <= RandomMonad m where
  randomInt :: Int -> Int -> m Int

instance MonadEffect m => RandomMonad m where
  randomInt = \l h -> liftEffect $ RandEff.randomInt l h

randomPermutationRange :: forall m. RandomMonad m => Int -> m (Array Int)
randomPermutationRange len =
  foldM applySwap original (range 0 (len - 2))
  where 
    original :: Array Int
    original = range 0 (len - 1)

    applySwap :: Array Int -> Int -> m (Array Int)
    applySwap prev index = do
      posSwitch <- randomInt index (len - 1)
      pure <<< unsafePartial $ swap index posSwitch prev
