module App.Utils.Aff
  ( withTimeout
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Aff, attempt, delay, error, forkAff, joinFiber, killFiber)

withTimeout :: forall a. Milliseconds -> Aff a -> Aff (Maybe a)
withTimeout waitTime computations = do
  main <- forkAff computations
  timeoutFiber <- forkAff $ do
    delay waitTime
    killFiber (error "Timeout") main
  result <- attempt $ joinFiber main
  case result of 
    Right r -> do 
      killFiber (error "Finished") timeoutFiber
      pure $ Just $ r
    Left _ ->  pure $ Nothing