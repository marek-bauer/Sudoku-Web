module App.Utils.Device where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import _isMobile :: EffectFn1 Int Boolean

isMobile :: Effect Boolean
isMobile = runEffectFn1 _isMobile 0