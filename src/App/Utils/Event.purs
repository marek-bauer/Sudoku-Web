module App.Utils.Event
  ( eventKey
  )
  where

import Web.UIEvent.KeyboardEvent (KeyboardEvent)

foreign import eventKey :: KeyboardEvent -> String