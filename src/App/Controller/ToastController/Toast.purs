module App.Controller.ToastController.Toast
  ( MessageLevel(..)
  , Output(..)
  , State
  , component
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data MessageLevel 
  = ErrorMsg
  | WarningMsg
  | InfoMsg

type State = 
  { msg     :: String
  , level   :: MessageLevel
  , visible :: Boolean
  }

type Input = State

data Output = Dismissed

data Action 
  = Clicked
  | Refresh Input

component :: forall q m. H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \x -> x
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , receive = Just <<< Refresh
                                   }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state = HH.div
  [ HP.classes $ map ClassName [ "my-toast", levelClass, if state.visible then "show-toast" else "hide-toast" ] 
  , HE.onClick $ \_ -> Clicked
  ]
  [ HH.text state.msg]
  where
    levelClass :: String
    levelClass = case state.level of 
      ErrorMsg -> "error-toast"
      WarningMsg -> "warning-toast"
      InfoMsg -> "info-toast"

handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  Clicked -> H.raise Dismissed
  Refresh i -> H.modify_ $ const i