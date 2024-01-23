module App.Widget.Keyboard
  ( Input
  , Output(..)
  , State
  , component
  )
  where

import Prelude

import App.Data.Sudoku.Board (Size, toRowSize)
import App.Data.Sudoku.Field (Value(..), unValue)
import Data.Array (range)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Input = 
  { selected :: Value
  , size :: Size
  }

type State = Input

data Output = Selected Value

data Action 
  = Pressed Int
  | Refresh Input 

component :: forall q m. H.Component q Input Output m
component =
  H.mkComponent
    { initialState: \x -> x
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , receive = \i -> Just $ Refresh i
                                   }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state = HH.div [HP.class_ $ ClassName "sudoku-keyboard"] $
  flip map (range 1 finalElement) \elem -> 
    HH.div 
      [ HP.classes $ map ClassName $ ["keyboard-btn"] <> if isSelected elem then ["keyboard-selected"] else [] 
      , HE.onClick $ \_ -> Pressed elem
      ] 
      [HH.text $ show (Value elem)]
  where
    finalElement = toRowSize state.size

    isSelected :: Int -> Boolean
    isSelected x = x == (unValue state.selected)

handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  Pressed pressed -> do
    { selected } <- H.get
    let newValue = Value $ if unValue selected == pressed then 0 else pressed 
    H.modify_ $ \s -> s { selected = newValue }
    H.raise $ Selected newValue
  Refresh i -> H.modify_ (const i)