module App.Widget.FreeGame
  ( Action(..)
  , State
  , _board
  , component
  , handleAction
  )
  where

import Prelude

import App.Data.Sudoku.Board (Board, getEmptyBoard, setAt)
import App.Data.Sudoku.Error (Error)
import App.Data.Sudoku.Field (valueToUserInput)
import App.Logic.Error (calcErrors)
import App.Wasm.Solver (solveSudoku, getHint)
import App.Widget.Board as BoardWidget
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

type State = { board :: Board }

data Action 
  = HandleBoard BoardWidget.Output
  | Solve 
  | Hint

type Slots = (board :: forall query. H.Slot query BoardWidget.Output Unit)
_board = Proxy :: Proxy "board"


component :: forall q m i o. MonadEffect m => MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> { board: getEmptyBoard 3 }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.slot _board unit BoardWidget.component { board: state.board, errors } HandleBoard
    , HH.div_
      [ HH.button 
          [ HE.onClick \_ -> Solve ] 
          [ HH.text "Solve this sudoku" ]
      , HH.button 
          [ HE.onClick \_ -> Hint ] 
          [ HH.text "Get hint" ]
      ]
    ]
  where
    errors :: Array Error
    errors = calcErrors state.board

handleAction :: forall m o. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  HandleBoard msg -> case msg of 
    BoardWidget.ValueInserted pos val ->
      H.modify_ (\s -> { board: unsafePartial $ setAt pos (valueToUserInput val) s.board })
    _ -> pure unit
  Solve -> do
    state <- H.get
    maybeSolution <- liftAff $ runMaybeT $ solveSudoku state.board
    case maybeSolution of
      Nothing -> liftEffect $ log "No solutions"
      Just solution -> H.modify_ $ const { board: solution }
  Hint -> do
    state <- H.get
    maybeHint <- liftAff $ runMaybeT $ getHint state.board 2
    case maybeHint of
      Nothing -> liftEffect $ log "No hint"
      Just { position, digit, level } -> do
        liftEffect $ log $ "Hint level " <> show level
        H.modify_ (\s -> { board: unsafePartial $ setAt position (valueToUserInput digit) s.board })