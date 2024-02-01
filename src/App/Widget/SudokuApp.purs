module App.Widget.SudokuApp
  ( Input
  , component
  )
  where
  
import Prelude

import App.Controller.PuzzleController as PuzzleController
import App.Data.Difficulty (Difficulty(..), allDiffuculties, prettyDifficulty)
import App.Data.Puzzle (Puzzle)
import App.Data.Sudoku.Board (Size)
import App.Widget.FreeGame as FreeGame
import App.Widget.PuzzleGame as PuzzleGame
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (error)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Web.Storage.Storage (Storage)

data Mode = Menu | FreeGame | PuzzleGame Puzzle

type State = 
  { mode :: Mode
  , localStorage :: Storage
  , size         :: Size
  }

type Input =
  { localStorage :: Storage
  , size         :: Size
  }

data Action 
  = HandlePuzzleGame PuzzleGame.Output
  | NewPuzzleGame Difficulty
  | NewFreeGame
  | Back

type Slots = 
  ( freeGame         :: forall o. H.Slot FreeGame.Query o Unit
  , puzzleGame       :: forall q. H.Slot q PuzzleGame.Output Unit
  , puzzleController :: forall o. H.Slot PuzzleController.Query o Unit
  )

_freeGame = Proxy :: Proxy "freeGame"
_puzzleGame = Proxy :: Proxy "puzzleGame"
_puzzleController = Proxy :: Proxy "puzzleController"

component :: forall q o m. MonadAff m => H.Component q Input o m
component =
  H.mkComponent
    { initialState: \i -> { mode: Menu, localStorage: i.localStorage, size: i.size }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state = HH.div [HP.classes $ map ClassName ["main-frame"]] $
  [ navBar
  , HH.div [HP.classes $ map ClassName ["row", "justify-content-center"]] [
      case state.mode of 
        Menu -> HH.div [HP.classes $ map ClassName ["menu", "col-10", "col-md-8", "col-xl-6", "justify-content-center"] ] 
          [ HH.div [HP.classes $ map ClassName [ "justify-content-center", "menu-text" ]] 
            [HH.text "Select game difficulty"]
          , HH.div [HP.classes $ map ClassName ["d-grid",  "gap-2", "align-middle"] ] $ 
            (map mkStartPuzzleBtn allDiffuculties) <> [mkStartFreeGameBtn]
          ]
        FreeGame -> HH.slot_ _freeGame unit FreeGame.component state.size
        PuzzleGame puzzle -> HH.slot _puzzleGame unit PuzzleGame.component puzzle HandlePuzzleGame
    ]
  , HH.slot_ _puzzleController unit PuzzleController.controller { localStorage: state.localStorage, size: state.size }
  ]
  where
    navBar :: forall w. HH.HTML w Action
    navBar = HH.nav [HP.classes $ map ClassName ["navbar", "bg-dark", "border-bottom"]] $ singleton $
      HH.div [HP.classes $ map ClassName ["container-fluid"]] 
      [ HH.span [HP.classes $ map ClassName ["navbar-brand",  "mb-0 h1", "justify-content-center"]]
        [ HH.text "Sudoku-Web"
        ]
      , HH.ul [HP.classes $ map ClassName ["nav-bar-btns", "nav", "justify-content-start"]] 
        $ case state.mode of 
          Menu -> []
          _ -> [ HH.li [HP.classes $ map ClassName ["nav-item"], onClick $ \_ -> Back] 
                  [ HH.i [HP.classes $ map ClassName ["bi", "bi-box-arrow-left"]] []
                  , HH.text "Back"]
               ]
      ]

    mkStartPuzzleBtn :: forall w. Difficulty -> HH.HTML w Action
    mkStartPuzzleBtn diff = HH.div 
      [ HP.classes $ map ClassName $ [ "btn", "menu-btn", diffClass ]
      , onClick $ \_ -> NewPuzzleGame diff
      ] 
      [HH.text $ prettyDifficulty diff]
      where 
        diffClass :: String 
        diffClass = case diff of 
          Nightmare -> "btn-danger"
          _ -> "btn-primary"

    mkStartFreeGameBtn :: forall w. HH.HTML w Action
    mkStartFreeGameBtn = HH.div 
      [ HP.classes $ map ClassName $ [ "btn", "menu-btn", "btn-success" ]
      , onClick $ \_ -> NewFreeGame
      ] 
      [HH.text $ "Solver"]

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of 
  HandlePuzzleGame PuzzleGame.Solved -> 
    H.tell _puzzleController unit PuzzleController.SolvedPuzzle
  NewPuzzleGame diff -> do
    mPuzzle <- H.request _puzzleController unit $ PuzzleController.GetPuzzle diff
    case mPuzzle of 
      Just puzzle -> H.modify_ $ \s -> s { mode = PuzzleGame puzzle }
      Nothing -> error "No game was provided"
  NewFreeGame -> do 
    H.tell _freeGame unit FreeGame.Restart
    H.modify_ $ \s -> s { mode = FreeGame }
  Back -> H.modify_ $ \s -> s { mode = Menu }