module App.Controller.PuzzleController
  ( Input(..)
  , Query(..)
  , controller
  )
  where

import Prelude

import App.Data.DefaultPuzzles (getDefaultPuzzles)
import App.Data.Difficulty (Difficulty(..), allDiffuculties)
import App.Data.Puzzle (Puzzle, loadPuzzle, savePuzzle)
import App.Data.Sudoku.Board (Size(..))
import App.Data.Usage (Usage(..), getAvailable, parseUsages, saveUsages)
import App.Utils.Array (withIndex)
import App.Utils.Hoist (hoistMaybe)
import App.Utils.Partial (runPartial)
import App.Utils.RandomMonad (class RandomMonad, randomInt)
import App.Wasm.Generator (generateSudoku)
import Control.Monad.Except (lift)
import Control.Monad.Except.Trans (except, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.State (class MonadState)
import Data.Array (cons, filter, foldMap, length, replicate)
import Data.Array.Extra.Unsafe (unsafeModifyAt, unsafeUpdateAt)
import Data.Either (Either(..), note)
import Data.Foldable (foldM, maximum, maximumBy, sum)
import Data.Int (pow, toNumber)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error, log)
import Halogen (fork)
import Halogen as H
import Halogen.HTML as HH
import Web.Storage.Storage (Storage, getItem, setItem)

maxPuzzlesPerDifficulty :: Int
maxPuzzlesPerDifficulty = 100

type SearchParameters =
  { minDifficulty :: Int
  , maxDifficulty :: Int
  , maxHintLevel  :: Int
  , fuel          :: Int
  , size          :: Int
  }

difficultySearch :: Size -> Difficulty -> SearchParameters
difficultySearch (Size size) = case _ of
  Easy      -> {minDifficulty: halfSize, maxDifficulty: fieldSize, maxHintLevel: 0, fuel: fieldSize, size}
  Medium    -> {minDifficulty: fieldSize, maxDifficulty: fieldSize + halfSize, maxHintLevel: 1, fuel: 2 * fieldSize, size}
  Hard      -> {minDifficulty: fieldSize + halfSize, maxDifficulty: 2 * fieldSize, maxHintLevel: 2, fuel: 3 * fieldSize, size}
  Nightmare -> {minDifficulty: 2 * fieldSize, maxDifficulty: pow 2 30, maxHintLevel: 3, fuel: 10 * fieldSize, size}
  where
    fieldSize = pow size 4
    halfSize = fieldSize / 2

type Input =
  { localStorage :: Storage
  , size         :: Size
  }

type State = 
  { input                :: Input
  , available            :: Map Difficulty (Array Int)
  , usage                :: Map Difficulty (Array Usage)
  , generationInProgress :: Boolean
  , providedPuzzle       :: Maybe (Tuple Difficulty Int)
  }

data Action
  = InitializeController
  | GeneratePuzzle Difficulty Int Int
  | GeneratedPuzzle Difficulty Int Puzzle
  | TickManager

data Query a
  = GetPuzzle Difficulty (Puzzle -> a)
  | SolvedPuzzle a

data Msg = Error String | Info String 

controller :: forall o m. MonadAff m => RandomMonad m => H.Component Query Input o m
controller =
  H.mkComponent
    { initialState: initializeState
    , render: \_ -> HH.text ""
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , handleQuery = handleQuery
                                   , initialize = Just InitializeController
                                   }
    }
  where
    initializeState :: Input -> State
    initializeState input =
      { input
      , available: empty
      , usage: empty
      , generationInProgress: false
      , providedPuzzle: Nothing
      }

handleQuery :: forall a cs o m. MonadAff m => RandomMonad m => Query a -> H.HalogenM State Action cs o m (Maybe a)
handleQuery = case _ of 
  GetPuzzle diff returnPuzzle -> runMaybeT $ do
    { available, usage } <- H.get

    -- getting puzzle
    availForDiff <- hoistMaybe $ lookup diff available
    index <- lift $ randomInt 0 (length availForDiff - 1)
    puzzle <- getFromStorage diff index

    -- update usage 
    usageForDiff <- hoistMaybe $ lookup diff usage
    updatedUsage <- hoistMaybe $ updateUsage incrementUsage index usageForDiff
    saveUsagesInStorage diff updatedUsage

    H.modify_ $ \s -> s { providedPuzzle = Just $ Tuple diff index }

    lift $ handleAction TickManager
    pure $ returnPuzzle puzzle
  SolvedPuzzle solved -> runMaybeT $ do
    { usage, providedPuzzle } <- H.get

    Tuple diff index <- hoistMaybe providedPuzzle
    usageForDiff <- hoistMaybe $ lookup diff usage
    updatedUsage <- hoistMaybe $ updateUsage (const Solved) index usageForDiff
    saveUsagesInStorage diff updatedUsage

    H.modify_ $ \s -> s { providedPuzzle = Nothing }

    lift $ handleAction TickManager
    pure solved
  where
    getFromStorage :: forall n. MonadEffect n => MonadState State n => Difficulty -> Int -> MaybeT n Puzzle
    getFromStorage diff index = do
      storage <- getStorage
      size <- getSize
      let recordName = puzzleRecordName size diff index
      puzzleStr <- MaybeT $ liftEffect $ getItem recordName storage
      hoistMaybe $ loadPuzzle puzzleStr

    updateUsage :: (Usage -> Usage) -> Int -> Array Usage -> Maybe (Array Usage)
    updateUsage f index = runPartial $ unsafeModifyAt index f

    incrementUsage :: Usage -> Usage
    incrementUsage (Used x) = Used $ x + 1
    incrementUsage u = u

handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  InitializeController -> do
    size <- getSize 
    flip foldMap allDiffuculties $ \diff -> do
      storedUsages <- runMaybeT $ getUsageList diff
      usages <- case storedUsages of
        Just u -> pure u
        Nothing -> do -- if no data in localStorage
          let defaults = getDefaultPuzzles size diff
          flip foldMap (withIndex defaults) \(Tuple index puzzleStr) -> 
            savePuzzleInStorage diff index puzzleStr
          pure $ replicate (length defaults) (Used 20) <> replicate (maxPuzzlesPerDifficulty - length defaults) Empty
      saveUsagesInStorage diff usages
    handleAction TickManager
  GeneratePuzzle difficulty index sleep -> do
    log $ "Generating " <> show difficulty <> " " <> show index <> " " <> show sleep
    { generationInProgress } <- H.get
    { size } <- H.gets (\s -> s.input)
    unless generationInProgress $ do
      H.modify_ \s -> s { generationInProgress = true }
      liftAff $ delay (Milliseconds $ toNumber $ 1000 * sleep)
      mPuzzle <- liftAff $ runMaybeT $ generateSudoku $ difficultySearch size difficulty
      case mPuzzle of 
        Just puzzle -> handleAction $ GeneratedPuzzle difficulty index puzzle
        Nothing -> do 
          H.modify_ \s -> s { generationInProgress = false }
          handleAction $ GeneratePuzzle difficulty index 5
  GeneratedPuzzle difficulty index puzzle -> do
    log $ "Genetated"
    H.modify_ \s -> s { generationInProgress = false }
    eSaved <- runExceptT $ do
      puzzleStr <- except $ note (Error "Generated invalid puzzle" ) $ savePuzzle puzzle
      savePuzzleInStorage difficulty index puzzleStr
      { usage } <- H.get 
      usageArray <- except $ note (Error "Usage array not found") $ lookup difficulty usage
      updatedUsage <- except $ note (Error "Usage update failed") 
        $ runPartial (unsafeUpdateAt index (Used 0)) usageArray
      lift $ saveUsagesInStorage difficulty updatedUsage
      H.modify_ \s -> s { providedPuzzle = if s.providedPuzzle == Just (Tuple difficulty index) 
                                           then Nothing 
                                           else s.providedPuzzle 
                        }
    case eSaved of 
      Right _ -> handleAction TickManager
      Left msg -> handleMsg msg

  TickManager -> do
    { usage } <- H.get
    let ePuzzleToCompute = do  
          scores <- note (Error "No usage array for some difficulties")
            $ foldM (\l x -> (flip cons l) <$> x) [] 
            $ map (\(Tuple diff score) -> (\x -> Tuple diff x) <$> score)
            $ map (\diff -> Tuple diff (map scoreUsages (lookup diff usage)))
            $ allDiffuculties
          diffWithWorstPuzzles <- note (Error "No difficulties found")
            $ map (\(Tuple d _) -> d)
            $ maximumBy (\(Tuple _ score) (Tuple _ score') -> compare score score') scores
          worstDiffUsage <- note (Error "Usage not found")
            $ lookup diffWithWorstPuzzles usage
          Tuple indexToReplace score <- note (Info "No index requires replacing")
            $ findIndexToReplace worstDiffUsage
          pure $ {index: indexToReplace, score, difficulty: diffWithWorstPuzzles}

    case ePuzzleToCompute of 
      Right req -> void
        $ fork
        $ handleAction 
        $ GeneratePuzzle req.difficulty req.index (scoreToDelay req.score)
      Left msg -> handleMsg msg
  where
    handleMsg :: forall n. MonadEffect n => Msg -> n Unit
    handleMsg = case _ of 
      Error err -> error err
      Info info -> log info

    scoreToDelay :: Int -> Int
    scoreToDelay x = 1000 / (x + 1) 

    savePuzzleInStorage :: forall n. MonadEffect n => MonadState State n => Difficulty -> Int -> String -> n Unit
    savePuzzleInStorage diff index puzzle = do 
      storage <- getStorage
      size <- getSize
      let recordName = puzzleRecordName size diff index
      liftEffect $ setItem recordName puzzle storage 


    getUsageList :: forall n. MonadEffect n => MonadState State n => Difficulty -> MaybeT n (Array Usage)
    getUsageList diff = do
      storage <- lift $ getStorage
      size <- lift $ getSize
      let recordName = puzzleUsageRecordName size diff
      usageString <- MaybeT $ liftEffect $ getItem recordName storage
      pure $ parseUsages maxPuzzlesPerDifficulty usageString

    -- | The higher score the worse situation
    computeScore :: Usage -> Int
    computeScore (Used x) 
      | x < 10 = 0
      | otherwise = 0
    computeScore Solved = 1000
    computeScore Empty = 10000

    scoreUsages :: Array Usage -> Int
    scoreUsages = sum <<< map computeScore

    findIndexToReplace :: Array Usage -> Maybe (Tuple Int Int)
    findIndexToReplace = maximum <<< filter (\(Tuple _ s) -> s > 0) <<< withIndex <<< map computeScore

getStorage :: forall n. MonadState State n => n Storage
getStorage = (\{input} -> input.localStorage) <$> H.get

getSize :: forall n. MonadState State n => n Size
getSize = (\{input} -> input.size) <$> H.get

puzzleUsageRecordName :: Size -> Difficulty -> String
puzzleUsageRecordName size diff = "Puzzle_usage_" <> show size <> "_" <> show diff

puzzleRecordName ::  Size -> Difficulty -> Int -> String
puzzleRecordName size diff index = "Puzzle_" <> show size <> "_" <> show diff <> "_" <> show index

getIndexes :: Array Boolean -> Array Int  
getIndexes = foldMap (\(Tuple i p) -> if p then [i] else [])  <<< withIndex

setUsageList ::  forall n. (MonadEffect n) => Storage -> Size -> Difficulty -> Array Usage -> n Unit
setUsageList storage size diff usage = do 
  let usageStr = saveUsages usage
  let recordName = puzzleUsageRecordName size diff
  liftEffect $ setItem recordName usageStr storage

saveUsagesInStorage :: forall n. MonadAff n => MonadState State n => Difficulty -> Array Usage -> n Unit
saveUsagesInStorage difficulty usages = do 
  { size, localStorage } <- H.gets (\s -> s.input)
  H.modify_ \s -> s { available = insert difficulty (getIndexes $ getAvailable usages) s.available
                    , usage = insert difficulty usages s.usage
                    }
  setUsageList localStorage size difficulty usages