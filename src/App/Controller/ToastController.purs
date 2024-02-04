module App.Controller.ToastController where

import Prelude

import App.Controller.ToastController.Toast (MessageLevel)
import App.Controller.ToastController.Toast as Toast
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import JS.Map.Primitive.Internal (Map, delete, empty, insert, lookup, toAscArray)
import JS.Map.Primitive.Key (class Key)
import Type.Proxy (Proxy(..))

toastTTL :: Milliseconds
toastTTL = Milliseconds (5.0 * 1000.0)

toastTransition :: Milliseconds
toastTransition = Milliseconds 1000.0

type State = 
  { nextToastId :: Int
  , toasts      :: Map Int Toast.State
  }

type ToastInput = 
  { msg   :: String
  , level :: MessageLevel
  }

type Slots = ( toast :: forall q. H.Slot q Toast.Output Int )

_toast = Proxy :: Proxy "toast"

data Query a 
  = NewToast ToastInput a

data Action 
  = RunToast ToastInput
  | HandleToast Int Toast.Output

controller :: forall i o m. MonadAff m => H.Component Query i o m
controller =
  H.mkComponent
    { initialState: const $ { nextToastId: 0, toasts: empty }
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction 
                                   , handleQuery = handleQuery
                                   }
    }

render :: forall m. State -> H.ComponentHTML Action Slots m
render state = HH.div
  [ HP.classes $ map ClassName [ "toast-runner"] ] $
  map (\(Tuple index toast) -> HH.slot _toast index Toast.component toast (HandleToast index) ) $ toAscArray state.toasts

handleQuery :: forall o m a. MonadAff m => Query a -> H.HalogenM State Action Slots o m (Maybe a)
handleQuery = case _ of 
  NewToast toast a -> do 
    void $ H.fork $ handleAction $ RunToast toast
    pure $ Just a

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  RunToast toastInput -> do 
    id <- nextId
    let toastState = { msg: toastInput.msg, level: toastInput.level, visible: false }
    H.modify_ $ \s -> s { toasts = insert id toastState s.toasts }
    liftAff $ delay $ Milliseconds 1.0
    H.modify_ $ \s -> s { toasts = mapModify id (\t -> t { visible = true }) s.toasts }
    liftAff $ delay $ toastTTL
    H.modify_ $ \s -> s { toasts = mapModify id (\t -> t { visible = false }) s.toasts }
    liftAff $ delay $ toastTransition
    H.modify_ $ \s -> s { toasts = delete id s.toasts }
  HandleToast id Toast.Dismissed -> do
    toast <- H.gets $ \s -> lookup id s.toasts
    when (map (\t -> t.visible) toast == Just true) $ do
      H.modify_ $ \s -> s { toasts = mapModify id (\t -> t { visible = false }) s.toasts }
      liftAff $ delay $ toastTransition
      H.modify_ $ \s -> s { toasts = delete id s.toasts }
  where
    nextId :: forall n. MonadState State n => n Int
    nextId = do 
      {nextToastId} <- H.modify $ \s -> s { nextToastId = s.nextToastId + 1 }
      pure nextToastId

    -- | Safe modify if no field preesent it does nothing
    mapModify :: forall k v. Key k => k -> (v -> v) -> Map k v -> Map k v
    mapModify key f map = case lookup key map of 
      Just value -> insert key (f value) map
      Nothing    -> map
