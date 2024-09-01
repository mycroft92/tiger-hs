module Resolver where
import Control.Monad.Except (ExceptT(..), runExceptT, throwError, catchError)
import Control.Monad.State  (State, StateT,
  runState,
  runStateT,
  MonadState(get, put),
  MonadIO(liftIO),
  MonadTrans(lift) )
import Data.Map.Strict as Map
import Data.Map as M
import Data.Foldable (forM_, foldl)
import Stack
import Data.Maybe (fromJust)

-- Resolution helper to map from each expression to scope distance