module JsonLogic.Type where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.Json

-- | The result of a function can be an error or another json value.
type Result m = ExceptT String m Json

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator m = Rule -> Data -> Result m

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function m = SubEvaluator m -> Rule -> Data -> Result m

-- | Operation is a function with a name.
type Operation m = (String, Function m)

-- | Operations is a Map from the operation name to the operation function.
type Operations m = M.Map String (Function m)

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnv m = JLEnv
  { operations :: Operations m, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | Show the current environment.
instance Show (JsonLogicEnv m) where
  show (JLEnv os vs) = "Operations: " ++ show (M.keys os) ++ "\nVariables: " ++ show vs
