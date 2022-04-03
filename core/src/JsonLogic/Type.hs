module JsonLogic.Type where

import Control.Monad.Except
import qualified Data.Map as M
import JsonLogic.Json

-- | The result of a function can be an error or another json value.
type Result r m = ExceptT String m r

-- | Subevaluator, with rule, its context and resulting json.
type SubEvaluator m = Rule -> Data -> Result Json m

-- | A function takes a subevaluator, a rule and data and returns a result.
type Function r m = SubEvaluator m -> Rule -> Data -> Result r m

-- | Operation is a function with a name.
type Operation m = (String, Function Json m)

-- | Operations is a Map from the operation name to the operation function.
type Operations m = M.Map String (Function Json m)

-- | The environment contains the functions and variables our environment has currently
data JsonLogicEnv_ m = JLEnv_
  { operations :: Operations m, -- All the operations (plus custom ones)
    variables :: Json -- Variables defined in rules
  }

-- | Show the current environment.
instance Show (JsonLogicEnv_ m) where
  show (JLEnv_ os vs) = "Operations: " ++ show (M.keys os) ++ "\nVariables: " ++ show vs
