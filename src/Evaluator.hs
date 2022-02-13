module Evaluator where

import Json (Json)
import Rule (Rule)

type Evaluator = Rule -> [Json] -> Json
