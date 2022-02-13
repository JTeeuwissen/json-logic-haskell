module Context where

import Json (Json)

-- Context to access context data from a function.
type Context = Json

-- A list of contexts, the first being the current context.
type Stack = [Context]