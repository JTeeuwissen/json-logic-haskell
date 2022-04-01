{-# LANGUAGE NoImplicitPrelude #-}

{- ORMOLU_DISABLE -}
module JsonLogic.IO.Operation
  ( defaultOperations,
    arrayOperations, map, reduce, filter, all, none, some, merge, in',
    booleanOperations, if', (==), (===), (!=), (!==), (!), (!!), and, or,
    dataOperations, var, missing, missingSome, preserve,
    miscOperations, trace, log,
    numericOperations, (>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%),
    stringOperations, cat, substr
  )
where
{- ORMOLU_ENABLE -}
import qualified Data.Map as M
import JsonLogic.IO.Operation.Misc (log, miscOperations, trace)
import JsonLogic.IO.Type
import qualified JsonLogic.Operation as O
import Prelude (String)

-- Default operators
defaultOperations :: M.Map String Function
defaultOperations = M.unions [arrayOperations, booleanOperations, dataOperations, miscOperations, numericOperations, stringOperations]

arrayOperations, booleanOperations, dataOperations, numericOperations, stringOperations :: Operations
arrayOperations = O.arrayOperations
booleanOperations = O.arrayOperations
dataOperations = O.arrayOperations
numericOperations = O.arrayOperations
stringOperations = O.arrayOperations

map, reduce, filter, all, none, some, merge, in' :: Operation
map = O.map
reduce = O.reduce
filter = O.filter
all = O.all
none = O.none
some = O.some
merge = O.merge
in' = O.in'

if', (==), (===), (!=), (!==), (!), (!!), and, or :: Operation
if' = O.if'
(==) = (O.==)
(===) = (O.===)
(!=) = (O.!=)
(!==) = (O.!==)
(!) = (O.!)
(!!) = (O.!!)
and = O.and
or = O.or

var, missing, missingSome, preserve :: Operation
var = O.var
missing = O.missing
missingSome = O.missingSome
preserve = O.preserve

(>), (>=), (<), (<=), max, min, sum, (+), (-), (*), (/), (%) :: Operation
(>) = (O.>)
(>=) = (O.>=)
(<) = (O.<)
(<=) = (O.<=)
max = O.max
min = O.min
sum = O.sum
(+) = (O.+)
(-) = (O.-)
(*) = (O.*)
(/) = (O./)
(%) = (O.%)

cat, substr :: Operation
cat = O.cat
substr = O.substr
