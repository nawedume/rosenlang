module Core where

import Dist

data Expr = Symbol String
    | Boolean Bool
    | Number Double
    | Tuple [Expr]  -- An order list of symbols, or literals
    | Reference Expr Expr -- Symbol to any Expr
    | DistInit Expr -- Symbol and Mapping
    | DistJoin [Expr] -- 
    | Measure [(Expr, Expr)] -- A mapping from a symbol to a number (probability)
    deriving (Show, Eq)
