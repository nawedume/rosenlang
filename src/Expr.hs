module Expr where

import Dist

type Mapping = [(Expr, Double)] -- Symbol to Probability

data Expr = Symbol String
    | Boolean Bool
    | Number Double
    | Tuple [Expr]  -- An order list of symbols, or literals
    | Reference Expr Expr -- Symbol to any Expr
    | DistInit Expr -- Symbol and Mapping
    | DistJoin [Expr] -- 
    | Map Mapping
    deriving (Show)


