module Core where

import Control.Monad.Except
import Control.Monad.State
import Dist
import Data.HashMap.Strict as H 
import Data.List as L

data Expr = Symbol String
    | Boolean Bool
    | Number Double
    | Str String 
    | Tuple [Expr]  -- An order list of symbols, or literals
    | Measure [(Expr, Expr)] -- A mapping from a symbol to a number (probability)
    | DistInit Expr -- Symbol and Mapping, mapping can be a Measure or a Symbol (variable)
    | DistJoin [Expr] -- 
    | Reference Expr Expr -- Symbol to any Expr
    deriving (Show, Eq)


type Dist = Distribution Val
data Val = 
      DistVal Dist -- A concrete value of a distribution
    | TupleVal [Val]
    | NumVal Double
    | BoolVal Bool
    | StrVal [Char]
    | MeasureVal [(Val, Probability)] -- creates a closure used for a distribution
    | NoneVal

type Env = HashMap String Val

type Program a = (ExceptT RosenError
    (State Env)) a

data RosenError 
    = InvalidExpression String
    | VariableNotDefined String -- When a variable isn't in the env, parameter is the variable name
    | MeasureNotInBounds Double -- When a measure is lower than 0 or greater than 1
    | VarNotAMeasure Expr Val        -- When a variable is supposed to be a measure but isn't
    | UnknownError String       -- This should never occur 
    deriving (Show)

listToString :: (Show a) => [a] -> [Char]
listToString l = "(" ++ (intercalate "," (L.map show l)) ++ ")"

instance Show Val where
    show (DistVal dist) = show dist
    show (TupleVal vals) = listToString vals
    show (NumVal d) = show d
    show (BoolVal b) = if b then "TRUE" else "FALSE"
    show (StrVal s) = show s
    show (MeasureVal arr) = listToString arr
    show (NoneVal) = ""

runProgram pro env = runState (runExceptT pro) env

constantDistribution :: a -> Distribution a
constantDistribution n = Distribution { dist = [(n, 1.0)] }