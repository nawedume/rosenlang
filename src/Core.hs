{- This module holds the key terms defined for the language. -}

module Core where

import Control.Monad.Except
import Control.Monad.State
import Dist
import Data.HashMap.Strict as H 
import Data.List as L

{- | The core expressions that can be expressed in the language.
-}
data Expr = Symbol String
    | Boolean Bool
    | Real Double
    | Int Integer
    | Str String 
    | Tuple [Expr]  -- An order list of symbols, or literals
    | Measure [(Expr, Expr)] -- A mapping from a symbol to a number (probability)
    | JoinOp [Expr] -- Cartesian product of the distributions in the list
    | CatOp [Expr] -- Combination of distributions in the list
    | ExpectQuery Expr -- Expectation for a distribution
    | DistCreate PreSetCreate Expr Expr -- To create distributions; the name, the existing distribution and the parameters as a tuple
    | Reference Expr Expr -- Symbol to any Expr
    | RelOpExp String Expr Expr -- The operation, and the two expressions
    deriving (Show, Eq)

{- | A short hand for a Distribution of values.
-}
type Dist = Distribution Val

{- | Values gained from evaluating expressions.
-}
data Val = 
     NoneVal
    | BoolVal Bool
    | IntVal Integer
    | RealVal Double
    | StrVal [Char]
    | TupleVal [Val]
    | DistVal Dist -- A concrete value of a distribution
    deriving (Eq, Ord)

{- | Environment to hold the state of the language
-}
type Env = HashMap String Val

{- | A type to run through a step of the language. This will be used during evaluation,
it allows evaluation functions to handle errors and hold state.
-}
type Program a = (ExceptT RosenError
    (State Env)) a

{- | The errors that can occur in the language.
-}
data RosenError 
    = InvalidExpression String
    | VariableNotDefined String -- When a variable isn't in the env, parameter is the variable name
    | MeasureNotInBounds Double -- When a measure is lower than 0 or greater than 1
    | VarNotAMeasure Expr Val        -- When a variable is supposed to be a measure but isn't
    | UnknownError String       -- This should never occur 
    deriving (Show, Eq)


{- | Used to wrap tuple values
-}
listToString :: (Show a) => [a] -> [Char]
listToString l = "(" ++ (intercalate "," (L.map show l)) ++ ")"

{- | Used to display the different values. Usually using the normal haskell values.
-}
instance Show Val where
    show (DistVal dist) = show dist
    show (TupleVal vals) = listToString vals
    show (RealVal d) = show d
    show (IntVal i) = show i
    show (BoolVal b) = if b then "TRUE" else "FALSE"
    show (StrVal s) = show s
    show (NoneVal) = ""

{- | Easy way to run a program
-}
runProgram pro env = runState (runExceptT pro) env

{- | A mapping from relation to their Haskell function
-}
relationOpMap :: [(String, Val -> Val -> Bool)]
relationOpMap = [
    ("==", (==)),
    ("<=", (<=)),
    (">=", (>=)),
    ("<", (<)),
    (">", (>)),
    ("!=", (/=))]

{- | A mapping from a keyword distribution to it's Preset value.
It's used to easily allow for checking that all cases are accounted for.
-}
data PreSetCreate = Geometric | Binomial | Sample deriving (Show, Eq)
presetDistMap = [
    ("GEOMETRIC", Geometric),
    ("BINOMIAL", Binomial),
    ("SAMPLE", Sample)]