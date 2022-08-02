module Eval where

import Core
import Control.Monad.Except (throwError)
import Control.Monad.State (get, put)
import Data.HashMap.Strict as H 
import Dist

eval :: Expr -> Program Val
eval (Symbol s) = do
    env <- get
    case H.lookup s env of
        Just v -> return v
        Nothing -> throwError $ VariableNotDefined s


eval (Real d) = return $ RealVal d
eval (Int i) = return $ IntVal i
eval (Boolean b) = return $ BoolVal b
eval (Str s) = return $ StrVal s

eval (Tuple exprs) = do
    vals <- mapExpr eval exprs
    return $ TupleVal vals

eval (Measure mapping) = do
    mapVals <- mapExpr evalMapping mapping
    return $ DistVal (Distribution { dist = mapVals }) where
        evalMapping :: (Expr, Expr) -> Program (Val, Probability)
        evalMapping (key, Real d) = do
            if d < 0.0 || d > 1.0 then throwError $ MeasureNotInBounds d
            else do
                v <- eval key
                return $ (v, d)

eval (Reference (Symbol s) e) = do
    val <- eval e
    env <- get
    let nenv = H.insert s val env
    put nenv
    return NoneVal

eval (JoinOp distExprs) = do
    dists <- mapExpr evalDist distExprs
    let finalDist = (\valArr -> TupleVal valArr) <$> join dists
    return $ DistVal finalDist

eval (CatOp distExprs) = do
    dists <- mapExpr evalDist distExprs
    return $ DistVal $ cat dists

eval _ = do
    throwError $ InvalidExpression "Expression not defined"

evalDist distExpr = do
    dv <- eval distExpr
    case dv of
        (DistVal dist)  -> return dist
        otherwise       -> throwError $ UnknownError "a non-distribution value was derived from a dist expr"

mapExpr :: (a -> Program b) -> [a] -> Program [b]
mapExpr _ [] = return []
mapExpr f (x:xs) = do
    v <- f x
    rest <- mapExpr f xs
    return $ v:rest