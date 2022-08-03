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

eval (RelOpExp op e1 e2) = do
    v1 <- eval e1
    v2 <- eval e2
    case Prelude.lookup op relationOpMap of
        Just boolOp -> return $ BoolVal $ boolOp v1 v2
        Nothing -> throwError $ InvalidExpression "Operation not defined for evaluation" 

eval (ExpectQuery distExpr) = do
    dVal <- eval distExpr
    case dVal of
        (DistVal distribution) -> do
            let realValDist = valToReal <$> distribution
            return $ RealVal $ expectation realValDist
        otherwise               -> throwError $ InvalidExpression "Expected a distribution."
    where
        valToReal (IntVal i) = fromIntegral i
        valToReal (RealVal d) = d
        valToReal val = 0.0

eval (DistCreate name distExpr argsExpr) = do
    dist <- eval distExpr
    args <- eval argsExpr
    case Prelude.lookup name nameToPreSetDistMap of
        Just evalFunc   -> evalFunc dist args
        Nothing         -> throwError $ InvalidExpression "No distribution of given name is defined." 

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

nameToPreSetDistMap :: [(PreSetCreate, Val -> Val -> Program Val)]
nameToPreSetDistMap = [
    (Geometric, evalGeometricDist), 
    (Binomial, evalBinomailDist),
    (Sample, evalSample)]

evalGeometricDist :: Val -> Val -> Program Val
evalGeometricDist (DistVal distribution) (TupleVal args) =
    case args of 
        [successArg] -> return $ DistVal $ TupleVal <$> (Dist.until distribution (== successArg))
        _   -> throwError $ InvalidExpression "Expected 1 argument, $success_condition"
        

evalBinomailDist :: Val -> Val -> Program Val
evalBinomailDist (DistVal distribution) (TupleVal args) =
    case args of
        [successArg, (IntVal n)] -> return $ DistVal $ (IntVal . fromIntegral) <$> (Dist.count (fromIntegral n) distribution (== successArg))
        _   -> throwError $ InvalidExpression "Expected 2 arguments, $success_condition, $number_of_trials"

evalSample :: Val -> Val -> Program Val
evalSample (DistVal distribution) (TupleVal args) =
    case args of
        [IntVal n] -> do
            env <- get
            case H.lookup "__seed" env of
                Just (IntVal seed) -> return $ TupleVal $ Dist.sample (fromIntegral seed) (fromIntegral n) distribution
                Nothing -> throwError $ UnknownError "Seed not defined."
        [] -> throwError $ InvalidExpression "Expected 1 argument, $number_of_trials"