module Main where

import Lib
import Core
import Parse
import Eval

import Data.HashMap.Strict as H


main :: IO ()
main = repl H.empty

step :: Env -> IO (Maybe Env)
step env = do
    putStr ">>> "
    line <- getLine
    case line of
        "quit" -> putStrLn "bye!" >> return Nothing
        _ -> Just <$> parseAndEval env line
    where
        parseAndEval env line = do
            let parses = run exprParse line
            case parses of 
                [] -> do
                    putStrLn "Could not parse."
                    return env
                ((parse,_):_) -> do
                    case runProgram (eval parse) env of
                        (Left err, env) -> do
                            putStrLn "Error when running: "
                            putStrLn ("  " ++ show err)
                            return env
                        (Right val, env) -> do
                            putStrLn (show val)
                            return env

repl :: Env -> IO ()
repl env = do
    menv <- step env
    case menv of
        Nothing -> pure () -- quit
        Just env' -> repl env'
