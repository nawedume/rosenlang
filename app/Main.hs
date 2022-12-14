{- This is the main repl of the "Rosen" language -}

module Main where

import Core
import Parse
import Eval

import Data.HashMap.Strict as H
import System.Random


main :: IO ()
main = repl H.empty

{- | Step through one iteration of the repl. Only handles single statement lines.
-}
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

{- | The main repl loop. Resets the seed for sampling on each iteration. -}
repl :: Env -> IO ()
repl env = do
    seed <- newRand
    let env2 = H.insert "__seed" (IntVal $ fromIntegral seed) env -- internal value
    menv <- step env2
    case menv of
        Nothing -> pure () -- quit
        Just env' -> repl env'

newRand = randomIO :: IO Int