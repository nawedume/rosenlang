import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Core
import Eval
import Data.HashMap.Strict as H 
import Dist
import Parse



main :: IO ()
main = defaultMain tests


tests = [
    testGroup "=G= parse" [
        testProperty "=P= SymbolParse simple alpha single" parseSymbolTest1,
        testProperty "=P= SymbolParse simple alpha multi"  parseSymbolTest2,
        testProperty "=P= SymbolParse simple alphanumeric" parseSymbolTest3,
        testProperty "=P= SymbolParse simple spit parse" parseSymbolTest4,
        testProperty "=P= SymbolParse invalid symbol " parseSymbolTest5,
        
        testProperty "=P= RealParse simple alpha single" parseRealTest1,
        testProperty "=P= RealParse simple alpha multi"  parseRealTest2,
        testProperty "=P= RealParse simple alphanumeric" parseRealTest3,
        testProperty "=P= RealParse simple spit parse" parseRealTest4,
        testProperty "=P= RealParse invalid symbol " parseRealTest6,
        testProperty "=P= RealParse invalid symbol " parseRealTest7,

        testProperty "=P= BooleanParse true test" parseBooleanTest1,
        testProperty "=P= BooleanParse false test" parseBooleanTest2,

        testProperty "=P= TupleParse Long chain" parseTupleTest1,
        testProperty "=P= TupleParse empty" parseTupleTest2,
        testProperty "=P= TupleParse single element" parseTupleTest3,

        testProperty "=P= MeasureParse single map" parseMeasureTest1,
        testProperty "=P= MeasureParse double map" parseMeasureTest2,
        testProperty "=P= MeasureParse invalid mapping" parseMeasureTest3,
        testProperty "=P= MeasureParse no closeing bracket" parseMeasureTest4,

        testProperty "=P= JoinOpParse simple" parseJoinOpTest1,
        testProperty "=P= JoinOpParse inner distributions" parseJoinOpTest2,

        testProperty "=P= ReferenceParse recursiveReference" parseReferenceTest1,
        testProperty "=P= ReferenceParse simple number assignment" parseReferenceTest2,

        testProperty "=P= StrParse alpha string" parseStrTest1,
        testProperty "=P= StrParse number string" parseStrTest2,
        testProperty "=P= StrParse invalid string" parseStrTest3,

        testProperty "=P= IntParse single int" parseIntTest1,
        testProperty "=P= IntParse single negative int" parseIntTest2,
        testProperty "=P= IntParse multi digit" parseIntTest3,

        testProperty "=P= CatOpParse simple" parseCatOpTest1,

        testProperty "=P= RelOpParse lt" parseRealTest1,
        testProperty "=P= RelOpParse lte" parseRealTest2,
        testProperty "=P= RelOpParse eq" parseRealTest3,
        testProperty "=P= RelOpParse neq" parseRealTest4,

        testProperty "=P= ExpectQueryParse var" parseExpectation1,
        testProperty "=P= ExpectQueryParse dist" parseExpectation2,

        testProperty "=P= CreateGeoParse var" parseCreateGeometric,
        testProperty "=P= CreateBiParse dist" parseCreateBinomial,
        testProperty "=P= CreateSampleParse var" parseCreateSample],
    
    testGroup "=G= eval" [
        testProperty "=P= SymbolEval invalid" evalSymbolTest1,
        testProperty "=P= SymbolEval valid" evalSymbolTest1,

        testProperty "=P= BooleanEval True" evalBooleanTest1,
        testProperty "=P= BooleanEval False" evalBooleanTest2,

        testProperty "=P= IntEval 1" evalIntTest1,
        testProperty "=P= IntEval -1" evalIntTest2,
        testProperty "=P= IntEval multidigit" evalIntTest3,
        testProperty "=P= RealEval 0" evalRealTest1,
        testProperty "=P= RealEval 1" evalRealTest2,
        testProperty "=P= RealEval multi" evalRealTest3,

        testProperty "=P= Tuple test single" evalTupleTest1,
        testProperty "=P= Tuple test double" evalTupleTest2,
        testProperty "=P= Tuple test different types" evalTupleTest3,

        testProperty "=P= Distribution creation test, single measure" evalMeasureTest1,
        testProperty "=P= Distribution creation test, multi measure" evalMeasureTest2,

        testProperty "=P= JoinOp test single" evalJoinOpTest1,
        testProperty "=P= JoinOp test multi" evalJoinOpTest1,

        testProperty "=P= CatOp test single" evalCatopTest1,
        testProperty "=P= CatOp test multi" evalCatopTest2,

        testProperty "=P= EvalQuery Expect test" evalExpectQueryTest1,

        testProperty "=P= EvalCreate Sample test" evalDistCreateTest1,
        testProperty "=P= EvalCreate Geometric test" evalDistCreateTest2,
        testProperty "=P= EvalCreate Binomial test" evalDistCreateTest3

    ]]

parseSymbolTest1 = run exprParse "A;" == [(Symbol "A", "")]
parseSymbolTest2 = run exprParse "abc; " == [(Symbol "abc", " ")]
parseSymbolTest3 = run exprParse "abc12;" == [(Symbol "abc12", "")]
parseSymbolTest4 = run exprParse "aBc; a;" == [(Symbol "aBc", " a;")]
parseSymbolTest5 = run exprParse "1Bc; a;" == []

parseRealTest1 = run exprParse "1.0;" == [(Real 1.0, "")]
parseRealTest2 = run exprParse "123.0;" == [(Real 123.0, "")]
parseRealTest3 = run exprParse "0.321;" == [(Real 0.321, "")]
parseRealTest4 = run exprParse "0.0;" == [(Real 0.0, "")]
parseRealTest5 = run exprParse "321.345;" == [(Real 321.345, "")]
parseRealTest6 = run exprParse ".3;" == []
parseRealTest7 = run exprParse "1.b;" == []

parseBooleanTest1 = run exprParse "TRUE;" == [(Boolean True, "")]
parseBooleanTest2 = run exprParse "FALSE;" == [(Boolean False, "")]

parseTupleTest1 = run exprParse "(1.0, TRUE, (A -> 1.0), (ab, cd));" == [(Tuple [(Real 1.0), (Boolean True), (Measure [(Symbol "A", Real 1.0)]), Tuple [Symbol "ab", Symbol "cd"]], "")]
parseTupleTest2 = run exprParse "();" == []
parseTupleTest3 = run exprParse "(FALSE);" == [(Tuple [Boolean False], "")]

parseMeasureTest1 = run exprParse "(A -> 1.0);" == [ (Measure [(Symbol "A", Real 1.0)], "") ]
parseMeasureTest2 = run exprParse "(A -> 0.4, B -> 0.6);" == [(Measure [(Symbol "A", Real 0.4), (Symbol "B", Real 0.6)], "")]
parseMeasureTest3 = run exprParse "(A -> );" == []
parseMeasureTest4 = run exprParse "(A -> 1.0;" == []

parseJoinOpTest1 = run exprParse "* (A, B, C);" == [(JoinOp [Symbol "A", Symbol "B", Symbol "C"], "")]
parseJoinOpTest2 = run exprParse "* (A, (A -> 1.0), * (A, B));" == [(JoinOp 
        [Symbol "A",
        Measure [(Symbol "A", Real 1.0)],
         JoinOp [Symbol "A", Symbol "B"]
        ], "")]

parseReferenceTest1 = run exprParse "A = * (A);" == [(Reference (Symbol "A") (JoinOp [Symbol "A"]), "")]
parseReferenceTest2 = run exprParse "A =1.0;" == [(Reference (Symbol "A") (Real 1.0), "")]

parseStrTest1 = run exprParse "'abc;" == [(Str "abc", "")]
parseStrTest2 = run exprParse "'1abc;" == [(Str "1abc", "")]
parseStrTest3 = run exprParse "''1abc;" == []

parseIntTest1 = run exprParse "1;" == [(Int 1, "")]
parseIntTest2 = run exprParse "-1;" == [(Int (-1), "")]
parseIntTest3 = run exprParse "0521;" == [(Int 521, "")]

parseCatOpTest1 = run exprParse "+ (A, B, C);" == [(CatOp [Symbol "A", Symbol "B", Symbol "C"], "")]

parseRelOpTest1 = run exprParse "1 < 4" == [(RelOpExp "<" (Int 1) (Int 4), "")]
parseRelOpTest2 = run exprParse "1 <= 4" == [(RelOpExp "<=" (Int 1) (Int 4), "")]
parseRelOpTest3 = run exprParse "1 == 4" == [(RelOpExp "==" (Int 1) (Int 4), "")]
parseRelOpTest4 = run exprParse "1.0 != 4.0" == [(RelOpExp "!=" (Real 1) (Real 4), "")]

parseExpectation1 = run exprParse "EXPECT coin;" == [(ExpectQuery (Symbol "coin"), "")]
parseExpectation2 = run exprParse "EXPECT (1 -> 0.5, 2 -> 0.5);" == [(ExpectQuery (Measure [(Int 1, Real 0.5), (Int 2, Real 0.5)]), "")]

parseCreateGeometric = run exprParse "CREATE GEOMETRIC FROM coin WITH ('Heads);" == [(DistCreate Geometric (Symbol "coin") (Tuple [Str "Heads"]), "")]
parseCreateBinomial = run exprParse "CREATE BINOMIAL FROM coin WITH ('Heads, 5);" == [(DistCreate Binomial (Symbol "coin") (Tuple [Str "Heads", Int 5]), "")]
parseCreateSample = run exprParse "CREATE SAMPLE FROM coin WITH ('Heads);" == [(DistCreate Sample (Symbol "coin") (Tuple [Str "Heads"]), "")]

runAndEval :: String -> Env -> Val
runAndEval str env = 
    let parses = run exprParse str
    in  case parses of 
            [] -> error "No parses"
            ((parse,_):_) -> do
                case runProgram (eval parse) env of
                    (Left err, env) -> error "Invalid evaluation"
                    (Right val, env) -> val


evalSymbolTest1 = runProgram (eval (Symbol "a")) H.empty == (Left (VariableNotDefined "a"),fromList [])
evalSymbolTest2 = runProgram (eval (Symbol "a")) (H.fromList [("a", IntVal 1)]) == (Right $ IntVal 1,fromList [("a", IntVal 1)])

evalBooleanTest1 = runAndEval "TRUE;" H.empty == BoolVal True
evalBooleanTest2 = runAndEval "FALSE;" H.empty == BoolVal False

evalIntTest1 = runAndEval "1;" H.empty == IntVal 1
evalIntTest2 = runAndEval "-1;" H.empty == IntVal (-1)
evalIntTest3 = runAndEval "112;" H.empty == IntVal 112

evalRealTest1 = runAndEval "1.0;" H.empty == RealVal 1.0
evalRealTest2 = runAndEval "0.0;" H.empty == RealVal 0.0
evalRealTest3 = runAndEval "11.3;" H.empty == RealVal 11.3

evalStrTest1 = runAndEval "'Heads;" H.empty == StrVal "Heads"

evalTupleTest1 = runAndEval "(0);" H.empty == TupleVal [IntVal 0]
evalTupleTest2 = runAndEval "(0, 1);" H.empty == TupleVal [IntVal 0, IntVal 1]
evalTupleTest3 = runAndEval "(0, 'heads);" H.empty == TupleVal [IntVal 0, StrVal "heads"]

evalMeasureTest1 = runAndEval "('A -> 1.0);" H.empty == DistVal (Distribution [(StrVal "A", 1.0)])
evalMeasureTest2 = runAndEval "('A -> 0.4, 'B -> 0.6);" H.empty == DistVal (Distribution [(StrVal "A", 0.4), (StrVal "B", 0.6)])

evalJoinOpTest1 = runAndEval "* (('A -> 1.0));" H.empty == DistVal (Distribution [(TupleVal [StrVal "A"], 1.0)])
evalJoinOpTest2 = runAndEval "* (('A -> 1.0), ('A -> 1.0));" H.empty == DistVal (Distribution [(TupleVal [StrVal "A", StrVal "A"], 1.0)])

evalCatopTest1 = runAndEval "+ (('A -> 1.0));" H.empty == DistVal (Distribution [(StrVal "A", 1.0)])
evalCatopTest2 = runAndEval "+ (('A -> 1.0), ('B -> 1.0));" H.empty == DistVal (Distribution [(StrVal "A", 0.5), (StrVal "B", 0.5)])

evalExpectQueryTest1 = runAndEval "EXPECT (1 -> 0.1, 2 -> 0.1, 3 -> 0.1, 4 -> 0.1, 5 -> 0.1, 6 -> 0.5);" H.empty == RealVal 4.5

evalDistCreateTest1 = runAndEval "CREATE SAMPLE FROM ('h -> 0.5, 't -> 0.5) WITH (3);" (H.fromList [("__seed", IntVal 0)]) == TupleVal [StrVal "h", StrVal "h", StrVal "h"] 

getDistribution (DistVal (Distribution dist)) = dist
evalDistCreateTest2 = 
    let (first:second:_) = take 2 $ getDistribution $ runAndEval "CREATE GEOMETRIC FROM ('h -> 0.5, 't -> 0.5) WITH ('h);" H.empty
    in  first == (TupleVal [StrVal "h"], 0.5) && second == (TupleVal [StrVal "t", StrVal "h"], 0.25)

evalDistCreateTest3 = runAndEval "CREATE BINOMIAL FROM ('h -> 0.5, 't -> 0.5) WITH ('h, 2);" H.empty == DistVal (Distribution { dist = [(IntVal 0, 0.25), (IntVal 1, 0.5), (IntVal 2, 0.25)] })