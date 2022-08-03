import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Parse
import Core

main :: IO ()
main = defaultMain tests


tests = [testGroup "=G= parse" [
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
    testProperty "=P= IntParse multi digit" parseIntTest3

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