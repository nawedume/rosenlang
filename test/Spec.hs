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
    
    testProperty "=P= NumberParse simple alpha single" parseNumberTest1,
    testProperty "=P= NumberParse simple alpha multi"  parseNumberTest2,
    testProperty "=P= NumberParse simple alphanumeric" parseNumberTest3,
    testProperty "=P= NumberParse simple spit parse" parseNumberTest4,
    testProperty "=P= NumberParse invalid symbol " parseNumberTest6,
    testProperty "=P= NumberParse invalid symbol " parseNumberTest7,

    testProperty "=P= BooleanParse true test" parseBooleanTest1,
    testProperty "=P= BooleanParse false test" parseBooleanTest2,

    testProperty "=P= TupleParse Long chain" parseTupleTest1,
    testProperty "=P= TupleParse empty" parseTupleTest2,
    testProperty "=P= TupleParse single element" parseTupleTest3,

    testProperty "=P= MeasureParse single map" parseMeasureTest1,
    testProperty "=P= MeasureParse double map" parseMeasureTest2,
    testProperty "=P= MeasureParse invalid mapping" parseMeasureTest3,
    testProperty "=P= MeasureParse no closeing bracket" parseMeasureTest4,

    testProperty "=P= DistInitParse double map" parseDistInitTest1,
    testProperty "=P= DistInitParse single map" parseDistInitTest2,
    testProperty "=P= DistInitParse no map" parseDistInitTest3,

    testProperty "=P= DistJoinParse simple" parseDistJoinTest1,
    testProperty "=P= DistJoinParse inner distributions" parseDistJoinTest2,

    testProperty "=P= ReferenceParse recursiveReference" parseReferenceTest1,
    testProperty "=P= ReferenceParse simple number assignment" parseReferenceTest2
    ]]

parseSymbolTest1 = run exprParse "A;" == [(Symbol "A", "")]
parseSymbolTest2 = run exprParse "abc; " == [(Symbol "abc", " ")]
parseSymbolTest3 = run exprParse "abc12;" == [(Symbol "abc12", "")]
parseSymbolTest4 = run exprParse "aBc; a;" == [(Symbol "aBc", " a;")]
parseSymbolTest5 = run exprParse "1Bc; a;" == []

parseNumberTest1 = run exprParse "1.0;" == [(Number 1.0, "")]
parseNumberTest2 = run exprParse "123.0;" == [(Number 123.0, "")]
parseNumberTest3 = run exprParse "0.321;" == [(Number 0.321, "")]
parseNumberTest4 = run exprParse "0.0;" == [(Number 0.0, "")]
parseNumberTest5 = run exprParse "321.345;" == [(Number 321.345, "")]
parseNumberTest6 = run exprParse "4;" == []
parseNumberTest7 = run exprParse "1.b;" == []

parseBooleanTest1 = run exprParse "TRUE;" == [(Boolean True, "")]
parseBooleanTest2 = run exprParse "FALSE;" == [(Boolean False, "")]

parseTupleTest1 = run exprParse "(1.0, TRUE, (A -> 1.0), (ab, cd));" == [(Tuple [(Number 1.0), (Boolean True), (Measure [(Symbol "A", Number 1.0)]), Tuple [Symbol "ab", Symbol "cd"]], "")]
parseTupleTest2 = run exprParse "();" == []
parseTupleTest3 = run exprParse "(FALSE);" == [(Tuple [Boolean False], "")]

parseMeasureTest1 = run exprParse "(A -> 1.0);" == [ (Measure [(Symbol "A", Number 1.0)], "") ]
parseMeasureTest2 = run exprParse "(A -> 0.4, B -> 0.6);" == [(Measure [(Symbol "A", Number 0.4), (Symbol "B", Number 0.6)], "")]
parseMeasureTest3 = run exprParse "(A -> );" == []
parseMeasureTest4 = run exprParse "(A -> 1.0;" == []

parseDistInitTest1 = run exprParse "DIST (A->0.5,B->0.5);" == [(DistInit (Measure [(Symbol "A", Number 0.5), (Symbol "B", Number 0.5)]) ,"")]
parseDistInitTest2 = run exprParse "DIST (A->1.0);" == [(DistInit (Measure [(Symbol "A", Number 1.0)]) ,"")]
parseDistInitTest3 = run exprParse "DIST ();" == []

parseDistJoinTest1 = run exprParse "DIST (A, B, C);" == [(DistJoin [Symbol "A", Symbol "B", Symbol "C"], "")]
parseDistJoinTest2 = run exprParse "DIST (A, DIST (A -> 1.0), DIST (A, B));" == [(DistJoin 
        [Symbol "A",
         DistInit (Measure [(Symbol "A", Number 1.0)]),
         DistJoin [Symbol "A", Symbol "B"]
        ], "")]

parseReferenceTest1 = run exprParse "A = DIST (A);" == [(Reference (Symbol "A") (DistJoin [Symbol "A"]), "")]
parseReferenceTest2 = run exprParse "A =1.0;" == [(Reference (Symbol "A") (Number 1.0), "")]