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
    testProperty "=P= NumberParse invalid symbol " parseNumberTest7

    
    ]]

parseSymbolTest1 = run exprParse "A" == [(Symbol "A", "")]
parseSymbolTest2 = run exprParse "abc " == [(Symbol "abc", " ")]
parseSymbolTest3 = run exprParse "abc12" == [(Symbol "abc12", "")]
parseSymbolTest4 = run exprParse "aBc a" == [(Symbol "aBc", " a")]
parseSymbolTest5 = run exprParse "1Bc a" == []

parseNumberTest1 = run exprParse "1.0" == [(Number 1.0, "")]
parseNumberTest2 = run exprParse "123.0" == [(Number 123.0, "")]
parseNumberTest3 = run exprParse "0.321" == [(Number 0.321, "")]
parseNumberTest4 = run exprParse "0.0" == [(Number 0.0, "")]
parseNumberTest5 = run exprParse "321.345" == [(Number 321.345, "")]
parseNumberTest6 = run exprParse "4" == []
parseNumberTest7 = run exprParse "1.b" == []


