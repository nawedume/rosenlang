{-
Most of the code was taken from the UIUC CS421 Lecture Slides by Mattox Beckman
-}

module Parse where

import Dist
import Core

{- | Combinator Parser
-}
data Parser t = Parser (String -> [(t, String)])
run (Parser p) = p

instance Functor Parser where
    fmap f (Parser p1) = Parser (\input -> [(f t, rs) | (t, rs) <- p1 input])

instance Applicative Parser where
    pure a = Parser (\input -> [(a, input)])
    (Parser pf) <*> (Parser p) = Parser (\input -> [(f t, rrs) |
     (f, rs) <- pf input,
     (t, rrs) <- (p rs)])


instance Monad Parser where
    (Parser p1) >>= f = Parser (\input -> [(t2, rs2) | 
        (t1, rs) <- p1 input,
        let (Parser p2) = f t1,
        (t2, rs2) <- p2 rs])
    

{- | Parse a single character
-}
charParser :: Char -> Parser Char
charParser a = Parser (\inp -> case inp of
    (s:rs) | a == s -> [(a, rs)]
    otherwse        -> [])

{- | Parse a string
-}
stringParser :: String -> Parser String
stringParser "" = return ""
stringParser (x:xs) = do
    a <- charParser x
    rs <- stringParser xs
    return $ a:rs

{- | Used to OR parsers together. Not commutative.
-}
(<|>) :: Parser t -> Parser t -> Parser t
(Parser p1) <|> Parser (p2) = Parser (\inp -> take 1 $ p1 inp ++ p2 inp)

{- | Parse 0 or more of the same characters.
-}
many :: Parser Char -> Parser [Char]
many p = next <|> return "" where
     next = do 
        v <- p
        vv <- many p
        return (v:vv)

{- | Parse 1 or more of the same character.
-}
many1 :: Parser Char -> Parser [Char]
many1 p = do
    c <- p
    cc <- many p
    return $ c:cc

{- Parse a char if the predicate is true
-}
sat :: (Char -> Bool) -> Parser Char
sat pred = Parser (\inp -> case inp of
    (x:xs) | pred x -> [(x, xs)]
    otherwse -> [])

{- | Parse if char is an element of xx
-}
oneOf :: [Char] -> Parser Char
oneOf xx = sat (\a -> a `elem` xx)

{- | Parse if whitespace.
-}
spaces :: Parser [Char]
spaces = many (oneOf " \t")

{- | Parse if at least 1 whitespace character exists.
-}
spaces1 :: Parser [Char]
spaces1 = many1 (oneOf " \t")

{- | Parse if a terminator ';' exists.
-}
terminator :: Parser [Char]
terminator = many1 (charParser ';')

{- | Parse if a character is inbetween two values.
-}
inbetween :: Char -> Char -> Parser Char
inbetween a b = sat (\x -> x >= a && x <= b)

{- | Parse if character is a digit.
-}
digit :: Parser Char
digit = inbetween '0' '9'

{- | Parse a string is a sequence of digits.
-}
digits :: Parser String
digits = many1 digit

{- | parse if Char is alpha character.
-}
alpha :: Parser Char
alpha = (inbetween 'A' 'Z') <|> (inbetween 'a' 'z')

{- | Parse if Char is alpha numeric.
-}
alphanumeric :: Parser Char
alphanumeric = alpha <|> digit

{- | Parse if char is a dot.
-}
dot :: Parser Char
dot = charParser '.'

{- Parse of string is a double, return a double.
-}
double :: Parser Double
double = do
    d1 <- digits
    dot
    d2 <- digits
    return $ (read (d1 ++ "." ++ d2) :: Double)

integer :: Parser Integer
integer = do
    c <- charParser '-' <|> digit
    d1 <- many digit
    return $ (read (c:d1) :: Integer)

probability :: Parser Probability
probability = underOne <|> one where
    one = do
        charParser '1'
        dot
        many (charParser '0')
        return 1.0

    underOne = do
        many (charParser '0')
        dot
        d2 <- digits
        return $ (read ("0." ++ d2) :: Double)

operC :: Char -> Parser Char
operC c = do
    spaces
    charParser c
    spaces
    return c

operS :: String -> Parser String
operS c = do
    spaces
    stringParser c
    spaces
    return c

{- Parsing Expressions of Rosen -}
parseSeqExpr :: String -> [Expr]
parseSeqExpr line = case (run exprParse line) of
        [] -> []
        ((x, rs):_) ->  let rest = parseSeqExpr rs
                        in x:rest

{- Parse an expression defined in the Core module.
-}
exprParse :: Parser Expr
exprParse = do
    spaces
    parse <- exprSetParse
    terminator
    return $ parse

{- A parser for the set of allowed expressions in the system. -}
exprSetParse = foldr1 (\x y -> x <|> y) grammerGens where
        grammerGens = [
            relOpParse,
            realParse,
            intParse,
            booleanParse,
            measureParse,
            distOpParse,
            distCreateParse,
            tupleParse,
            expectationParse,
            refParse,
            strParse,
            symbolParse]

{- A parser to parse Real Number expressions-}
realParse :: Parser Expr
realParse = do
    d <- double
    return $ Real d

{- A parser to parse Integer Expression -}
intParse :: Parser Expr
intParse = do
    i <- integer
    return $ Int i

{- A parser to parse String expressions -}
strParse :: Parser Expr
strParse = do
    charParser '\''
    s <- many alphanumeric
    return $ Str s

{- A parser to parse symbols (vars) -}
symbolParse :: Parser Expr
symbolParse = do
    first <- alpha
    rest <- many alphanumeric
    return $ Symbol $ first:rest

{- A parser to parse Booleans -}
booleanParse :: Parser Expr
booleanParse = do
    bstr <- (stringParser "TRUE") <|> (stringParser "FALSE")
    return $ Boolean $ bstr == "TRUE"

{- A parser to parse Measures (Distributions) -}
measureParse :: Parser Expr
measureParse = do
    l <- listParse parseMeasure
    return $ Measure l
    where
        parseMeasure :: Parser (Expr, Expr)
        parseMeasure = do
            s <- exprSetParse
            operS "->"
            d <- realParse
            return (s, d)

{- A parser to parse tuples -}
tupleParse :: Parser Expr
tupleParse = do
    l <- listParse exprSetParse
    return $ Tuple l

{- A parser that parses a list of expressions for another parser. -}
listParse :: Parser a -> Parser [a]
listParse parser = do
    operC '('
    m <- parseLists
    return m
    where
        parseLists = do
            a <- parser
            c <- operC ',' <|> operC ')'
            if (c == ',') 
                then
                    do
                        rest <- parseLists
                        return $ a:rest
                else
                    return [a]

{- A parser for variable assignment. -}
refParse :: Parser Expr
refParse = do
    sym <- symbolParse
    operC '='
    e <- exprSetParse
    return $ Reference sym e

{- A parser for a distribution operation. -}
distOpParse :: Parser Expr
distOpParse = do
    op <- distOperatorParser
    distExprs <- listParse $ measureParse <|> symbolParse <|> distOpParse
    case lookup op distOps of
        Just opCons -> return $ opCons distExprs
        Nothing -> error "Operator should always be defined here, due to parse"
    where
        validParsers = (measureParse <|> symbolParse)
        distOps = [("*", JoinOp), 
                ("+", CatOp)]
        distOperatorParser = keywordParser distOps

{- A parser for relation operations -}
relOpParse :: Parser Expr
relOpParse = do
    first <- validParsers
    op <- foldr1 (<|>) (map (operS . fst) relationOpMap)
    second <- validParsers <|> relOpParse
    return $ RelOpExp op first second
    where
        validParsers = symbolParse <|> strParse <|> intParse <|> realParse

{- A parser for the expectation operation -}
expectationParse :: Parser Expr
expectationParse = do
    operS "EXPECT"
    distExpr <- exprSetParse 
    return $ ExpectQuery distExpr

{- A parser for distribution operations that create a new sequence/measure -}
distCreateParse :: Parser Expr
distCreateParse = do
    operS "CREATE"
    name <- keywordParser presetDistMap
    operS "FROM"
    distExpr <- exprSetParse
    operS "WITH"
    paramsTuple <- tupleParse
    case lookup name presetDistMap of
        Just name' -> return $ DistCreate name' distExpr paramsTuple
        Nothing -> error "Distribution not defined for parsing."

{- A parser generator for parsing key words. Used mostly for function creation. -}
keywordParser :: [(String, a)] -> Parser String
keywordParser mapping = foldr1 (<|>) (map (operS . fst) mapping)
