module Parse where

import Dist
import Core

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
    
empty = Parser (\input -> [])

charParser :: Char -> Parser Char
charParser a = Parser (\inp -> case inp of
    (s:rs) | a == s -> [(a, rs)]
    otherwse        -> [])

stringParser :: String -> Parser String
stringParser "" = return ""
stringParser (x:xs) = do
    a <- charParser x
    rs <- stringParser xs
    return $ a:rs

(<|>) :: Parser t -> Parser t -> Parser t
(Parser p1) <|> Parser (p2) = Parser (\inp -> take 1 $ p1 inp ++ p2 inp)

many :: Parser Char -> Parser [Char]
many p = next <|> return "" where
     next = do 
        v <- p
        vv <- many p
        return (v:vv)

many1 :: Parser Char -> Parser [Char]
many1 p = do
    c <- p
    cc <- many p
    return $ c:cc

sat :: (Char -> Bool) -> Parser Char
sat pred = Parser (\inp -> case inp of
    (x:xs) | pred x -> [(x, xs)]
    otherwse -> [])

oneOf :: [Char] -> Parser Char
oneOf xx = sat (\a -> a `elem` xx)

spaces :: Parser [Char]
spaces = many (oneOf " \t")

spaces1 :: Parser [Char]
spaces1 = many1 (oneOf " \t")

terminator :: Parser [Char]
terminator = many1 (charParser ';')

inbetween :: Char -> Char -> Parser Char
inbetween a b = sat (\x -> x >= a && x <= b)

digit :: Parser Char
digit = inbetween '0' '9'

digits :: Parser String
digits = many1 digit

alpha :: Parser Char
alpha = (inbetween 'A' 'Z') <|> (inbetween 'a' 'z')

alphanumeric :: Parser Char
alphanumeric = alpha <|> digit

dot :: Parser Char
dot = charParser '.'

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

exprParse :: Parser Expr
exprParse = do
    spaces
    parse <- exprSetParse
    terminator
    return $ parse


exprSetParse = foldr1 (\x y -> x <|> y) grammerGens where
        grammerGens = [
            relOpParse,
            realParse,
            intParse,
            booleanParse,
            measureParse,
            distOpParse,
            tupleParse,
            expectationParse,
            refParse,
            strParse,
            symbolParse]

realParse :: Parser Expr
realParse = do
    d <- double
    return $ Real d

intParse :: Parser Expr
intParse = do
    i <- integer
    return $ Int i

strParse :: Parser Expr
strParse = do
    charParser '\''
    s <- many alphanumeric
    return $ Str s

symbolParse :: Parser Expr
symbolParse = do
    first <- alpha
    rest <- many alphanumeric
    return $ Symbol $ first:rest

booleanParse :: Parser Expr
booleanParse = do
    bstr <- (stringParser "TRUE") <|> (stringParser "FALSE")
    return $ Boolean $ bstr == "TRUE"

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

tupleParse :: Parser Expr
tupleParse = do
    l <- listParse exprSetParse
    return $ Tuple l


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

refParse :: Parser Expr
refParse = do
    sym <- symbolParse
    operC '='
    e <- exprSetParse
    return $ Reference sym e

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
distOperatorParser = foldr1 (<|>) (map (operS . fst) distOps) 

relOpParse :: Parser Expr
relOpParse = do
    first <- validParsers
    op <- foldr1 (<|>) (map (operS . fst) relationOpMap)
    second <- validParsers <|> relOpParse
    return $ RelOpExp op first second
    where
        validParsers = symbolParse <|> strParse <|> intParse <|> realParse

expectationParse :: Parser Expr
expectationParse = do
    operS "EXPECT"
    distExpr <- exprSetParse 
    return $ ExpectQuery distExpr
