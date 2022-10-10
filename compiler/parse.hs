module Parse where

import Data.Functor
import System.Exit (die)
import Text.ParserCombinators.Parsec
import Types

-------------------------------------------------------------------------------
-- PARSING
-------------------------------------------------------------------------------
whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

-- parse an arbitary forth word; either  a primitive or something extant in the environment.
parseForthWord :: Parser ForthWord
parseForthWord = try parseForthWordPrim <|> try parseForthWordDefn

-- parse a forth primitive word
parseForthWordPrim :: Parser ForthWord
parseForthWordPrim = choice $ map (fmap WordPrim . try . pfp) defaultPrims
  where
    pfp :: ForthPrimWord -> Parser ForthPrimWord
    -- TODO(jl): case insensitive.
    pfp (LIT _) = LIT . read <$> many1 digit
    pfp DUP = string "DUP" $> DUP
    pfp PLUS = string "+" $> PLUS
    pfp SWAP = string "SWAP" $> SWAP
    pfp DROP = string "DROP" $> DROP
    pfp MREAD = string "@" $> MREAD
    pfp MWRITE = string "!" $> MWRITE

-- parse a non-primitive (user defined) forth word
parseForthWordDefn :: Parser ForthWord
parseForthWordDefn = WordDefn <$> many1 alphaNum

parseForthDefn :: Parser ForthDefn
parseForthDefn = do
    void $ string ":"
    optional whitespace
    name <- many1 alphaNum
    optional whitespace
    body <- parseForthWord `sepEndBy` whitespace
    void $ string ";"
    return Defn {name = name, body = body}

parseForthStmtDefn :: Parser ForthStmt
parseForthStmtDefn = ForthStmtDefn <$> parseForthDefn

parseForthStmtExec :: Parser ForthStmt
parseForthStmtExec = ForthStmtExec <$> parseForthWord `sepEndBy1` whitespace

parseForthStmt :: Parser ForthStmt
parseForthStmt = try parseForthStmtDefn <|> try parseForthStmtExec

parseForth :: Parser Program
parseForth = parseForthStmt `sepEndBy` optional whitespace

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

parseWithLeftOver :: Parser a -> String -> Either ParseError (a, String)
parseWithLeftOver p = parse ((,) <$> p <*> leftOver) ""
  where
    leftOver = manyTill anyToken eof

parseIO :: String -> IO Program
parseIO s =
    case parseWithEof parseForth s of
        Left err -> die (show err)
        Right p -> pure p
