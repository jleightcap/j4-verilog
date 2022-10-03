module Asm where

import Data.Functor
import Text.ParserCombinators.Parsec
import Text.Printf
import System.Environment (getArgs)
import System.Exit (die)

data ForthPrimWord
    = SWAP
    | DUP
    | DROP
    | PLUS
    | LIT Int
    deriving (Show)

type Program = [ForthPrimWord]

whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

parseForth :: Parser Program
parseForth = parseForthPrimWord `sepEndBy` whitespace

parseForthPrimWord :: Parser ForthPrimWord
parseForthPrimWord = try parseSwap <|> try parseDup <|> try parseDrop <|> try parseLit

parseSwap :: Parser ForthPrimWord
parseSwap = string "SWAP" $> SWAP

parseDup :: Parser ForthPrimWord
parseDup = string "DUP" $> DUP

parseDrop :: Parser ForthPrimWord
parseDrop = string "DROP" $> DROP

parseLit :: Parser ForthPrimWord
parseLit = LIT . read <$> many1 digit

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

compileForthPrimWord :: ForthPrimWord -> String
compileForthPrimWord SWAP = "011_0_0001_1000_00_00"
compileForthPrimWord DUP = "011_0_0000_1000_00_01"
compileForthPrimWord DROP = "011_0_0001_0000_00_11"
compileForthPrimWord (LIT n) = "1_" ++ printf "%015b" n

compile :: Program -> String
compile = unlines . map compileForthPrimWord

main :: IO ()
main = do
    args <- getArgs
    txt <- getContents
    parsed <- case parseWithEof parseForth txt of
        Left err -> die (show err)
        Right p -> return p
    putStrLn (compile parsed)
