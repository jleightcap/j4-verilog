module Asm where

import Data.Functor
import Text.ParserCombinators.Parsec
import Text.Printf
import System.Environment (getArgs)
import System.Exit (die)

data ForthWord
    = SWAP
    | DUP
    | DROP
    | LIT Int
    deriving (Show)

type Program = [ForthWord]

whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

parseForth :: Parser Program
parseForth = parseForthWord `sepEndBy` whitespace

parseForthWord :: Parser ForthWord
parseForthWord = try parseSwap <|> try parseDup <|> try parseDrop <|> try parseLit

parseSwap :: Parser ForthWord
parseSwap = string "SWAP" $> SWAP

parseDup :: Parser ForthWord
parseDup = string "DUP" $> DUP

parseDrop :: Parser ForthWord
parseDrop = string "DROP" $> DROP

parseLit :: Parser ForthWord
parseLit = LIT . read <$> many1 digit

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

compileForthWord :: ForthWord -> String
compileForthWord SWAP = "011_0_0001_1000_00_00"
compileForthWord DUP = "011_0_0000_1000_00_01"
compileForthWord DROP = "011_0_0001_0000_00_11"
compileForthWord (LIT n) = "1_" ++ printf "%015b" n

compile :: Program -> String
compile = unlines . map compileForthWord

main :: IO ()
main = do
    args <- getArgs
    txt <- getContents
    parsed <- case parseWithEof parseForth txt of
        Left err -> die (show err)
        Right p -> return p
    putStrLn (compile parsed)
