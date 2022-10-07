module J4 where

import Data.Functor
import System.Exit (die)
import Text.ParserCombinators.Parsec

import Types

data ForthPrimWord
    = LIT Int
    | DUP
    | PLUS
    | SWAP
    | DROP
    | MREAD -- `@`
    | MWRITE -- `!`
    deriving (Show)

type Program = [ForthPrimWord]

-------------------------------------------------------------------------------
-- PARSING
whitespace :: Parser ()
whitespace = void $ many1 $ oneOf " \n\t"

parseForth :: Parser Program
parseForth =
    choice (map (try . parseForthPrimWord) [SWAP, DUP, DROP, PLUS, LIT 0]) `sepEndBy`
    whitespace

parseForthPrimWord :: ForthPrimWord -> Parser ForthPrimWord
parseForthPrimWord (LIT _) = LIT . read <$> many1 digit
parseForthPrimWord DUP = string "DUP" $> DUP
parseForthPrimWord PLUS = string "+" $> PLUS
parseForthPrimWord SWAP = string "SWAP" $> SWAP
parseForthPrimWord DROP = string "DROP" $> DROP
parseForthPrimWord MREAD = string "@" $> MREAD
parseForthPrimWord MWRITE = string "!" $> MWRITE

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (p <* eof) ""

-------------------------------------------------------------------------------
-- COMPILATION
compileForthPrimWord :: ForthPrimWord -> String
compileForthPrimWord pw = concatMap show (cpw pw) ++ comment (show pw)
  where
    cpw :: ForthPrimWord -> [Instruction]
    cpw (LIT n) = [Lit n]
    cpw DUP =
        [ ALU
              { r'pc = Low
              , t' = T
              , t'n = High
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Inc
              }
        ]
    cpw PLUS =
        [ ALU
              { r'pc = Low
              , t' = Add
              , t'n = Low
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Dec
              }
        ]
    cpw SWAP =
        [ ALU
              { r'pc = Low
              , t' = N
              , t'n = High
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Null
              }
        ]
    cpw DROP =
        [ ALU
              { r'pc = Low
              , t' = N
              , t'n = Low
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Dec
              }
        ]
    cpw MREAD =
        [ ALU
              { r'pc = Low
              , t' = Ptr
              , t'n = Low
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Null
              }
        ]
    cpw MWRITE =
        [ ALU
              { r'pc = Low
              , t' = N
              , t'n = Low
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Dec
              }
        ]

compile :: Program -> String
compile = unlines . map compileForthPrimWord

comment :: String -> String
comment = (++) "\t//\t"

main :: IO ()
main
    -- args <- getArgs
 = do
    txt <- getContents
    parsed <-
        case parseWithEof parseForth txt of
            Left err -> die (show err)
            Right p -> return p
    putStrLn (compile parsed)
