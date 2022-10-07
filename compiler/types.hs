module Types where

import Text.Printf

newtype Target =
    Target Int

instance Show Target where
    show (Target i) = printf "%015b" i

data Bit
    = Low
    | High

instance Show Bit where
    show Low = "0"
    show High = "1"

type R'PC = Bit

type T' = Bit

type T'N = Bit

type T'R = Bit

type N'T = Bit

data StackDelta
    = Null
    | Inc
    | DecDec
    | Dec
    deriving (Enum)

instance Show StackDelta where
    show = printf "%02b" . fromEnum

data AluOp
    = T
    | N
    | Add
    | And
    | Or
    | Xor
    | Not
    | Equal
    | Less
    | Rshift
    | Sub1
    | R
    | Ptr
    | Lshift
    | Depth
    | Uless
    deriving (Enum)

instance Show AluOp where
    show = printf "%04b" . fromEnum

data Instruction
    = Lit Int
    | Jmp Target
    | Cjmp Target
    | Call Target
    | ALU
          { r'pc :: Bit
          , t' :: AluOp
          , t'n :: Bit
          , t'r :: Bit
          , n't :: Bit
          , rsd :: StackDelta
          , dsd :: StackDelta
          }

instance Show Instruction where
    show (Lit n) = "1" ++ printf "%015b" n
    show (Jmp t) = "000" ++ show t
    show (Cjmp t) = "001" ++ show t
    show (Call t) = "010" ++ show t
    show (ALU {r'pc, t', t'n, t'r, n't, dsd, rsd}) =
        concat
            [ "011"
            , show r'pc
            , show t'
            , show t'n
            , show t'r
            , show n't
            , "0"
            , show rsd
            , show dsd
            ]
