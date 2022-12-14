module Types where

import qualified Data.Map as M
import Text.Printf

-------------------------------------------------------------------------------
-- FORTH TYPES
-------------------------------------------------------------------------------
data ForthPrimWord
    = LIT Int
    | DUP
    | PLUS
    | SWAP
    | DROP
    | MREAD -- `@`
    | MWRITE -- `!`
    deriving (Show)

defaultPrims :: [ForthPrimWord]
defaultPrims = [LIT 0, DUP, PLUS, SWAP, DROP, MREAD, MWRITE]

data ForthDefn =
    Defn
        { name :: String
        , body :: [ForthWord]
        }
    deriving (Show)

data ForthWord
    = WordPrim ForthPrimWord -- forth primitive
    | WordDefn String -- defined (non-primitive) word
    deriving (Show)

data ForthStmt
    = ForthStmtExec [ForthWord]
    | ForthStmtDefn ForthDefn
    deriving (Show)

type ForthEnv = M.Map String ForthDefn

defaultEnv :: ForthEnv
defaultEnv = M.empty

type AST = [ForthStmt]

-------------------------------------------------------------------------------
-- COMPILATION TYPES
-------------------------------------------------------------------------------
newtype Target =
    Target Int

-- a label, used to parametrize `Instruction` before pointer resolution.
newtype Label =
    Label String

instance Show Label where
    show (Label s) = s

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

data AluArgs =
    AluArgs
        { r'pc :: Bit
        , t' :: AluOp
        , t'n :: Bit
        , t'r :: Bit
        , n't :: Bit
        , rsd :: StackDelta
        , dsd :: StackDelta
        }
    deriving (Show)

data Instruction t
    = Lit Int
    | Jmp t
    | Cjmp t
    | Call t
    | ALU AluArgs

instance Show (Instruction Target) where
    show (Lit n) = "1" ++ printf "%015b" n
    show (Jmp t) = "000" ++ show t
    show (Cjmp t) = "001" ++ show t
    show (Call t) = "010" ++ show t
    show (ALU (AluArgs {r'pc, t', t'n, t'r, n't, dsd, rsd})) =
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

instance Show (Instruction Label) where
    show (Lit n) = "LIT " ++ show n
    show (Jmp t) = "JMP " ++ show t
    show (Cjmp t) = "CJMP " ++ show t
    show (Call t) = "CALL " ++ show t
    show (ALU (AluArgs {r'pc, t', t'n, t'r, n't, dsd, rsd})) = "ALU " ++ show t'
