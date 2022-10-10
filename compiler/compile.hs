{-# LANGUAGE LambdaCase #-}

module Compile where

import qualified Data.Map as M
import qualified Data.Set as S
import Types

-------------------------------------------------------------------------------
-- PRE-PROCESSING
-------------------------------------------------------------------------------
called :: AST -> [String]
called p = [name defn | (ForthStmtDefn defn) <- p]

-- restrict environment to only called functions.
calledEnv :: [String] -> ForthEnv -> ForthEnv
calledEnv called env = env `M.restrictKeys` S.fromList called

-- scope checking exists to make sure that the set of defined words is strictly a superset of used words.
-- scope checking is 'fatal' -- if there exists any used word not in the set of defined words, compilation is impossible.
-- `deadCodeElim` is the mirror function, it restricts the environment to defined words that are also used.
-- the result of scope checking and `deadCodeElim` is that the `AST`'s interdependent statements and definitions are brought fully 'in sync'.
deadCodeElim :: AST -> ForthEnv -> ForthEnv
deadCodeElim = calledEnv . called

-------------------------------------------------------------------------------
-- COMPILATION
-------------------------------------------------------------------------------
-- FIXME(jl): padding to full ROM size
compile :: ForthEnv -> AST -> String
compile env p = unlines $ map show (compileForth env p)

compileForth :: ForthEnv -> AST -> [Instruction Target]
compileForth env p =
    let code = concat [compileForthExec exec | (ForthStmtExec exec) <- p]
        cenv = compileForthEnv env
        renv = resolveForthEnv (Target (length code)) cenv
        compiled = resolve renv code
     in compiled

type ForthEnvCompiled = M.Map String [Instruction Label]

-- for every funciton in the environment, compile the body.
compileForthEnv :: ForthEnv -> ForthEnvCompiled
compileForthEnv = M.map compileForthDefn

-- compile function definition: compile body.
compileForthDefn :: ForthDefn -> [Instruction Label]
compileForthDefn (Defn {body}) = compileForthExec body

-- compile executable code.
compileForthExec :: [ForthWord] -> [Instruction Label]
compileForthExec = concatMap compileForthWord

compileForthWord :: ForthWord -> [Instruction Label]
compileForthWord (WordDefn name) = [Call (Label name)]
compileForthWord (WordPrim prim) = compileForthPrimWord prim

compileForthPrimWord :: ForthPrimWord -> [Instruction t]
compileForthPrimWord = cpw
  where
    cpw :: ForthPrimWord -> [Instruction t]
    cpw (LIT n) = [Lit n]
    cpw DUP =
        [ ALU $
          AluArgs
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
        [ ALU $
          AluArgs
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
        [ ALU $
          AluArgs
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
        [ ALU $
          AluArgs
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
        [ ALU $
          AluArgs
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
        [ ALU $
          AluArgs
              { r'pc = Low
              , t' = N
              , t'n = Low
              , t'r = Low
              , n't = Low
              , rsd = Null
              , dsd = Dec
              }
        ]

-------------------------------------------------------------------------------
-- NAME RESOLUTION
-------------------------------------------------------------------------------
comment :: String -> String
comment = (++) "\t//\t"

type ForthEnvResolved = M.Map String Target

-- from a starting pointer, generate a map of functions to their absolute addresses.
resolveForthEnv :: Target -> ForthEnvCompiled -> ForthEnvResolved
resolveForthEnv = (snd .) . M.mapAccum go
  where
    go :: Target -> [Instruction Label] -> (Target, Target)
    go (Target n) is = (Target n, Target (n + length is))

resolve :: ForthEnvResolved -> [Instruction Label] -> [Instruction Target]
resolve renv =
    map (\case
             Lit n -> Lit n
             Jmp (Label s) -> Jmp (renv M.! s)
             Cjmp (Label s) -> Cjmp (renv M.! s)
             Call (Label s) -> Call (renv M.! s)
             ALU alu -> ALU alu)
