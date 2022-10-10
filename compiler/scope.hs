module Scope where

import qualified Data.Map as M
import System.Exit (die)
import Types

-------------------------------------------------------------------------------
-- SCOPE CHECKING PASS
-------------------------------------------------------------------------------
scope :: AST -> ForthEnv -> IO ForthEnv
scope [] env = pure env
scope (ForthStmtExec forthWords:stmts) env =
    scopeForthWords env forthWords >>= scope stmts
scope (ForthStmtDefn forthDefn:stmts) env =
    scopeForthDefn env forthDefn >>= scope stmts

scopeForthWords :: ForthEnv -> [ForthWord] -> IO ForthEnv
scopeForthWords env fwds = either (die . fail fwds) pure (scope' env fwds)
  where
    scope' :: ForthEnv -> [ForthWord] -> Either String ForthEnv
    scope' env [] = Right env
    scope' env ((WordDefn binding):fwds) =
        case M.lookup binding env of
            Just _ -> Right env
            Nothing -> Left binding
    scope' env (_:fwds) = Right env
    fail :: [ForthWord] -> String -> String
    fail fwds badTerm =
        unwords ["scope checking", show fwds, "failed with term:", badTerm]

scopeForthDefn :: ForthEnv -> ForthDefn -> IO ForthEnv
scopeForthDefn env defn@(Defn {name = name, body = body}) =
    let env' = M.insert name defn env
     in scopeForthWords env' body -- fuck it go crazy recursion is allowed
