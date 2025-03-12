--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk n k = factk (n - 1) (\v -> k (v * n)) 

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk (x:xs) keven kodd 
    | xs /= [] && even x = evenoddk xs (\v -> keven $ v + x) kodd
    | xs /= [] && odd x = evenoddk xs keven (\v -> kodd $ v + x)
    | xs == [] && even x = keven x
    | otherwise = kodd x

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (AppExp _ _) = False
isSimple (IntExp _) = True
isSimple (VarExp _) = True
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp op e1 e2) = isSimple e1 && isSimple e2

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k acc = ((AppExp k (IntExp i)), acc)
cpsExp (VarExp v) k acc = ((AppExp k (VarExp v)), acc)
--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k acc 
    | isSimple e = (AppExp (AppExp f e) k, acc)
    | otherwise = let( v, nextAcc) = gensym(acc) in 
        cpsExp e (LamExp v ((AppExp (AppExp f (VarExp v)) k))   ) nextAcc
--- #### Define `cpsExp` for Operator Expressions
cpsExp e@(OpExp op e1 e2) k acc
    | (isSimple e1) && (isSimple e2) = (AppExp k e, acc)
    | (not $ isSimple e1) && (isSimple e2) = let (v,nextAcc) = gensym(acc) in cpsExp e1 (LamExp v (AppExp k (OpExp op (VarExp v) e2))) nextAcc
    | (isSimple e1) && (not $ isSimple e2) = let (v,nextAcc) = gensym(acc) in cpsExp e2 (LamExp v (AppExp k (OpExp op e1 (VarExp v)))) nextAcc
    | otherwise = 
        let (v1, tempAcc) = gensym(acc)
            (v2, nextAcc) = gensym(tempAcc) in 
                let (e2output, nextAcc1) = cpsExp e2 (LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))) nextAcc in
                    cpsExp e1 (LamExp v1 e2output) nextAcc1
--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k acc
    | isSimple e1 = 
        let (e2prime, _) = cpsExp e2 k acc 
            (e3prime, newacc) = cpsExp e3 k acc in
                (IfExp e1 e2prime e3prime, newacc)
    | otherwise = 
        let (e2prime, _) = cpsExp e2 k acc 
            (e3prime, tempacc) = cpsExp e3 k acc
            (v, newacc) = gensym(tempacc) in
                cpsExp e1 (LamExp v (IfExp (VarExp v) e2prime e3prime)) newacc
--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl f params exp) = 
    let newparams = params ++ ["k"]
        (newexp, _) = cpsExp exp (VarExp "k") 0 in
            (Decl f newparams newexp)

