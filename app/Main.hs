module Main where

import System.IO

data Type
  = TInt
  | TBool
  | TArrow Type Type
  deriving(Show, Eq)

data Expr
  = Var String
  | Lam String Type Expr
  | App Expr Expr
  | LitInt Int
  | LitBool Bool
  deriving (Show, Eq)

type Context = [(String, Type)]

identityExpr :: Expr
identityExpr = Lam "x" TInt (Var "x")

appliedId :: Expr
appliedId = App identityExpr (Var "y")

k :: Expr
k = Lam "x" (TArrow TInt TInt) (Lam "y" (TArrow TInt TInt) (Var "x"))

main :: IO ()
main = do
  -- repl
  print $ typeCheck [] identityExpr
  print $ typeCheck [] (App identityExpr (LitBool True))
  print $ typeCheck [] (App identityExpr (LitInt 42))
  print $ eval identityExpr
  print $ eval appliedId
  print $ eval (App (App k (Lam "q" TInt (Var "q"))) (Lam "y" TInt (Var "y")))


typeCheck :: Context -> Expr -> Either String Type
typeCheck ctx (Var x)     = case lookup x ctx of
  Nothing -> Left ("unbound variable: " ++ x)
  Just t  -> Right t
typeCheck _ (LitInt _)  = Right TInt
typeCheck _ (LitBool _) = Right TBool
typeCheck ctx (Lam x t body) =
  case typeCheck ((x, t) : ctx) body of
    Left err -> Left err
    Right t2 -> Right (TArrow t t2)
typeCheck ctx (App f a) =
  case typeCheck ctx f of
    Left err -> Left err
    Right t  -> case t of
      (TArrow t1 t2) -> case typeCheck ctx a of
        Left err -> Left err
        Right t3 -> if t1 /= t3
                      then Left ("type mismatch")
                      else Right t2
      _ -> Left ("Not a function")
  

eval :: Expr -> Expr
eval (Var x)        = Var x
eval (Lam x t body) = Lam x t body
eval (LitInt i)     = LitInt i
eval (LitBool b)    = LitBool b
eval (App f a)      =
  case eval f of
    Lam x _ body -> eval (subst x (eval a) body)
    other        -> App other (eval a)


subst :: String -> Expr -> Expr -> Expr
subst x value expr =
  case expr of
    LitInt n     -> LitInt n
    LitBool b    -> LitBool b
    Var n        -> if x == n then value else Var n
    Lam n t body -> if x == n then Lam n t body else Lam n t (subst x value body)
    App f p      -> App (subst x value f) (subst x value p)

repl :: IO ()
repl = do
  putStr "typey > "
  hFlush stdout
  line <- getLine
  case line of
    ":quit" -> return ()
    ":q"    -> return ()
    input   -> do
      processInput input
      repl

processInput :: String -> IO ()
processInput input = do 
  putStrLn input

