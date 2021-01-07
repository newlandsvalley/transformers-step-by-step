module Data 
  ( 
    Name(..)
  , Exp (..)
  , Value (..)
  , Environment
  ) where

import Prelude
import Data.Map as Map

type Name = String -- variable names

data Exp -- expressions
  =  Lit Int
   | Var Name
   | Plus Exp Exp
   | Abs Name Exp
   | App Exp Exp

instance showExp :: Show Exp where
  show (Lit i) = "Lit: " <> (show i)
  show (Var n) = "Var: " <> n
  show (Plus l r) = "Plus: (" <> (show l) <> " + " <> (show r) <> ")"
  show (Abs n e) = "Abs: " <> (show n) <> ": " <> (show e)
  show (App e1 e2) = "App:" <> (show e1) <> ": " <> (show e2)


derive instance eqExp :: Eq Exp

-- values 
data Value = 
    IntVal Int
  | FunVal Environment Name Exp

instance showValue :: Show Value where 
  show (IntVal i) = "Int: " <> (show i)
  show (FunVal env name exp) = "Func: " <> (show env) <> ": " <> name <> ": " <> (show exp)

derive instance eqValue :: Eq Value

-- mapping from names to values
type Environment
  = Map.Map Name Value

