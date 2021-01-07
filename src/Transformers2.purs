module Transformers2
  ( eval2
  , runEval2
  ) where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

type Eval2  = ExceptT String Identity 
-- or equivalently we could use Control.Monad.Except and say:
-- type Eval2 = Except String


runEval2 :: forall a. Eval2 a -> Either String a
runEval2 ev = 
  let 
    (Identity a) = runExceptT ev 
  in 
    a

-- add error handling
eval2 :: Environment -> Exp -> Eval2 Value
eval2 env (Lit i ) = 
  pure $ IntVal i
eval2 env (Var n) = 
  case (Map.lookup n env) of 
    Just v -> pure v 
    Nothing -> throwError ("unbound variable: " <> n)
eval2 env (Plus e1 e2 ) = do 
  vali1 <- eval2 env e1
  vali2 <- eval2 env e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        throwError "type error in addition"
eval2 env (Abs n e) = 
  pure $ FunVal env n e
eval2 env (App e1 e2 ) = do 
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body ->
        eval2 (Map.insert n val2 env' ) body
    IntVal _ -> 
        throwError "type error in application"