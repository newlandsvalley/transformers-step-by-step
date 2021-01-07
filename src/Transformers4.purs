module Transformers4
  ( eval4
  , runEval4
  ) where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Tuple (Tuple)
import Prelude
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (class MonadState, get, put)

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

type Eval4 = ReaderT Environment (ExceptT String (StateT Int Identity)) 

runEval4 :: forall a. Environment -> Int -> Eval4 a -> Tuple (Either String a) Int
runEval4 env st ev = 
  let 
    (Identity a) = (runStateT (runExceptT (runReaderT ev env )) st)
  in 
    a

tick :: forall m. (MonadState Int m) => m Unit
tick = do 
  st <- get
  put (st + 1)    

-- count each evaluation step and save to state
eval4 :: Exp -> Eval4 Value
eval4 (Lit i ) = do 
  tick
  pure $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case (Map.lookup n env) of 
    Just v -> pure v 
    Nothing -> throwError ("unbound variable: " <> n)
eval4 (Plus e1 e2 ) = do 
  tick
  env <- ask
  vali1 <- eval4 e1
  vali2 <- eval4 e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        throwError "type error in addition"
eval4 (Abs n e) = do 
  tick
  env <- ask  
  pure $ FunVal env n e
eval4 (App e1 e2 ) = do 
  tick
  env <- ask  
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body ->
        local (const (Map.insert n val2 env' )) (eval4 body)
    IntVal _ -> 
        throwError "type error in application"