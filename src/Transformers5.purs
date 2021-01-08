module Transformers5
  ( eval5
  , runEval5
  ) where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Data.Tuple (Tuple)
import Data.Array (singleton)
import Prelude
import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.Reader.Class (class MonadReader)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)
import Control.Monad.Writer.Class (class MonadTell)

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

type Eval5 = ReaderT Environment (ExceptT String
        (WriterT (Array String) (StateT Int Identity))) 


runEval5 :: forall a. 
   Environment -> 
   Int -> 
   Eval5 a -> 
   Tuple (Tuple (Either String a) (Array String)) Int
runEval5 env st ev = 
  let 
    (Identity a) = (runStateT (runWriterT (runExceptT (runReaderT ev env ))) st)
  in 
    a

tick :: forall m. (MonadState Int m) => m Unit
tick = do 
  st <- get
  put (st + 1)    

-- log new variable definitions via Writer
-- 
-- we can also use the more specific signature (below) 
-- eval5 :: Exp -> Eval5 Value
eval5 :: forall m. 
  MonadThrow String m => 
  MonadReader Environment m => 
  MonadState Int m =>
  MonadTell (Array String) m =>
  Exp -> 
  m Value
eval5 (Lit i ) = do 
  tick
  pure $ IntVal i
eval5 (Var n) = do
  tick
  tell (singleton n)
  env <- ask
  case (Map.lookup n env) of 
    Just v -> pure v 
    Nothing -> throwError ("unbound variable: " <> n)
eval5 (Plus e1 e2 ) = do 
  tick
  env <- ask
  vali1 <- eval5 e1
  vali2 <- eval5 e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        throwError "type error in addition"
eval5 (Abs n e) = do 
  tick
  env <- ask  
  pure $ FunVal env n e
eval5 (App e1 e2 ) = do 
  tick
  env <- ask  
  val1 <- eval5 e1
  val2 <- eval5 e2
  case val1 of
    FunVal env' n body ->
        local (const (Map.insert n val2 env' )) (eval5 body)
    IntVal _ -> 
        throwError "type error in application"