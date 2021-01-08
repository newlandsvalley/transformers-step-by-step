module Transformers6
  ( eval6
  , runEval6
  ) where

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
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

type Eval6 = ReaderT Environment (ExceptT String
        (WriterT (Array String) (StateT Int Aff))) 


runEval6 :: forall a. 
   Environment -> 
   Int -> 
   Eval6 a -> 
   Aff (Tuple (Tuple (Either String a) (Array String)) Int)
runEval6 env st ev = 
    runStateT (runWriterT (runExceptT (runReaderT ev env ))) st

tick :: forall m. (MonadState Int m) => m Unit
tick = do 
  st <- get
  put (st + 1)    

-- Now run in Aff (instead of Identity)
-- 
-- we can also use the more specific signature (below) 
-- eval6 :: Exp -> Eval6 Value
eval6 :: forall m. 
  MonadThrow String m => 
  MonadReader Environment m => 
  MonadState Int m =>
  MonadTell (Array String) m =>
  MonadAff m =>
  Exp -> 
  m Value
eval6 (Lit i ) = do 
  tick
  pure $ IntVal i
eval6 (Var n) = do
  tick
  tell (singleton n)
  env <- ask
  case (Map.lookup n env) of 
    Just v -> pure v 
    Nothing -> throwError ("unbound variable: " <> n)
eval6 (Plus e1 e2 ) = do 
  tick
  env <- ask
  vali1 <- eval6 e1
  vali2 <- eval6 e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        throwError "type error in addition"
eval6 (Abs n e) = do 
  tick
  env <- ask  
  pure $ FunVal env n e
eval6 (App e1 e2 ) = do 
  tick
  env <- ask  
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body ->
        local (const (Map.insert n val2 env' )) (eval6 body)
    IntVal _ -> 
        throwError "type error in application"