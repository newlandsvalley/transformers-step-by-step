module Transformers3
  ( eval3
  , runEval3
  ) where

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Either (Either)
import Prelude
import Control.Monad.Except.Trans (class MonadThrow, ExceptT, runExceptT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (ReaderT, ask, local, runReaderT)
import Control.Monad.Reader.Class (class MonadReader)

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

type Eval3 = ReaderT Environment (ExceptT String Identity)


runEval3 :: forall a. 
  Environment -> 
  Eval3 a -> 
  Either String a
runEval3 env ev = 
  let 
    (Identity a) = runExceptT (runReaderT ev env) 
  in 
    a

-- get the environment from the Reader
--
-- We can either continue defining the signature like this:
--
-- eval3 :: Exp -> Eval3 Value 
--
-- which is specific to the context in which we're using it here or we can
-- use a more generalised signature which would allow it to be used in any
-- monadic context. 
eval3 :: forall m. 
  MonadThrow String m => 
  MonadReader Environment m => 
  Exp -> 
  m Value
eval3 (Lit i ) = 
  pure $ IntVal i
eval3 (Var n) = do
  env <- ask
  case (Map.lookup n env) of 
    Just v -> pure v 
    Nothing -> throwError ("unbound variable: " <> n)
eval3 (Plus e1 e2 ) = do 
  env <- ask
  vali1 <- eval3 e1
  vali2 <- eval3 e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        throwError "type error in addition"
eval3 (Abs n e) = do 
  env <- ask  
  pure $ FunVal env n e
eval3 (App e1 e2 ) = do 
  env <- ask  
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body ->
        -- note local requires an (env -> env) function in order to pass the environment 
        -- down to sub-computations.  In this case, we don't need to change it after adding 
        -- the new value so we just use a const function.

        -- note too that this means the environment can change for sub-computations, but 
        -- it's not generally available in other branches (as it will be for StateT)
        local (const (Map.insert n val2 env' )) (eval3 body)
    IntVal _ -> 
        throwError "type error in application"