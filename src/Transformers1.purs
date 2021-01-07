module Transformers1
  ( eval1
  , runEval1
  ) where

import Data.Identity (Identity(..))
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import Prelude

import Data.Map as Map
import Data (Environment, Exp(..), Value(..))

-- note this and succeeding types are partially applied
-- i.e. they are of kind type -> type
type Eval1 = Identity

runEval1 :: forall a. Eval1 a -> a
runEval1 (Identity a) = a

-- introduce monadic behaviour
eval1 :: Environment -> Exp -> Eval1 Value
eval1 env (Lit i ) = 
  pure $ IntVal i
eval1 env (Var n) = 
  pure $ unsafePartial $ fromJust (Map.lookup n env )
eval1 env (Plus e1 e2 ) = do 
  vali1 <- eval1 env e1
  vali2 <- eval1 env e2
  case vali1, vali2 of 
      IntVal i1, IntVal i2 -> pure $ IntVal (i1 + i2)
      _, _ -> 
        unsafeCrashWith "type error in addition"
eval1 env (Abs n e) = 
  pure $ FunVal env n e
eval1 env (App e1 e2 ) = do 
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body ->
        eval1 (Map.insert n val2 env' ) body
    IntVal _ -> 
        unsafeCrashWith "type error in application"