module Transformers0
  ( eval0
  ) where

import Prelude

import Data (Environment, Exp(..), Value(..))
import Data.Map as Map
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)


-- reference evaluation function
eval0 :: Environment → Exp → Value
eval0 env (Lit i ) = IntVal i
eval0 env (Var n) = unsafePartial $ fromJust (Map.lookup n env )
eval0 env (Plus e1 e2 ) = 
  let vali1 = eval0 env e1
      vali2 = eval0 env e2
  in 
    case vali1, vali2 of 
      IntVal i1, IntVal i2 -> IntVal (i1 + i2)
      _, _ -> 
        unsafeCrashWith "type error in addition"
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2 ) = 
  let val1 = eval0 env e1
      val2 = eval0 env e2
  in case val1 of
    FunVal env' n body -> eval0 (Map.insert n val2 env' ) body
    IntVal _ -> 
        unsafeCrashWith "type error in application"

