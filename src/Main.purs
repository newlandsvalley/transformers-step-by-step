module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import Effect.Class (liftEffect)
import Effect.Aff (Fiber, launchAff)
import Data.Map as Map
import Data (Exp(..))
import Transformers6 (eval6, runEval6)

-- we use the final version (Eval6) which runs in Aff
main :: Effect (Fiber Unit)
main = launchAff $ do
  let 
    initialState = 0
  result <- runEval6 Map.empty initialState (eval6 example)   
  liftEffect $ logShow result
  pure unit

example :: Exp
example = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

