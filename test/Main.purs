module Test.Main where

import Prelude (Unit, discard)
import Effect (Effect)
import Control.Monad.Free (Free)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

import Test.Unit.Main (runTest)
import Data.Map as Map
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Transformers0 (eval0)
import Transformers1 (eval1, runEval1)
import Transformers2 (eval2, runEval2)
import Transformers3 (eval3, runEval3)
import Transformers4 (eval4, runEval4)
import Data



main :: Effect  Unit
main = runTest do
  suite "basic evaluation" do
    basicSuite

basicSuite :: Free TestF Unit
basicSuite =
  suite "evaluation" do
    test "eval0" do
      Assert.equal 
        (eval0 Map.empty example) (IntVal 18)
    test "eval1" do
      Assert.equal 
        (runEval1 (eval1 Map.empty example)) (IntVal 18)
    test "eval2 success" do
      Assert.equal 
        (runEval2 (eval2 Map.empty example)) (Right (IntVal 18))
    
    test "eval2 type failure" do
      Assert.equal
        (runEval2 (eval2 Map.empty typeErrorExample)) (Left "type error in addition")

    test "eval2 lookup failure" do
      Assert.equal
        (runEval2 (eval2 Map.empty unboundVariableExample)) (Left "unbound variable: x")
     
    test "eval3 success" do
      Assert.equal 
        (runEval3 Map.empty (eval3 example)) (Right (IntVal 18)) 

    test "eval4 success" do
      let 
        initialState = 0
        Tuple result count = runEval4 Map.empty initialState (eval4 example)
      Assert.equal result (Right (IntVal 18)) 
      Assert.equal count 8



example :: Exp
example = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

typeErrorExample :: Exp 
typeErrorExample = Plus (Lit 1) (Abs "x" (Var "x"))

unboundVariableExample :: Exp
unboundVariableExample = Var "x"