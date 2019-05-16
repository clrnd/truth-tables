module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run, runSpec)

import Lib.Parser
import Lib.Table


main :: Effect Unit
main = run [consoleReporter] do
  describe "Parsing" do

    describe "simple expressions like" do
      it "q works" do
         parse "q" `shouldEqual` Right (Var "q")
      it "& doesn't" do
         parse "&" `shouldEqual` Left "Expected letter or digit"
      it "q | j is or" do
         parse "q | j" `shouldEqual` Right (Var "q" :|| Var "j")
      it "q & j is and" do
         parse "q & j" `shouldEqual` Right (Var "q" :&& Var "j")
      it "q => j is imply" do
         parse "q => j" `shouldEqual` Right (Var "q" :=> Var "j")

    describe "associativity like" do
      it "q | j & r is q | (j & r)" do
         parse "q | j & r" `shouldEqual` Right (Var "q" :|| (Var "j" :&& Var "r"))
      it "q | j => r is (q | j) => r" do
         parse "q | j => r" `shouldEqual` Right ((Var "q" :|| Var "j") :=> Var "r")
      it "q | j => r & p is (q | j) => (r & p)" do
         parse "q | j => r & p" `shouldEqual` Right ((Var "q" :|| Var "j") :=> (Var "r" :&& Var "p"))

    describe "parentheses like" do
      it "(p | q) & r" do
         parse "(p | q) & r" `shouldEqual` Right ((Var "p" :|| Var "q") :&& Var "r")
      it "(p & q) | r" do
         parse "(p & q) | r" `shouldEqual` Right ((Var "p" :&& Var "q") :|| Var "r")
      it "(p => q) & (r | j)" do
         parse "(p => q) & (r | j)" `shouldEqual` Right ((Var "p" :=> Var "q") :&& (Var "r" :|| Var "j"))

  describe "Eval" do

    let m = fromFoldable [Tuple "p" true, Tuple "q" false, Tuple "r" true]

    describe "simple expressions like" do
      it "p & q" do
       eval m (Var "p" :&& Var "q") `shouldEqual` false
      it "p & r" do
       eval m (Var "p" :&& Var "r") `shouldEqual` true
      it "q | q" do
       eval m (Var "q" :|| Var "q") `shouldEqual` false
      it "p | r" do
       eval m (Var "p" :|| Var "q") `shouldEqual` true

    describe "complex expressions like" do
      it "(p => q) & (r | p)" do
        eval m ((Var "p" :=> Var "q") :&& (Var "r" :|| Var "p")) `shouldEqual` false
      it "(r => p) & (r | q)" do
        eval m ((Var "r" :=> Var "p") :&& (Var "r" :|| Var "q")) `shouldEqual` true

  describe "Table generator" do

    describe "header works" do
      it "a" do
        headerFor (Var "a") `shouldEqual` [Header "a"]
      it "a & b" do
        headerFor (Var "a" :&& Var "b") `shouldEqual` [Header "a", Header "b"]
      it "unordered b & a" do
        headerFor (Var "b" :&& Var "a") `shouldEqual` [Header "a", Header "b"]

    describe "rows work" do
      it "a" do
        rowsFor (Var "a") `shouldEqual` [ Row [false] false
                                        , Row [true] true ]
      it "a & b" do
        rowsFor (Var "a" :&& Var "b") `shouldEqual` [ Row [false, false] false
                                                    , Row [false, true] false
                                                    , Row [true, false] false
                                                    , Row [true, true] true ]
      it "a & b || c" do
        rowsFor (Var "a" :&& Var "b" :|| Var "c")
                `shouldEqual`
                [ Row [false, false, false] false
                , Row [false, false, true] true
                , Row [false, true, false] false
                , Row [false, true, true] true
                , Row [true, false, false] false
                , Row [true, false, true] true
                , Row [true, true, false] true
                , Row [true, true, true] true ]

    describe "full example" do
      it "a | (b => c) & d" do
        tableFor (Var "a" :|| ((Var "b" :=> Var "c") :&& Var "d"))
                 `shouldEqual`
                 TruthTable
                   [ Header "a", Header "b", Header "c", Header "d" ]
                   [ Row [false, false, false, false] false
                   , Row [false, false, false, true] true
                   , Row [false, false, true, false] false
                   , Row [false, false, true, true] true
                   , Row [false, true, false, false] false
                   , Row [false, true, false, true] false
                   , Row [false, true, true, false] false
                   , Row [false, true, true, true] true
                   , Row [true, false, false, false] true
                   , Row [true, false, false, true] true
                   , Row [true, false, true, false] true
                   , Row [true, false, true, true] true
                   , Row [true, true, false, false] true
                   , Row [true, true, false, true] true
                   , Row [true, true, true, false] true
                   , Row [true, true, true, true] true ]
