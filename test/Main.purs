module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run, runSpec)

import Lib.Parser

main :: Effect Unit
main = run [consoleReporter] do
  describe "Parsing" do
    describe "identifiers" do
      it "q works" do
         parse "q" `shouldEqual` Right (Var 'q')
      it "& doesn't" do
         parse "&" `shouldEqual` Left "Expected letter"
    describe "disjunction" do
      it "q | j" do
         parse "q | j" `shouldEqual` Right (Or (Var 'q') (Var 'j'))
    describe "conjunction" do
      it "q & j" do
         parse "q & j" `shouldEqual` Right (And (Var 'q') (Var 'j'))
