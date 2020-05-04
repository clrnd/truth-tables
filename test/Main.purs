module Test.Main where

import Prelude (Unit, discard, ($))

import Data.Maybe
import Data.Either (isLeft, Either(..))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Lib.Parser (Expr(..), eval, parse, (:&&), (:=>), (:||))
import Lib.Table (Header(..), Row(..), TruthTable(..), headerFor, rowsFor, tableFor, Comp(..))

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Parsing" do

    describe "simple expressions" do

      it "a single letter is a var" do
         parse "q" `shouldEqual` Right (Var "q")

      it "a string of letters is a var" do
         parse "qar" `shouldEqual` Right (Var "qar")

      it "a standalone operator fails" do
         parse "&" `shouldSatisfy` isLeft

      it "an infix | is OR" do
         parse "q | j" `shouldEqual` Right (Var "q" :|| Var "j")

      it "an infix & is AND" do
         parse "q & j" `shouldEqual` Right (Var "q" :&& Var "j")

      it "an infix => is implies" do
         parse "q => j" `shouldEqual` Right (Var "q" :=> Var "j")

      it "a prefix ~ is not" do
         parse "~q" `shouldEqual` Right (Not (Var "q"))


    describe "associativity" do

      it "NOT is stronger than AND" do
         parse "~q & p" `shouldEqual` Right (Not (Var "q") :&& Var "p")

      it "AND is stronger than OR" do
         parse "q | j & r" `shouldEqual` Right (Var "q" :|| (Var "j" :&& Var "r"))

      it "OR is stronger than IMPLIES" do
         parse "q | j => r" `shouldEqual` Right ((Var "q" :|| Var "j") :=> Var "r")

      it "a mixed expression is parsed correctly" do
         parse "q | j => r & p" `shouldEqual` Right ((Var "q" :|| Var "j") :=> (Var "r" :&& Var "p"))

    describe "parentheses" do

      it "OR in parentheses is stronger than AND" do
         parse "(p | q) & r" `shouldEqual` Right ((Var "p" :|| Var "q") :&& Var "r")

      it "AND in parentheses is stronger than OR" do
         parse "(p & q) | r" `shouldEqual` Right ((Var "p" :&& Var "q") :|| Var "r")

      it "NOT negates the whole parentheses" do
         parse "~(p | q) & r" `shouldEqual` Right (Not (Var "p" :|| Var "q") :&& Var "r")

      it "mixed expression with parentheses parses correctly" do
         parse "(p => q) & (r | j)" `shouldEqual` Right ((Var "p" :=> Var "q") :&& (Var "r" :|| Var "j"))

  describe "Eval" do

    let m = fromFoldable [Tuple "p" true, Tuple "q" false, Tuple "r" true]

    describe "simple expressions" do

      it "AND with one false value as false" do
       eval m (Var "p" :&& Var "q") `shouldEqual` false

      it "AND with two true values is true" do
       eval m (Var "p" :&& Var "r") `shouldEqual` true

      it "OR with two false values is false" do
       eval m (Var "q" :|| Var "q") `shouldEqual` false

      it "OR with one true value is true" do
       eval m (Var "p" :|| Var "q") `shouldEqual` true

      it "false IMPLIES true disregard the second term" do
       eval m (Var "q" :=> Var "p") `shouldEqual` true
       eval m (Var "q" :=> Var "q") `shouldEqual` true

      it "true IMPLIES true is true" do
       eval m (Var "p" :=> Var "r") `shouldEqual` true

      it "true IMPLIES false is false" do
       eval m (Var "p" :=> Var "q") `shouldEqual` false

      it "respects evaluation order of nested expresions" do
        eval m ((Var "p" :=> Var "q") :&& (Var "r" :|| Var "p")) `shouldEqual` false
        eval m ((Var "r" :=> Var "p") :&& (Var "r" :|| Var "q")) `shouldEqual` true

  describe "Table generator" do

    describe "builds the header" do

      it "one variable generates a single column with it" do
        headerFor (Var "a") `shouldEqual` [Header "a"]

      it "two variables generates two columns with them" do
        headerFor (Var "a" :&& Var "b") `shouldEqual` [Header "a", Header "b"]

      it "two variables generates two columns with them ordered alphabetically" do
        headerFor (Var "b" :&& Var "a") `shouldEqual` [Header "a", Header "b"]

      it "repeated variables don't generate repeated columns" do
        headerFor (Var "a" :&& Var "a") `shouldEqual` [Header "a"]

    describe "builds the rows" do

      it "a single variable is two rows, false and true" do
        rowsFor (Var "a") Nothing
            `shouldEqual`
            [ Row [false] false Nothing
            , Row [true] true Nothing ]

      it "two variables is 4 rows with the cominations, and evaluation" do
        rowsFor (Var "a" :&& Var "b") Nothing
            `shouldEqual`
            [ Row [false, false] false Nothing
            , Row [false, true] false Nothing
            , Row [true, false] false Nothing
            , Row [true, true] true Nothing ]

      it "repeated vars aren't duplicated" do
        rowsFor (Var "a" :&& Var "a") Nothing
            `shouldEqual`
            [ Row [false] false Nothing
            , Row [true] true Nothing ]

    describe "a table combines both header and rows" do
      it "complicated exaple" do
        tableFor (Var "a" :|| ((Var "b" :=> Var "c") :&& Var "d")) Nothing
            `shouldEqual`
            TruthTable
              [ Header "a", Header "b", Header "c", Header "d" ]
              [ Row [false, false, false, false] false Nothing
              , Row [false, false, false, true] true Nothing
              , Row [false, false, true, false] false Nothing
              , Row [false, false, true, true] true Nothing
              , Row [false, true, false, false] false Nothing
              , Row [false, true, false, true] false Nothing
              , Row [false, true, true, false] false Nothing
              , Row [false, true, true, true] true Nothing
              , Row [true, false, false, false] true Nothing
              , Row [true, false, false, true] true Nothing
              , Row [true, false, true, false] true Nothing
              , Row [true, false, true, true] true Nothing
              , Row [true, true, false, false] true Nothing
              , Row [true, true, false, true] true Nothing
              , Row [true, true, true, false] true Nothing
              , Row [true, true, true, true] true Nothing ]

    describe "extracts comparissions" do

      it "a single value is equal to itself" do
        rowsFor (Var "a") (Just (Var "a"))
            `shouldEqual`
            [ Row [false] false (Just Equal)
            , Row [true] true (Just Equal) ]

      it "an expression with different vars doesn't show anything" do
        rowsFor (Var "a") (Just (Var "b"))
            `shouldEqual`
            [ Row [false] false Nothing
            , Row [true] true Nothing ]
