module Lib.Parser where

import Prelude

import Data.Map
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Array (some)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

data Expr = Var String
          | Or Expr Expr
          | And Expr Expr
          | Imp Expr Expr

infixl 0 Imp as :=>
infixl 0 Or as :||
infixl 0 And as :&&

instance showExpr :: Show Expr where
  show (Var a) = "Var " <> show a
  show (Or e1 e2) = "(" <> show e1 <> " | " <> show e2 <> ")"
  show (And e1 e2) = "(" <> show e1 <> " & " <> show e2 <> ")"
  show (Imp e1 e2) = "(" <> show e1 <> " => " <> show e2 <> ")"

instance eqExpr :: Eq Expr where
  eq (Var s) (Var s') = s == s'
  eq (Or e1 e2) (Or e1' e2') = e1 == e1' && e2 == e2'
  eq (And e1 e2) (And e1' e2') = e1 == e1' && e2 == e2'
  eq (Imp e1 e2) (Imp e1' e2') = e1 == e1' && e2 == e2'
  eq _ _ = false

parser :: Parser String Expr
parser = fix (\p -> parser' p) <* skipSpaces <* eof

parser' :: Parser String Expr -> Parser String Expr
parser' p = imps
  where
    imps = ors `chainl1` (operator "=>" Imp)
    ors = ands `chainl1` (operator "|" Or)
    ands = (bracketed <|> vars) `chainl1` (operator "&" And)
    vars = Var <<< fromCharArray <$> some alphaNum <* skipSpaces
    bracketed = char '(' *> p <* char ')' <* skipSpaces
    operator s op = string s *> pure op <* skipSpaces

parse :: String -> Either String Expr
parse input = case runParser input parser of
  Left e -> Left $ parseErrorMessage e
  Right v -> Right v

eval :: Map String Boolean -> Expr -> Boolean
eval m (Var v) = unsafePartial $ fromJust $ lookup v m
eval m (p :&& q) = eval m p && eval m q
eval m (p :|| q) = eval m p || eval m q
eval m (p :=> q) = if (eval m p && not eval m q) then false else true
