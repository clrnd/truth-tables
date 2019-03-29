module Lib.Parser where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either(..))
import Text.Parsing.Parser (Parser, runParser, parseErrorMessage)
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

data Expr = Var Char
          | Or Expr Expr
          | And Expr Expr

instance showExpr :: Show Expr where
  show (Var a) = "Var " <> show a
  show (Or e1 e2) = show e1 <> " | " <> show e2
  show (And e1 e2) = show e1 <> " & " <> show e2

instance eqExpr :: Eq Expr where
  eq (Var s) (Var s') = s == s'
  eq (Or e1 e2) (Or e1' e2') = e1 == e1' && e2 == e2'
  eq (And e1 e2) (And e1' e2') = e1 == e1' && e2 == e2'
  eq _ _ = false

parseVar :: Parser String Expr
parseVar = Var <$> letter <* skipSpaces

parseOperator :: Parser String (Expr -> Expr -> Expr)
parseOperator = (char '|' *> pure Or <* skipSpaces)
            <|> (char '&' *> pure And <* skipSpaces)

parser :: Parser String Expr
parser = chainl1 parseVar parseOperator

parse :: String -> Either String Expr
parse input = case runParser input parser of
  Left e -> Left $ parseErrorMessage e
  Right v -> Right v
