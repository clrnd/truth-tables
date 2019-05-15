module Lib.Table where

import Prelude

import Data.Array (replicate, zip, length, sort)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Data.Unit

import Lib.Parser


data TruthTable = TruthTable (Array Header) (Array Row)
derive instance genericTruthTable :: Generic TruthTable _
instance showTruthTable :: Show TruthTable where
  show = genericShow
instance eqTruthTable :: Eq TruthTable where
  eq = genericEq

data Header = Header String
derive instance genericHeader :: Generic Header _
instance showHeader :: Show Header where
  show = genericShow
instance eqHeader :: Eq Header where
  eq = genericEq
instance ordHeader :: Ord Header where
  compare = genericCompare

data Row = Row (Array Boolean) Boolean
derive instance genericRow :: Generic Row _
instance showRow :: Show Row where
  show = genericShow
instance eqRow :: Eq Row where
  eq = genericEq

tableFor :: Expr -> TruthTable
tableFor e = TruthTable (headerFor e) (rowsFor e)

headerFor :: Expr -> Array Header
headerFor = map Header <<< extractVarsSorted

extractVarsSorted :: Expr -> Array String
extractVarsSorted = sort <<< extractVars
  where
    extractVars (Var s) = [s]
    extractVars (e1 :&& e2) = extractVars e1 <> extractVars e2
    extractVars (e1 :|| e2) = extractVars e1 <> extractVars e2
    extractVars (e1 :=> e2) = extractVars e1 <> extractVars e2


rowsFor :: Expr -> Array Row
rowsFor e = map toRow $ combs [false, true]
  where
    combs l = traverse (\_ -> l) $ replicate size unit
    toRow l = Row l (eval (env l) e)
    env l = fromFoldable (zip names l)
    names = extractVarsSorted e
    size = length names
