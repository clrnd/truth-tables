module Lib.Table where

import Prelude

import Data.Maybe
import Data.Array (nub, replicate, zip, length, sort)
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

data Comp = Equal | Distinct
derive instance genericComp :: Generic Comp _
instance showComp :: Show Comp where
  show = genericShow
instance eqComp :: Eq Comp where
  eq = genericEq

data Row = Row (Array Boolean) Boolean (Maybe Comp)
derive instance genericRow :: Generic Row _
instance showRow :: Show Row where
  show = genericShow
instance eqRow :: Eq Row where
  eq = genericEq

tableFor :: Expr -> Maybe Expr -> TruthTable
tableFor e me = TruthTable (headerFor e) (rowsFor e me)

headerFor :: Expr -> Array Header
headerFor = map Header <<< extractVarsSorted

extractVarsSorted :: Expr -> Array String
extractVarsSorted = nub <<< sort <<< extractVars
  where
    extractVars (Var s) = [s]
    extractVars (e1 :&& e2) = extractVars e1 <> extractVars e2
    extractVars (e1 :|| e2) = extractVars e1 <> extractVars e2
    extractVars (e1 :=> e2) = extractVars e1 <> extractVars e2
    extractVars (Not e) = extractVars e


rowsFor :: Expr -> Maybe Expr -> Array Row
rowsFor e me = map toRow $ combs [false, true]
  where
    combs l = traverse (\_ -> l) $ replicate size unit
    toRow l = let env' = env l
                  evalLeft = eval env' e
              in Row l evalLeft (comp <<< (==) evalLeft <<< eval env' <$> (me >>= filter))
    env l = fromFoldable (zip names l)
    names = extractVarsSorted e
    size = length names
    comp true = Equal
    comp false = Distinct
    filter exp = case extractVarsSorted exp == names of
                    true -> Just exp
                    false -> Nothing
