module Components.ResultTable where

import Prelude
import Data.Either (Either(..))
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R

import MaterialUI.Typography as M
import MaterialUI.Table as M
import MaterialUI.Types as M
import Lib.Parser
import Lib.Table


resultTable :: Either String Expr -> JSX
resultTable (Left error) = M.typography_ [ R.text error ]
resultTable (Right result) = render (prettyExpr result) (tableFor result)

render :: String -> TruthTable -> JSX
render expr (TruthTable header rows) =
  M.table { style: { width: "auto", margin: "auto" } }
    [ M.tableHead_ [ M.tableRow_ (map toHeader header <> toResultHeader expr) ]
    , M.tableBody_ (map toRow rows)
    ]
  where
    bigger s = M.typography { variant: M.variantTitle } [ R.text s ]
    toResultHeader s = [ cell [ bigger s ] ]
    toHeader (Header s) = cell [ bigger s ]
    toCell b = cell [ M.typography_ [ R.text $ prettyBool b ] ]
    cell = M.tableCell { align: M.center, style: { padding: 10 } }
    toRow (Row vals res) = M.tableRow_ $ map toCell vals <> [ toCell res ]

prettyBool :: Boolean -> String
prettyBool true = "T"
prettyBool false = "F"

prettyExpr :: Expr -> String
prettyExpr (Var a) = a
prettyExpr (Or e1 e2) = "(" <> prettyExpr e1 <> " ∨ " <> prettyExpr e2 <> ")"
prettyExpr (And e1 e2) = "(" <> prettyExpr e1 <> " ∧ " <> prettyExpr e2 <> ")"
prettyExpr (Imp e1 e2) = "(" <> prettyExpr e1 <> " ⇒ " <> prettyExpr e2 <> ")"
