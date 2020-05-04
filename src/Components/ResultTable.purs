module Components.ResultTable where

import Prelude
import Data.Maybe
import Data.Either (Either(..))
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R

import MaterialUI.Typography as M
import MaterialUI.Table as M
import MaterialUI.Types as M
import Lib.Parser
import Lib.Table


resultTable :: Either String Expr -> Either String Expr -> JSX
resultTable eqThis eqOther = case eqThis of
    Left error -> M.typography_ [ R.text error ]
    Right result -> render (prettyExpr result) (tableFor result mEqOther)
  where
      mEqOther = case eqOther of
                    Left _ -> Nothing
                    Right e -> Just e

render :: String -> TruthTable -> JSX
render expr (TruthTable header rows) = R.div
  { style: R.css { padding: "6px 28px 20px 28px"
                 , width: "fit-content"
                 , margin: "auto"
                 , backgroundColor: "#f5f5f5" }
  , children:
      [ M.table { style: R.css { width: "auto" } }
        [ M.tableHead_ [ M.tableRow_ (map toHeader header <> toResultHeader expr) ]
        , M.tableBody_ (map toRow rows)
        ]
      ]
  }
  where
    bigger s = M.typography { variant: M.variantTitle } [ R.text s ]
    toResultHeader s = [ cell [ bigger s ] ]
    toHeader (Header s) = cell [ bigger s ]
    toCell b = cell [ M.typography_ [ R.text $ prettyBool b ] ]
    toResultCell b comp = [ cell
                            [ M.typography (styleForResult comp)
                              [ R.text $ prettyBool b ] ] ]
    styleForResult Nothing = { color: M.default }
    styleForResult (Just Equal) = { color: M.primary }
    styleForResult (Just Distinct) = { color: M.error }
    cell = M.tableCell { align: M.center, style: R.css { padding: 8 } }
    toRow (Row vals res comp) = M.tableRow { style: R.css { height: 5 } }
                           $ map toCell vals <> toResultCell res comp

prettyBool :: Boolean -> String
prettyBool true = "T"
prettyBool false = "F"

prettyExpr :: Expr -> String
prettyExpr (Var a) = a
prettyExpr (Or e1 e2) = "(" <> prettyExpr e1 <> " ∨ " <> prettyExpr e2 <> ")"
prettyExpr (And e1 e2) = "(" <> prettyExpr e1 <> " ∧ " <> prettyExpr e2 <> ")"
prettyExpr (Imp e1 e2) = "(" <> prettyExpr e1 <> " ⇒ " <> prettyExpr e2 <> ")"
prettyExpr (Not e) = "~" <> prettyExpr e
