module Components.ResultTable where

import Prelude
import Data.Either (Either(..))
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R

import MaterialUI.Typography as M
import MaterialUI.Table as M
import Lib.Parser
import Lib.Table


resultTable :: Either String Expr -> JSX
resultTable (Left error) = M.typography_ [ R.text error ]
resultTable (Right result) = render $ tableFor result


render :: TruthTable -> JSX
render (TruthTable header rows) =
  M.table_
    [ M.tableHead_
      [ M.tableRow_ (map toHeader header) ]
    , M.tableBody_ (map toRow rows)
    ]
  where
    toHeader (Header s) = M.tableCell_ [ R.text s ]
    toCell b = M.tableCell_ [ R.text $ show b ]
    toRow (Row vals res) = M.tableRow_ $ map toCell vals <> [ toCell res ]
