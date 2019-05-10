module Components.ResultTable where

import Prelude
import Data.Either (Either(..))
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM as R

import MaterialUI.Typography as M
import MaterialUI.Table as M
import Lib.Parser


resultTable :: Either String Expr -> JSX
resultTable (Left error) = M.typography_ [ R.text error ]
resultTable (Right result) =
  M.table_
    [ M.tableHead_
      [ M.tableRow_
        [ M.tableCell_ [ R.text "lala" ]
        ]
      ]
    ]
