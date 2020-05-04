module Components.App where

import Prelude
import Data.Array (intercalate)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import React.Basic.Hooks (fragment, element, ReactComponent, JSX, useState, component, (/\))
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (bind) as R
import React.Basic.DOM as R

import Components.ResultTable as C
import MaterialUI.Button as M
import MaterialUI.Input as M
import MaterialUI.Divider as M
import MaterialUI.Grid as M
import MaterialUI.Paper as M
import MaterialUI.Styles as M
import MaterialUI.Typography as M
import Lib.Parser (parse)

mkApp :: Effect (ReactComponent {})
mkApp = component "App" \props -> R.do

  let initialExmple = "a | (b=>c) & d"

  equation1 /\ setEquation1 <- useState "(p | q) & (p | r)"
  equation2 /\ setEquation2 <- useState "p | (q & r)"

  let exp1 = parse equation1
  let exp2 = parse equation2

  pure $ M.muiThemeProvider { theme: theme }
    [ M.grid { container: true, spacing: M.spacing8, justify: M.centerJustify }
      [ M.grid { item: true, sm: M.grids6 }
        [ M.paper { style: R.css { padding: "1em" } }
          [ M.grid { container: true, spacing: M.spacing8 }
            [ M.grid { item: true, sm: M.grids12 }
              [ M.typography { variant: M.variantH2 } [ R.text "Truth Table Comparator" ]
              , M.typography_ [ R.text "This tool generates two truth tables fo you to compare." ]
              ]
            , M.grid { item: true, sm: M.grids6 }
              [ M.typography_ [ R.text "Enter one formula:" ]
              , M.input
                { fullWidth: true
                , onChange: capture targetValue $ \v -> do
                    setEquation1 (\_ -> fromMaybe "" v)
                , value: equation1 }
              , R.p_ []
              , C.resultTable exp1 exp2
              ]
            , M.grid { item: true, sm: M.grids6 }
              [ M.typography_ [ R.text "Enter another formula:" ]
              , M.input
                { fullWidth: true
                , onChange: capture targetValue $ \v -> do
                    setEquation2 (\_ -> fromMaybe "" v)
                , value: equation2 }
              , R.p_ []
              , C.resultTable exp2 exp1
              ]
            ]
          ]
        ]
      ]
    ]

theme :: M.Theme
theme = M.createMuiTheme {
    typography: {
      fontFamily: fonts,
      useNextVariants: true
    }
  }
  where fonts = intercalate "," ["SymbolaRegular", "Helvetica", "Arial", "sans-serif"]
