module Components.App where

import Prelude
import Data.Array (intercalate)
import Data.Maybe (fromMaybe)
import React.Basic.Hooks (fragment, element, CreateComponent, JSX, useState, component, (/\))
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
import Lib.Parser (parse, eval)

mkApp :: CreateComponent {}
mkApp = component "App" \props -> R.do

  equation /\ setEquation <- useState "a | (b=>c) & d"

  pure $ M.muiThemeProvider { theme: theme }
    [ M.grid { container: true, spacing: M.spacing8, justify: M.centerJustify }
      [ M.grid { item: true, sm: M.grids6 }
        [ M.paper { style: { padding: 8 } }
          [ M.typography { variant: M.variantH2 } [ R.text "Hi!" ]
          , M.typography_ [ R.text "Some blah blah thing" ]
          , M.input
            { placeholder: "p & q => r"
            , fullWidth: true
            , onChange: capture targetValue $ \v -> do
                setEquation (\_ -> fromMaybe "" v)
            , value: equation }
          ]
        ]
      , M.grid { item: true, sm: M.grids12 } []
      , M.grid { item: true, sm: M.grids6 }
        [ M.paper { style: { padding: 10 } }
          [ C.resultTable (parse equation)
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
