module Components.App where

import Prelude
import Data.Array (intercalate)
import Data.Maybe (fromMaybe)
import React.Basic.Hooks (fragment, element, CreateComponent, JSX, useState, component, (/\))
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (bind) as R
import React.Basic.DOM as R

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

  equation /\ setEquation <- useState ""

  pure $ M.muiThemeProvider { theme: theme }
    [ M.grid { container: true, spacing: M.spacing16, justify: M.centerJustify }
      [ M.grid { item: true, sm: M.grids6 }
        [ M.paper_
          [ M.typography_ [ R.text "Hi!" ]
          , M.typography_ [ R.text "Blha lbahbaslhdab d asldblasbd l asbdldabs" ]
          , M.divider_
          , M.input
            { defaultValue: "p & q => r"
            , fullWidth: true
            , onChange: capture targetValue $ \v -> do
                setEquation (\_ -> fromMaybe "" v)
            , value: equation }
          , R.text (show $ parse equation)
          ]
        ]
      ]
    ]

theme :: M.Theme
theme = M.createMuiTheme {
    typography: {
        fontFamily: fonts
        }
    }
  where fonts = intercalate "," ["SymbolaRegular", "Helvetica", "Arial", "sans-serif"]
