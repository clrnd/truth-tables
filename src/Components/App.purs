module Components.App where

import Prelude
import Data.Array (intercalate)
import Data.Maybe (fromMaybe)
import React.Basic.Hooks (fragment, element, CreateComponent, JSX, useState, component, (/\))
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (bind) as R
import React.Basic.DOM as R

import MaterialUI.Button as M
import MaterialUI.Grid as M
import MaterialUI.Paper as M
import MaterialUI.Styles as M
import Lib.Parser (parse, eval)

mkApp :: CreateComponent {}
mkApp = component "App" \props -> R.do

  text /\ setText <- useState ""

  pure $ M.muiThemeProvider { theme: theme }
    [ M.grid { container: true, spacing: M.spacing16, justify: M.centerJustify }
    [ M.grid { item: true, sm: M.grids12, alignItems: M.centerItems }
        [ R.text "Hi!"
        , M.button { color: M.primary
                   , variant: M.contained }
          [ R.text "Lol" ]
        , R.input
          { onChange: capture targetValue $ \v -> do
              setText (\_ -> fromMaybe "" v)
          , value: text }
        , R.br {}
        , R.text (show $ parse text)
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
