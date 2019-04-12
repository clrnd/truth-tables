module Components.App where

import Prelude
import Data.Maybe (fromMaybe)
import React.Basic.Hooks (element, CreateComponent, JSX, useState, component, (/\))
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (bind) as R
import React.Basic.DOM as R

import MaterialUI.Button as M
import Lib.Parser (parse, eval)

mkApp :: CreateComponent {}
mkApp = component "App" \props -> R.do

  text /\ setText <- useState ""

  pure $ R.div_
    [ R.h1_ [ R.text "Hi!" ]
    , element M.button { color: "primary"
                       , children: [R.text "Lol"]
                       , variant:  "contained"
                       }
    , R.input
        { onChange: capture targetValue $ \v -> do
            setText (\_ -> fromMaybe "" v)
        , value: text
        }
    , R.br {}
    , R.text (show $ parse text)
    ]
