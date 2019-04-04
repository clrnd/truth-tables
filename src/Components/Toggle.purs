module Components.Toggle where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, targetValue)

import Lib.Parser (parse)

type Props =
  { initialValue :: Boolean
  }

data Action = Toggle

toggle :: Props -> JSX
toggle = make (createComponent "") { initialState, render }
  where
    initialState = ""
    render self =
      R.div_
        [ R.input
            { onChange: capture targetValue $ \v -> do
                self.setState (\_ -> fromMaybe initialState v)
            , value: self.state
            }
        , R.br {}
        , R.text (show $ parse self.state)
        ]
