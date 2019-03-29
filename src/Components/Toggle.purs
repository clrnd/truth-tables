module Components.Toggle where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)

type Props =
  { initialValue :: Boolean
  }

data Action = Toggle

toggle :: Props -> JSX
toggle = make (createComponent "") { initialState, render }
  where
    initialState = Nothing
    render self =
      let on = fromMaybe self.props.initialValue self.state
      in R.button
           { onClick: capture_ do
               self.setState (Just <<< not <<< fromMaybe on)
           , children:
               [ R.text
                   if on
                     then "On"
                     else "Off"
               ]
           }
