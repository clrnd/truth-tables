module MaterialUI.Button where

import Prelude
import Prelude
import React.Basic

type ButtonProps =
  { color :: String
  , variant :: String
  , children :: Array JSX
  }

foreign import button :: ReactComponent ButtonProps
