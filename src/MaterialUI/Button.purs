module MaterialUI.Button where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Record as Record

type ButtonProps =
  ( color :: Color
  , disabled :: Boolean
  , disableFocusRipple :: Boolean
  , disableRipple :: Boolean
  , fullWidth :: Boolean
  , href :: String
  , mini :: Boolean
  , size :: Size
  , variant :: Variant
  )


newtype Color = Color String

primary :: Color
primary = Color "primary"

secondary :: Color
secondary = Color "secondary"

default :: Color
default = Color "default"

inherit :: Color
inherit = Color "inherit"


newtype Variant = Variant String

text :: Variant
text = Variant "text"

flat :: Variant
flat = Variant "flat"

outlined :: Variant
outlined = Variant "outlined"

contained :: Variant
contained = Variant "contained"

raised :: Variant
raised = Variant "raised"

fab :: Variant
fab = Variant "fab"

extendedFab :: Variant
extendedFab = Variant "extendedFab"


newtype Size = Size String

small :: Size
small = Size "small"

medium :: Size
medium = Size "medium"

large :: Size
large = Size "large"


foreign import buttonImpl :: forall p. ReactComponent p

button :: forall p. SubRow p ButtonProps => { | p } -> Array JSX -> JSX
button props children = element buttonImpl $ Record.union { children } props

button_ :: Array JSX -> JSX
button_ children  = element buttonImpl { children }
