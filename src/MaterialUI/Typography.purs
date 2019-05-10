module MaterialUI.Typography where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Record as Record

type TypographyProps =
  ( align :: Alignment
  , color :: Color
  --, component :: ReactComponent {}
  , gutterBottom :: Boolean
  , noWrap :: Boolean
  , paragraph :: Boolean
  , variant :: Variant
  )


newtype Alignment = Alignment String

inheritAlign :: Alignment
inheritAlign = Alignment "inherit"

left :: Alignment
left = Alignment "left"

right :: Alignment
right = Alignment "right"

center :: Alignment
center = Alignment "center"

justify :: Alignment
justify = Alignment "justify"


newtype Color = Color String

inheritColor :: Color
inheritColor = Color "inherit"

primary :: Color
primary = Color "primary"

secondary :: Color
secondary = Color "secondary"

textSecondary :: Color
textSecondary = Color "textSecondary"

error :: Color
error = Color "error"

default :: Color
default = Color "default"

newtype Variant = Variant String

variantButton :: Variant
variantButton = Variant "button"

variantH1 :: Variant
variantH1 = Variant "h1"

variantH2 :: Variant
variantH2 = Variant "h2"

variantH3 :: Variant
variantH3 = Variant "h3"

variantH4 :: Variant
variantH4 = Variant "h4"


foreign import typographyImpl :: forall p. ReactComponent p

typography :: forall p. SubRow p TypographyProps => { | p } -> Array JSX -> JSX
typography props children = element typographyImpl $ Record.union { children } props

typography_ :: Array JSX -> JSX
typography_ children = element typographyImpl { children }
