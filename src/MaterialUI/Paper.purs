module MaterialUI.Paper where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (CSS)
import Record as Record


type PaperProps =
  ( elevation :: Int -- ^ Default: `2`
  , square :: Boolean -- ^ Default: `false`
  , style :: CSS
  )


foreign import paperImpl :: forall p. ReactComponent p

paper :: forall p. SubRow p PaperProps => { | p } -> Array JSX -> JSX
paper props children = element paperImpl $ Record.union { children } props

paper_ :: Array JSX -> JSX
paper_ children  = element paperImpl { children }
