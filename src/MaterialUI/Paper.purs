module MaterialUI.Paper where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Record as Record

import MaterialUI.StyleProps

type PaperProps s =
  ( elevation :: Int -- ^ Default: `2`
  , square :: Boolean -- ^ Default: `false`
  , style :: { | s }
  )


foreign import paperImpl :: forall p. ReactComponent p

paper :: forall s p. SubRow s StyleProps =>
                     SubRow p (PaperProps s) => { | p } -> Array JSX -> JSX
paper props children = element paperImpl $ Record.union { children } props

paper_ :: Array JSX -> JSX
paper_ children  = element paperImpl { children }
