module MaterialUI.Paper where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Data.Symbol
import Prim.Row (class Lacks)
import Record as Record

type PaperProps =
  ( elevation :: Int
  , square :: Boolean
  , children :: Array JSX
  )


foreign import paperImpl :: forall p. ReactComponent p

paper :: forall p. Lacks "children" p => SubRow p PaperProps => { | p } -> Array JSX -> JSX
paper props children = element paperImpl $ Record.insert (SProxy :: SProxy "children") children props

paper_ :: Array JSX -> JSX
paper_ children  = element paperImpl { children }
