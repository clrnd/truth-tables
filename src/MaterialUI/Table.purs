module MaterialUI.Table where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (CSS)
import React.Basic.Events (EventHandler)
import Record as Record

import MaterialUI.Types


type TableProps =
  ( style :: CSS
  )

foreign import tableImpl :: forall p. ReactComponent p

table :: forall p. SubRow p TableProps => { | p } -> Array JSX -> JSX
table props children = element tableImpl $ Record.union { children } props

table_ :: Array JSX -> JSX
table_ children  = element tableImpl { children }

----------------------------

type TableBodyProps =
  (
  )

foreign import tableBodyImpl :: forall p. ReactComponent p

tableBody :: forall p. SubRow p TableBodyProps => { | p } -> Array JSX -> JSX
tableBody props children = element tableBodyImpl $ Record.union { children } props

tableBody_ :: Array JSX -> JSX
tableBody_ children  = element tableBodyImpl { children }

----------------------------

type TableRowProps =
  ( hover :: Boolean
  , selected :: Boolean
  , onClick :: EventHandler
  , style :: CSS
  )


foreign import tableRowImpl :: forall p. ReactComponent p

tableRow :: forall p. SubRow p TableRowProps => { | p } -> Array JSX -> JSX
tableRow props children = element tableRowImpl $ Record.union { children } props

tableRow_ :: Array JSX -> JSX
tableRow_ children  = element tableRowImpl { children }

----------------------------

type TableCellProps =
  ( numeric :: Boolean
  , padding :: Padding
  , onClick :: EventHandler
  , align :: Alignment
  , style :: CSS
  )

newtype Padding = Padding String

--default :: Padding
--default = Padding "default"

checkbox :: Padding
checkbox = Padding "checkbox"

dense :: Padding
dense = Padding "dense"

none :: Padding
none = Padding "none"


foreign import tableCellImpl :: forall p. ReactComponent p

tableCell :: forall p. SubRow p TableCellProps => { | p } -> Array JSX -> JSX
tableCell props children = element tableCellImpl $ Record.union { children } props

tableCell_ :: Array JSX -> JSX
tableCell_ children  = element tableCellImpl { children }

----------------------------

type TableHeadProps = ()

foreign import tableHeadImpl :: forall p. ReactComponent p

tableHead :: forall p. SubRow p TableHeadProps => { | p } -> Array JSX -> JSX
tableHead props children = element tableHeadImpl $ Record.union { children } props

tableHead_ :: Array JSX -> JSX
tableHead_ children  = element tableHeadImpl { children }

----------------------------

type TableFooterProps = ()

foreign import tableFooterImpl :: forall p. ReactComponent p

tableFooter :: forall p. SubRow p TableFooterProps => { | p } -> Array JSX -> JSX
tableFooter props children = element tableFooterImpl $ Record.union { children } props

tableFooter_ :: Array JSX -> JSX
tableFooter_ children  = element tableFooterImpl { children }
