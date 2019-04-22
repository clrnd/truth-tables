module MaterialUI.Divider where

import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)

type DividerProps =
  ( absolute :: Boolean -- ^ Default: `false`
  , inset :: Boolean -- ^ Default: `false`
  , light :: Boolean -- ^ Default: `false`
  )


foreign import dividerImpl :: forall p. ReactComponent p

divider :: forall p. SubRow p DividerProps => { | p } -> JSX
divider props = element dividerImpl props

divider_ :: JSX
divider_ = element dividerImpl {}
