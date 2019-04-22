module MaterialUI.Input where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)

type InputProps =
  ( defaultValue :: String
  , disabled :: Boolean
  , error :: Boolean
  , fullWidth :: Boolean -- ^ Default: `false`
  , id :: String
  , margin :: Margin
  , multiline :: Boolean -- ^ Default: `false`
  , name :: String
  , onChange :: EventHandler
  , placeholder :: String
  , readOnly :: Boolean
  , required :: Boolean
  , rows :: Int
  , rowsMax :: Int
  , value :: String
  )


newtype Margin = Margin String

dense :: Margin
dense = Margin "dense"

none :: Margin
none = Margin "none"


foreign import inputImpl :: forall p. ReactComponent p

input :: forall p. SubRow p InputProps => { | p } -> JSX
input props = element inputImpl props

input_ :: JSX
input_ = element inputImpl {}
