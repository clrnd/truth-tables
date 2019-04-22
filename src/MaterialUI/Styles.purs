module MaterialUI.Styles where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Record as Record

type CreateThemeOpts =
    ( typography :: { | ( fontFamily :: String ) } )

type MuiThemeProviderProps =
  ( theme :: Theme
  )

foreign import data Theme :: Type
foreign import defaultMuiTheme :: Theme
foreign import muiThemeProviderImpl :: forall p. ReactComponent p
foreign import createMuiThemeImpl :: forall x. x -> Theme

createMuiTheme :: forall p. SubRow p CreateThemeOpts => { | p } -> Theme
createMuiTheme = createMuiThemeImpl

muiThemeProvider :: forall p. SubRow p MuiThemeProviderProps => { | p } -> Array JSX -> JSX
muiThemeProvider props children = element muiThemeProviderImpl $
    Record.union { children } props

muiThemeProvider_ :: Array JSX -> JSX
muiThemeProvider_ children = element muiThemeProviderImpl { children }
