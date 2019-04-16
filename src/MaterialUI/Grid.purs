module MaterialUI.Grid where

import Prelude
import Row.Class (class SubRow)
import React.Basic (JSX, ReactComponent, element)
import Record as Record
import Unsafe.Coerce (unsafeCoerce)

newtype AlignContent = AlignContent String

stretchContent :: AlignContent
stretchContent = AlignContent "stretch"

centerContent :: AlignContent
centerContent = AlignContent "center"

flexStartContent :: AlignContent
flexStartContent = AlignContent "flex-start"

flexEndContent :: AlignContent
flexEndContent = AlignContent "flex-end"

spaceBetweenContent :: AlignContent
spaceBetweenContent = AlignContent "space-between"

spaceAroundContent :: AlignContent
spaceAroundContent = AlignContent "space-around"

newtype AlignItems = AlignItems String

flexStartItems :: AlignItems
flexStartItems = AlignItems "flex-start"

centerItems :: AlignItems
centerItems = AlignItems "center"

flexEndItems :: AlignItems
flexEndItems = AlignItems "flex-end"

stretchItems :: AlignItems
stretchItems = AlignItems "stretch"

baselineItems :: AlignItems
baselineItems = AlignItems "baseline"

newtype Direction = Direction String

row :: Direction
row = Direction "row"

rowReverse :: Direction
rowReverse = Direction "row-reverse"

column :: Direction
column = Direction "column"

columnReverse :: Direction
columnReverse = Direction "column-reverse"

newtype Justify = Justify String

flexStartJustify :: Justify
flexStartJustify = Justify "flex-start"

flexEndJustify :: Justify
flexEndJustify = Justify "flex-end"

centerJustify :: Justify
centerJustify = Justify "center"

spaceBetweenJustify :: Justify
spaceBetweenJustify = Justify "space-between"

spaceAroundJustify :: Justify
spaceAroundJustify = Justify "space-around"

newtype Grids = Grids Int

falseGrids :: Grids
falseGrids = Grids (unsafeCoerce false)

trueGrids :: Grids
trueGrids = Grids (unsafeCoerce true)

autoGrids :: Grids
autoGrids = Grids (unsafeCoerce "auto")

grids1 :: Grids
grids1 = Grids 1

grids2 :: Grids
grids2 = Grids 2

grids3 :: Grids
grids3 = Grids 3

grids4 :: Grids
grids4 = Grids 4

grids5 :: Grids
grids5 = Grids 5

grids6 :: Grids
grids6 = Grids 6

grids7 :: Grids
grids7 = Grids 7

grids8 :: Grids
grids8 = Grids 8

grids9 :: Grids
grids9 = Grids 9

grids10 :: Grids
grids10 = Grids 10

grids11 :: Grids
grids11 = Grids 11

grids12 :: Grids
grids12 = Grids 12

newtype Spacing = Spacing Int

spacing0 :: Spacing
spacing0 = Spacing 0

spacing8 :: Spacing
spacing8 = Spacing 8

spacing16 :: Spacing
spacing16 = Spacing 16

spacing24 :: Spacing
spacing24 = Spacing 24

spacing32 :: Spacing
spacing32 = Spacing 32

spacing40 :: Spacing
spacing40 = Spacing 40

newtype Wrap = Wrap String

nowrap :: Wrap
nowrap = Wrap "nowrap"

wrap :: Wrap
wrap = Wrap "wrap"

wrapReverse :: Wrap
wrapReverse = Wrap "wrap-reverse"


type GridProps =
  ( alignContent :: AlignContent -- ^ Default: `stretch`
  , alignItems :: AlignItems -- ^ Default: `stretch`
  , container :: Boolean -- ^ Default: `false`
  , direction :: Direction -- ^ Default: `row`
  , item :: Boolean -- ^ Default: `false`
  , justify :: Justify -- ^ Default: `flex-start`
  , xl :: Grids -- ^ Default: `false`
  , lg :: Grids -- ^ Default: `false`
  , md :: Grids -- ^ Default: `false`
  , sm :: Grids -- ^ Default: `false`
  , xs :: Grids -- ^ Default: `false`
  , spacing :: Spacing -- ^ Default: 0
  , wrap :: Wrap -- ^ Default: `wrap`
  , zeroMinWidth :: Boolean -- ^ Default: `false`
  )


foreign import gridImpl :: forall p. ReactComponent p

grid :: forall p. SubRow p GridProps => { | p } -> Array JSX -> JSX
grid props children = element gridImpl $ Record.union { children } props

grid_ :: Array JSX -> JSX
grid_ children  = element gridImpl { children }
