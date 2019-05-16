module MaterialUI.Types where

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
