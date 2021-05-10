module Config.ScreenEdge where

data ScreenEdge
  = ScreenTop
  | ScreenBottom
  | ScreenLeft
  | ScreenRight
  deriving (Enum, Bounded)
