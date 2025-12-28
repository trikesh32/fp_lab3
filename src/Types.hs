module Types
  ( Point (..),
  )
where

data Point = Point
  { px :: Double,
    py :: Double
  }
  deriving (Show, Eq)