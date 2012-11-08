module SlipSlop.Element (
  NumberedElement(..),
  NamedElement(..)) where


data NumberedElement = NumberedElement Integer
  deriving (Eq, Ord, Show)

data NamedElement = NamedElement String
  deriving (Eq, Ord, Show)