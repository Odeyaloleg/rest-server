module Logic where

class MustBeUnique a where
  isPresent :: a -> Bool

class HasParent a where
  isParentProper :: a -> Bool

-- Not sure this is the right place for this types(maybe should move into App component)

data Tag = Tag String

data Category = Category String

data ParentCategory = ParentCategory Int
