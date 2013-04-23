module Fretboarder.Parser.Expr where

data Expr a = Set a
            | Different (Expr a) (Expr a)
            | Join (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Set a)           = Set $ f a
  fmap f (Different e1 e2) = Different (fmap f e1) (fmap f e2)
  fmap f (Join e1 e2)      = Join (fmap f e1) (fmap f e2)
