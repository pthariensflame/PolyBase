{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies, ConstraintKinds #-}
module PolyBase.Monad (Pointed(..){-,
                       Apply(..),
                       Applicative(..),
                       Bind(..),
                       Monad(..),
                       Kleisli(..)-}) where
import Prelude hiding ((.), id, Functor(..), Monad(..))
import GHC.Exts (Constraint)
import PolyBase.Category
import PolyBase.Indexed
import PolyBase.Functor

class (Functor f, FunctorC1 f ~ FunctorC2 f) => Pointed (f :: k -> k) where
  point :: forall (cat :: k2 -> k2 -> *). FunctorC2 cat 
