{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies #-}
module PolyBase.Functor (Functor(..),
                         contramap) where
import Prelude hiding ((.), id, Functor(..))
import PolyBase.Category
import PolyBase.Indexed

class (Category (FunctorC1 f), Category (FunctorC2 f)) => Functor (f :: k1 -> k2) where
  type FunctorC1 f :: k1 -> k1 -> *
  type FunctorC2 f :: k2 -> k2 -> *
  fmap :: forall (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (cat1 ~ FunctorC1 f, cat2 ~ FunctorC2 f) => cat1 a b -> cat2 (f a) (f b)

instance Functor ((a :: *) := (i :: ki) :: ki -> *) where
  type FunctorC1 (a := i) = (:~)
  type FunctorC2 (a := i) = (->)
  fmap Refl = id

contramap :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, Op cat1 ~ FunctorC1 f, cat2 ~ FunctorC2 f) => cat1 b a -> cat2 (f a) (f b)
contramap = fmap . Op
