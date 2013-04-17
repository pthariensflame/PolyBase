{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies, FlexibleContexts #-}
module PolyBase.Functor (Functor(..),
                         contramap,
                         contramap',
                         fmap') where
import Prelude hiding ((.), id, Functor(..))
import PolyBase.Category
import PolyBase.Indexed

class (Category (FunctorC1 f), Category (FunctorC2 f)) => Functor (f :: k1 -> k2) where
  type FunctorC1 f :: k1 -> k1 -> *
  type FunctorC2 f :: k2 -> k2 -> *
  fmap :: forall (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (cat1 ~ FunctorC1 f, cat2 ~ FunctorC2 f) => cat1 a b -> cat2 (f a) (f b)

instance Functor ((v :: *) := (i :: ki) :: ki -> *) where
  type FunctorC1 (v := i) = ((:~) :: ki -> ki -> *)
  type FunctorC2 (v := i) = ((->) :: * -> * -> *)
  fmap Refl = id

instance Functor ((:=) (v :: *) :: ki -> ki -> *) where
  type FunctorC1 ((:=) v) = ((:~) :: ki -> ki -> *)
  type FunctorC2 ((:=) v) = ((:->) :: (ki -> *) -> (ki -> *) -> *)
  fmap Refl = id
{-
instance Functor ((:=) :: * -> ki -> ki -> *) where
  type FunctorC1 (:=) = ((->) :: * -> * -> *)
  type FunctorC2 (:=) = (Indexed (:->) :: (ki -> ki -> *) -> (ki -> ki -> *) -> *)
  fmap f (AtKey x) = Indexed (Indexed (AtKey (f x)))
-}
instance Functor ((->) (e :: *) :: * -> *) where
  type FunctorC1 ((->) e) = (->)
  type FunctorC2 ((->) e) = (->)
  fmap f g = f . g

instance Functor ((->) :: * -> * -> *) where
  type FunctorC1 (->) = Op (->)
  type FunctorC2 (->) = Indexed (->)
  fmap f (Op g) = Indexed (g . f)

contramap :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, Op cat1 ~ FunctorC1 f, cat2 ~ FunctorC2 f) => cat1 b a -> cat2 (f a) (f b)
contramap = fmap . Op

contramap' :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, cat1 ~ FunctorC1 f, Op cat2 ~ FunctorC2 f) => cat1 a b -> cat2 (f b) (f a)
contramap' = runOp . fmap

fmap' :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, Op cat1 ~ FunctorC1 f, Op cat2 ~ FunctorC2 f) => cat1 b a -> cat2 (f b) (f a)
fmap' = runOp . fmap . Op
