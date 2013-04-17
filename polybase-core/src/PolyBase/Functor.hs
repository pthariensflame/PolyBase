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

-- | 'FunctorC1' = ':~'
--   
--   'FunctorC2' = @->@
instance Functor ((:~) (x :: k) :: k -> *) where
  type FunctorC1 ((:~) x) = ((:~) :: k -> k -> *)
  type FunctorC2 ((:~) x) = ((->) :: * -> * -> *)
  fmap Refl Refl = Refl

-- | 'FunctorC1' = ':~'
--   
--   'FunctorC2' = ':->'
instance Functor ((:~) :: k -> k -> *) where
  type FunctorC1 (:~) = ((:~) :: k -> k -> *)
  type FunctorC2 (:~) = ((:->) :: (k -> *) -> (k -> *) -> *)
  fmap Refl = Indexed (\Refl -> Refl)

-- | 'FunctorC1' = ':~'
--   
--   'FunctorC2' = @->@
instance Functor ((v :: *) := (i :: ki) :: ki -> *) where
  type FunctorC1 (v := i) = ((:~) :: ki -> ki -> *)
  type FunctorC2 (v := i) = ((->) :: * -> * -> *)
  fmap Refl = id

-- | 'FunctorC1' = ':~'
--   
--   'FunctorC2' = ':->'
instance Functor ((:=) (v :: *) :: ki -> ki -> *) where
  type FunctorC1 ((:=) v) = ((:~) :: ki -> ki -> *)
  type FunctorC2 ((:=) v) = ((:->) :: (ki -> *) -> (ki -> *) -> *)
  fmap Refl = id

-- | 'FunctorC1' = ':~'
--   
--   'FunctorC2' = 'Indexed' (':->')
instance Functor ((:=) :: * -> ki -> ki -> *) where
  type FunctorC1 ((:=) :: * -> ki -> ki -> *) = ((->) :: * -> * -> *)
  type FunctorC2 ((:=) :: * -> ki -> ki -> *) = (Indexed (:->) :: (ki -> ki -> *) -> (ki -> ki -> *) -> *)
  fmap f = Indexed (Indexed (\(AtKey x) -> AtKey (f x)))

-- | 'FunctorC1' = @->@
--   
--   'FunctorC2' = @->@
instance Functor ((->) (e :: *) :: * -> *) where
  type FunctorC1 ((->) e) = (->)
  type FunctorC2 ((->) e) = (->)
  fmap f g = f . g

-- | 'FunctorC1' = 'Op' (@->@)
--   
--   'FunctorC2' = ':->'
instance Functor ((->) :: * -> * -> *) where
  type FunctorC1 (->) = Op (->)
  type FunctorC2 (->) = (:->)
  fmap (Op f) = Indexed (\g -> g . f)

contramap :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, Op cat1 ~ FunctorC1 f, cat2 ~ FunctorC2 f) => cat1 b a -> cat2 (f a) (f b)
contramap = fmap . Op

contramap' :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, cat1 ~ FunctorC1 f, Op cat2 ~ FunctorC2 f) => cat1 a b -> cat2 (f b) (f a)
contramap' = runOp . fmap

fmap' :: forall (f :: k1 -> k2) (a :: k1) (b :: k1) (cat1 :: k1 -> k1 -> *) (cat2 :: k2 -> k2 -> *). (Functor f, Op cat1 ~ FunctorC1 f, Op cat2 ~ FunctorC2 f) => cat1 b a -> cat2 (f b) (f a)
fmap' = runOp . fmap . Op
