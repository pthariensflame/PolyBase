{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies, UndecidableInstances, FlexibleContexts, RankNTypes #-}
module PolyBase.Monad (Pointed(..),
                       copoint,
                       ipoint,
                       icopoint,
                       Bind(..),
                       (=<?),
                       ijoin,
                       Monad(..),
                       Kleisli(..)) where
import Prelude hiding ((.), id, Functor(..), Monad(..), (=<<))
import PolyBase.Category
import PolyBase.Indexed
import PolyBase.Functor

class (Category (PointedC f)) => Pointed (f :: k -> k) where
  type PointedC f :: k -> k -> *
  point :: forall (cat :: k -> k -> *) (a :: k). (cat ~ PointedC f) => cat a (f a)

copoint :: forall (f :: k -> k) (cat :: k -> k -> *) (a :: k). (Pointed f, Op cat ~ PointedC f) => cat (f a) a
copoint = runOp point

ipoint :: forall (f :: (ki -> k) -> (ki -> k)) (cat :: k -> k -> *) (a :: ki -> k) (i :: ki). (Pointed f, (Indexed cat :: (ki -> k) -> (ki -> k) -> *) ~ PointedC f) => cat (a i) (f a i)
ipoint = runIndexed point

icopoint :: forall (f :: (ki -> k) -> (ki -> k)) (cat :: k -> k -> *) (a :: ki -> k) (i :: ki). (Pointed f, Op (Indexed cat :: (ki -> k) -> (ki -> k) -> *) ~ PointedC f) => cat (f a i) (a i)
icopoint = runIndexed (runOp point)

icopoint' :: forall (f :: (ki -> k) -> (ki -> k)) (cat :: k -> k -> *) (a :: ki -> k) (i :: ki). (Pointed f, (Indexed (Op cat) :: (ki -> k) -> (ki -> k) -> *) ~ PointedC f) => cat (f a i) (a i)
icopoint' = runOp (runIndexed point)

class (Functor m, FunctorC1 m ~ FunctorC2 m) => Bind (m :: k -> k) where
  (=<<) :: forall (cat :: k -> k -> *) (a :: k) (b :: k). (cat ~ FunctorC1 m) => cat a (m b) -> cat (m a) (m b)
  (=<<) f = join . fmap f
  join :: forall (cat :: k -> k -> *) (a :: k). (cat ~ FunctorC1 m) => cat (m (m a)) (m a)
  join = (=<<) id

(=<?) :: forall (m :: (ki -> k) -> (ki -> k)) (cat :: k -> k -> *) (a :: ki -> k) (b :: ki -> k) (j :: ki). (Bind m, (Indexed cat :: (ki -> k) -> (ki -> k) -> *) ~ FunctorC1 m) => (forall (i :: ki). cat (a i) (m b i)) -> cat (m a j) (m b j)
(=<?) = runIndexed . (=<<) . Indexed

ijoin :: forall (m :: (ki -> k) -> (ki -> k)) (cat :: k -> k -> *) (a :: ki -> k) (i :: ki). (Bind m, (Indexed cat :: (ki -> k) -> (ki -> k) -> *) ~ FunctorC1 m) => cat (m (m a) i) (m a i)
ijoin = runIndexed join

class (Pointed m, Bind m, PointedC m ~ FunctorC1 m) => Monad m where

newtype Kleisli (m :: k -> k) (a :: k) (b :: k) = Kleisli { runKleisli :: FunctorC1 m a (m b) }

instance (Monad m) => Category (Kleisli (m :: k -> k) :: k -> k -> *) where
  Kleisli f . Kleisli g = Kleisli ((=<<) f . g)
  id = Kleisli point

-- | 'FunctorC1' = 'FunctorC1' @m@
--   
--   'FunctorC2' = @->@
instance (Functor m, FunctorC1 m ~ FunctorC2 m) => Functor (Kleisli (m :: k -> k) (a :: k) :: k -> *) where
  type FunctorC1 (Kleisli m a) = (FunctorC1 m :: k -> k -> *)
  type FunctorC2 (Kleisli m a) = ((->) :: * -> * -> *)
  fmap f (Kleisli g) = Kleisli (fmap f . g)

-- | 'FunctorC1' = 'Op' ('FunctorC1' @m@)
--   
--   'FunctorC2' = ':->'
instance (Functor m) => Functor (Kleisli (m :: k -> k) :: k -> k -> *) where
  type FunctorC1 (Kleisli m) = (Op (FunctorC1 m) :: k -> k -> *)
  type FunctorC2 (Kleisli m) = ((:->) :: (k -> *) -> (k -> *) -> *)
  fmap (Op f) = Indexed (\(Kleisli g) -> Kleisli (g . f))
