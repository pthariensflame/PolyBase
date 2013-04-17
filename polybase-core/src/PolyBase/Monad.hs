{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies, UndecidableInstances #-}
module PolyBase.Monad (Pointed(..){-,
                       Apply(..),
                       Applicative(..)-},
                       Bind(..),
                       Monad(..),
                       Kleisli(..)) where
import Prelude hiding ((.), id, Functor(..), Monad(..), (=<<))
import PolyBase.Category
import PolyBase.Indexed
import PolyBase.Functor

class (Functor f, FunctorC1 f ~ FunctorC2 f) => Pointed (f :: k -> k) where
  point :: forall (cat :: k -> k -> *) (a :: k). (cat ~ FunctorC1 f) => cat a (f a)

class (Functor m, FunctorC1 m ~ FunctorC2 m) => Bind (m :: k -> k) where
  (=<<) :: forall (cat :: k -> k -> *) (a :: k) (b :: k). (cat ~ FunctorC1 m) => cat a (m b) -> cat (m a) (m b)
  (=<<) f = join . fmap f
  join :: forall (cat :: k -> k -> *) (a :: k). (cat ~ FunctorC1 m) => cat (m (m a)) (m a)
  join = (=<<) id

class (Pointed m, Bind m) => Monad m where

newtype Kleisli (m :: k -> k) (a :: k) (b :: k) = Kleisli { runKleisli :: FunctorC1 m a (m b) }

instance (Monad m) => Category (Kleisli (m :: k -> k) :: k -> k -> *) where
  Kleisli f . Kleisli g = Kleisli ((=<<) f . g)
  id = Kleisli point

instance (Functor m, FunctorC1 m ~ FunctorC2 m) => Functor (Kleisli (m :: k -> k) (a :: k) :: k -> *) where
  type FunctorC1 (Kleisli m a) = (FunctorC1 m :: k -> k -> *)
  type FunctorC2 (Kleisli m a) = ((->) :: * -> * -> *)
  fmap f (Kleisli g) = Kleisli (fmap f . g)

instance (Functor m, FunctorC1 m ~ FunctorC2 m) => Functor (Kleisli (m :: k -> k) :: k -> k -> *) where
  type FunctorC1 (Kleisli m) = (Op (FunctorC1 m) :: k -> k -> *)
  type FunctorC2 (Kleisli m) = ((:->) :: (k -> *) -> (k -> *) -> *)
  fmap (Op f) = Indexed (\(Kleisli g) -> Kleisli (g . f))
