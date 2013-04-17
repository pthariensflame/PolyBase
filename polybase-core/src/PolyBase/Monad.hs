{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies #-}
module PolyBase.Monad (Pointed(..){-,
                       Apply(..),
                       Applicative(..)-},
                       Bind(..),
                       Monad(..),
                       Kleisli(..)) where
import Prelude hiding ((.), id, Functor(..), Monad(..), (=<<))
import PolyBase.Category
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

instance (Functor m) => Functor (Kleisli (m :: k -> k) (a :: k) :: k -> *) where
  type FunctorC1
