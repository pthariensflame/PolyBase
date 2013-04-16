{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies #-}
module PolyBase.Monad (Pointed(..),
                       Apply(..),
                       Applicative(..),
                       Bind(..),
                       Monad(..),
                       Kleisli(..)) where
import Prelude hiding ((.), id, Functor(..), Monad(..))
import PolyBase.Category
import PolyBase.Functor


