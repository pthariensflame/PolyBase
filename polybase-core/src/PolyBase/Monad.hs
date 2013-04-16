{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies #-}
module PolyBase.Monad (Applicative(..),
                       Monad(..),
                       Kleisli(..)) where
import Prelude hiding ((.), id, Functor(..), Monad(..))
import PolyBase.Category
import PolyBase.Functor

