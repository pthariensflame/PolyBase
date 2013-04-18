{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, RankNTypes, GADTs #-}
module PolyBase.Indexed (Indexed(..),
                         (:->),
                         (:=)(..)) where
import Prelude hiding ((.), id)
import PolyBase.Category

newtype Indexed (cat :: k -> k -> *) (a :: ki -> k) (b :: ki -> k) = Indexed { runIndexed :: forall (i :: ki). cat (a i) (b i) }

type (:->) = (Indexed (->) :: (ki -> *) -> (ki -> *) -> *)

instance (Category cat) => Category (Indexed (cat :: k -> k -> *) :: (ki -> k) -> (ki -> k) -> *) where
  Indexed x . Indexed y = Indexed (x . y)
  id = Indexed id

data (:=) :: * -> ki -> ki -> * where
  AtKey :: forall (v :: *) (i :: ki). { getAtKey :: v } -> (v := i) i
