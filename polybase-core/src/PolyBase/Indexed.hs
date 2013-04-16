{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, RankNTypes, GADTs #-}
module PolyBase.Indexed (Indexed(..),
                         (:->),
                         (:=)(..)) where
import Prelude hiding ((.), id)
import PolyBase.Category

data Indexed :: (k -> k -> *) -> (ki -> k) -> (ki -> k) -> * where
    Indexed :: forall (cat :: k -> k -> *) (a :: ki -> k) (b :: ki -> k). { runIndexed :: forall (i :: ki). cat (a i) (b i) } -> Indexed cat a b

type (:->) = (Indexed (->) :: (ki -> *) -> (ki -> *) -> *)

instance (Category cat) => Category (Indexed (cat :: k -> k -> *) :: (ki -> k) -> (ki -> k) -> *) where
    Indexed x . Indexed y = Indexed (x . y)
    id = Indexed id

data (:=) :: * -> ki -> ki -> * where
    AtKey :: forall (a :: *) (i :: ki). { getAtKey :: a } -> (a := i) i
