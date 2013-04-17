{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs, TypeFamilies #-}
module PolyBase.Category (Category(..),
                          (:~)(..),
                          Op(..),
                          HasProduct(..),
                          HasUnit(..)) where
import Prelude hiding ((.), id, fst, snd)
import qualified Prelude as P
import qualified Data.Tuple as T (swap)

class Category (cat :: k -> k -> *) where
  (.) :: forall (a :: k) (b :: k) (c :: k). cat b c -> cat a b -> cat a c
  id :: forall (a :: k). cat a a

instance Category (->) where
  (.) = (P..)
  id = P.id

data (:~) :: k -> k -> * where
  Refl :: forall (a :: k). a :~ a

instance Category (:~) where
  Refl . Refl = Refl
  id = Refl

newtype Op (cat :: k -> k -> *) (a :: k) (b :: k) = Op { runOp :: cat b a }

instance (Category cat) => Category (Op cat) where
  Op x . Op y = Op (y . x)
  id = Op id

class (Category cat) => HasProduct (cat :: k -> k -> *) where
  type Product cat :: k -> k -> k
  fst :: forall (prod :: k -> k -> k) (a :: k) (b :: k). (prod ~ Product cat) => cat (prod a b) a
  fst = snd . swap
  snd :: forall (prod :: k -> k -> k) (a :: k) (b :: k). (prod ~ Product cat) => cat (prod a b) b
  snd = fst . swap
  swap :: forall (prod :: k -> k -> k) (a :: k) (b :: k). (prod ~ Product cat) => cat (prod a b) (prod b a)

-- | 'Product' = @(,)@
instance HasProduct (->) where
  type Product (->) = (,)
  fst = P.fst
  snd = P.snd
  swap = T.swap

class (HasProduct cat) => HasUnit (cat :: k -> k -> *) where
  type Unit cat :: k
  unitL :: forall (prod :: k -> k -> k) (unit :: k) (a :: k). (prod ~ Product cat, unit ~ Unit cat) => cat a (prod unit a)
  unitL = swap . unitR
  unitR :: forall (prod :: k -> k -> k) (unit :: k) (a :: k). (prod ~ Product cat, unit ~ Unit cat) => cat a (prod a unit)
  unitR = swap . unitL

-- | 'Unit' = @()@
instance HasUnit (->) where
  type Unit (->) = ()
  unitL v = ((), v)
  unitR v = (v, ())
