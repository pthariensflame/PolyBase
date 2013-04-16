{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, GADTs #-}
module PolyBase.Category (Category(..),
                          (:~)(..),
                          Op(..)) where
import Prelude hiding ((.), id)
import qualified Prelude as P

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
