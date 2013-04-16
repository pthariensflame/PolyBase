{-# LANGUAGE PolyKinds, TypeOperators, KindSignatures, ScopedTypeVariables, RankNTypes, GADTs #-}
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

data Op :: (k -> k -> *) -> k -> k -> * where
  Op :: forall (cat :: k -> k -> *) (a :: k) (b :: k). { getCat :: cat b a } -> Op cat a b


