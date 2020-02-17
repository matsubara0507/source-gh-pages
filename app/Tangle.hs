{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Tangle
    ( hitchAt'
    , lasso'
    , lasso2
    , Record' (..)
    ) where

import           Control.Monad.Tangle
import           Data.Extensible       hiding (TangleT)
import           Data.Functor.Identity

newtype Record' xs h = Record' (xs :& Comp h (Field Identity))

hitchAt' ::
  forall xs x m . Monad m
  =>  Membership xs x -> TangleT (Record' xs) m (Field Identity x)
hitchAt' m =
  hitch $ \f (Record' r) -> fmap Record' (pieceAt m (fmap Comp . f . getComp) r)

lasso' ::
  forall xs k v m . (Monad m, Lookup xs k v)
   => FieldName k -> TangleT (Record' xs) m v
lasso' _ = fmap (runIdentity . getField) val
  where
    val :: TangleT (Record' xs) m (Field Identity (k >: v))
    val = hitch $ \f (Record' r) -> fmap Record' (pieceAssoc (fmap Comp . f . getComp) r)

lasso2
  :: forall xs k1 k2 v1 v2 m . (Monad m, Lookup xs k1 v1, Lookup xs k2 v2)
  => (FieldName k1, FieldName k2) -> TangleT (Record' xs) m (v1, v2)
lasso2 (k1, k2) = (,) <$> lasso' k1 <*> lasso' k2
