{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Union
  ( OpenUnion (..)
  , inj
  , prj
  , decompose
  , weaken
  , match
  ) where

import Data.Kind (Type)
import Data.Proxy
import Fcf
import GHC.TypeLits hiding (type (+))
import Unsafe.Coerce

data OpenUnion (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenUnion :: Int -> f t -> OpenUnion f ts

type FindElem (key :: k) (ts :: [k]) = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))
{-# INLINE findElem #-}

inj :: forall f t ts. Member t ts => f t -> OpenUnion f ts
inj = UnsafeOpenUnion (findElem @t @ts)
{-# INLINE inj #-}

prj :: forall f t ts. Member t ts => OpenUnion f ts -> Maybe (f t)
prj (UnsafeOpenUnion i f) = if i == findElem @t @ts then Just $ unsafeCoerce f else Nothing
{-# INLINE prj #-}

decompose :: OpenUnion f (t ': ts) -> Either (f t) (OpenUnion f ts)
decompose (UnsafeOpenUnion 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenUnion n t) = Right $ UnsafeOpenUnion (n - 1) t
{-# INLINE decompose #-}

weaken :: OpenUnion f ts -> OpenUnion f (x ': ts)
weaken (UnsafeOpenUnion n t) = UnsafeOpenUnion (n + 1) t
{-# INLINE weaken #-}

match :: forall f ts b. (forall t. f t -> b) -> OpenUnion f ts -> b
match fn (UnsafeOpenUnion _ t) = fn t
{-# INLINE match #-}
