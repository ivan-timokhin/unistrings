{-
Copyright 2020 Ivan Timokhin

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Singletons
Description : Singletons support for runtime reflection
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Singletons
  ( Sing
  , Known(sing)
  , SDecide(decideEq)
  , Decided(Proven, Disproven)
  ) where

import Data.Type.Equality ((:~:), TestEquality(testEquality))
import Data.Kind (Type)

data family Sing (a :: k)

class Known a where
  sing :: Sing a

data Decided p
  = Proven p
  | Disproven (forall v. p -> v)

class SDecide k where
  decideEq :: Sing (a :: k) -> Sing (b :: k) -> Decided (a :~: b)

instance SDecide k => TestEquality (Sing :: k -> Type) where
  testEquality s1 s2 =
    case decideEq s1 s2 of
      Proven refl -> Just refl
      Disproven _ -> Nothing
