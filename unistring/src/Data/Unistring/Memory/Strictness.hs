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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Strictness
Description : Parameter differentiating strict and lazy strings
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Strictness
  ( Strictness(Strict, Lazy)
  , Sing(SStrict, SLazy)
  ) where

import Data.Type.Equality ((:~:)(Refl))

import Data.Unistring.Singletons
  ( Decided(Disproven, Proven)
  , Known(sing)
  , SDecide(decideEq)
  , Sing
  )

data Strictness
  = Strict
  | Lazy

data instance  Sing (strictness :: Strictness) where
        SStrict :: Sing 'Strict
        SLazy :: Sing 'Lazy

deriving instance Show (Sing (strictness :: Strictness))

instance Known 'Strict where
  sing = SStrict

instance Known 'Lazy where
  sing = SLazy

instance SDecide Strictness where
  decideEq SStrict SStrict = Proven Refl
  decideEq SLazy SLazy = Proven Refl
  decideEq SStrict SLazy = Disproven $ \case {}
  decideEq SLazy SStrict = Disproven $ \case {}
