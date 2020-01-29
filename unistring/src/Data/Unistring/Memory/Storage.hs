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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{-|
Module      : Data.Unistring.Memory.Storage
Description : Supported storage types
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Storage
  ( Storage(Native, Foreign)
  , Sing(SNative, SForeign)
  ) where

import Data.Type.Equality ((:~:)(Refl))

import Data.Unistring.Singletons
  ( Decided(Disproven, Proven)
  , Known(sing)
  , SDecide(decideEq)
  , Sing
  )

data Storage
  = Native
  | Foreign

data instance  Sing (storage :: Storage) where
        SNative :: Sing 'Native
        SForeign :: Sing 'Foreign

deriving instance Show (Sing (storage :: Storage))

instance Known 'Native where
  sing = SNative

instance Known 'Foreign where
  sing = SForeign

instance SDecide Storage where
  decideEq SNative SNative = Proven Refl
  decideEq SForeign SForeign = Proven Refl
  decideEq SNative SForeign = Disproven $ \case {}
  decideEq SForeign SNative = Disproven $ \case {}
