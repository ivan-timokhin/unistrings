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
Module      : Data.Unistring.Memory.Ownership
Description : Differentiating strings backed by a full array or a slice
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Memory.Ownership
  ( Ownership(Full, Slice)
  , Sing(SFull, SSlice)
  ) where

import Data.Type.Equality ((:~:)(Refl))

import Data.Unistring.Singletons
  ( Decided(Disproven, Proven)
  , Known(sing)
  , SDecide(decideEq)
  , Sing
  )

data Ownership
  = Full
  | Slice

data instance  Sing (ownership :: Ownership) where
        SFull :: Sing 'Full
        SSlice :: Sing 'Slice

deriving instance Show (Sing (ownership :: Ownership))

instance Known 'Full where
  sing = SFull

instance Known 'Slice where
  sing = SSlice

instance SDecide Ownership where
  decideEq SFull SFull = Proven Refl
  decideEq SSlice SSlice = Proven Refl
  decideEq SFull SSlice = Disproven $ \case {}
  decideEq SSlice SFull = Disproven $ \case {}
