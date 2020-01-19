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
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_HADDOCK show-extensions #-}
{-|
Module      : Data.Unistring.Compat.Typeable
Description : Uniform Typeable interface for base-4.9
Copyright   : (c) Ivan Timokhin 2020
License     : Apache-2.0
Maintainer  : timokhin.iv@gmail.com
Stability   : experimental


-}
module Data.Unistring.Compat.Typeable
  ( Typeable
  , TypeRep
  , typeRep
  ) where
#if MIN_VERSION_base(4, 10, 0)
import Type.Reflection (TypeRep, Typeable, typeRep)
#else
import Data.Typeable (Typeable, eqT)
import Data.Type.Equality (TestEquality(testEquality))

-- This type wraps a constraint instead of Data.Typeable.TypeRep,
-- because TypeRep isn't indexed, but the dictionary /is/, which means
-- there's no need to manually keep track of indices ourselves.
data TypeRep a where
  TypeRep :: Typeable a => TypeRep a

typeRep :: Typeable a => TypeRep a
typeRep = TypeRep

instance TestEquality TypeRep where
  testEquality TypeRep TypeRep = eqT
#endif
