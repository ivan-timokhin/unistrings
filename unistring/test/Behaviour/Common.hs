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
{-# LANGUAGE MultiParamTypeClasses #-}

module Behaviour.Common
  ( (~~~)
  , (~/~)
  ) where

import Test.Tasty.QuickCheck (Property, counterexample)

import qualified Data.Unistring.Memory.Array as Array
import Data.Unistring.Memory.Primitive.Class.Unsafe (Primitive)
import qualified Data.Unistring.Memory.Slice.Internal as Slice
import qualified Data.Unistring.Memory.Sequence.Internal as Sequence
import qualified Data.Unistring.Scalar.Sequence.Internal as SSequence
import Data.Unistring.Singletons (Known)

class Eqv a b where
  eqv :: a -> b -> Bool

instance (Known storage1, Known storage2, Primitive a) =>
         Eqv (Array.Array storage1 allocator1 a) (Array.Array storage2 allocator2 a) where
  eqv = Array.equal

instance (Known storage1, Known storage2, Primitive a) =>
         Eqv (Slice.Slice storage1 allocator1 a) (Slice.Slice storage2 allocator2 a) where
  eqv = Slice.equal

instance ( Known storage1
         , Known storage2
         , Known ownership1
         , Known ownership2
         , Known strictness1
         , Known strictness2
         , Primitive a
         ) =>
         Eqv
         (Sequence.Sequence storage1 allocator1 ownership1 strictness1 a)
         (Sequence.Sequence storage2 allocator2 ownership2 strictness2 a) where
  eqv = Sequence.equal

instance ( Known storage1
         , Known storage2
         , Known ownership1
         , Known ownership2
         , Known strictness1
         , Known strictness2
         , Known encoding1
         , Known encoding2
         ) =>
         Eqv (SSequence.Sequence storage1 allocator1 ownership1 strictness1 encoding1)
             (SSequence.Sequence storage2 allocator2 ownership2 strictness2 encoding2) where
  eqv = SSequence.equal

(~~~) :: (Eqv a b, Show a, Show b) => a -> b -> Property
a ~~~ b = counterexample (show a ++ interpret res ++ show b) res
  where
    res = a `eqv` b
    interpret True = " == "
    interpret False = " /= "

(~/~) :: (Eqv a b, Show a, Show b) => a -> b -> Property
a ~/~ b = counterexample (show a ++ interpret res ++ show b) res
  where
    res = not $ a `eqv` b
    interpret True = " /= "
    interpret False = " == "
