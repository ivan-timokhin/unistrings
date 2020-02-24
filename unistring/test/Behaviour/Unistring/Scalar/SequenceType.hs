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
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Behaviour.Unistring.Scalar.SequenceType
  ( SequenceType
  , SomeSequenceType(SomeSequenceType)
  ) where

import Test.Tasty.QuickCheck (Arbitrary(arbitrary, shrink))

import qualified Data.Unistring.Encoding.Form as EF
import Data.Unistring.Memory.Allocator (Allocator)
import Data.Unistring.Singletons (Known, Sing)

import Behaviour.Unistring.Encoding.Form (SomeEncodingForm(SomeEncodingForm))
import qualified Behaviour.Unistring.Memory.SequenceType as M
  ( SequenceType
  , SomeSequenceType(SomeSequenceType)
  , changeType
  )

data SequenceType storage allocator ownership strictness encoding =
  SequenceType
    (M.SequenceType storage allocator ownership strictness (EF.CodeUnit encoding))
    (Sing encoding)
  deriving (Show)

data SomeSequenceType where
  SomeSequenceType
    :: ( Allocator storage allocator
       , Known ownership
       , Known strictness
       , Known encoding
       )
    => SequenceType storage allocator ownership strictness encoding
    -> SomeSequenceType

deriving instance Show SomeSequenceType

instance Arbitrary SomeSequenceType where
  arbitrary = do
    SomeEncodingForm enc <- arbitrary
    M.SomeSequenceType mseq <- arbitrary
    pure $ SomeSequenceType (SequenceType mseq enc)
  shrink (SomeSequenceType (SequenceType mseq enc)) =
    [ SomeSequenceType (SequenceType (M.changeType mseq) enc')
    | SomeEncodingForm enc' <- shrink (SomeEncodingForm enc)
    ] ++
    [ SomeSequenceType (SequenceType mseq' enc)
    | M.SomeSequenceType mseq' <- shrink (M.SomeSequenceType mseq)
    ]
