{-
Copyright 2019 Ivan Timokhin

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
module UCD.Common
  ( Table(Table, getTable)
  , Range(Single, Range)
  , unicodeTableSize
  , tableToVector
  , adjustWith
  , adjustWithM
  , Parser
  , Parser_
  , tableP
  , enumeratedP
  , enumeratedFullP
  , enumeratedAbbrP
  , range
  , comment
  , comments
  , dropNothing
  , semicolon
  , char8
  , tableParser
  , fetchSimple
  , fetchBinaryMulti
  , fetchGeneral
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)

import Data.Unistring.UCD.Internal.Types
  ( EnumeratedProperty(abbreviatedPropertyValueName,
                   fullPropertyValueName)
  )
import Parser
  ( Parser
  , Parser_
  , Range(Range, Single)
  , Table(Table, getTable)
  , adjustWith
  , adjustWithM
  , char8
  , comment
  , comments
  , dropNothing
  , enumeratedP
  , range
  , semicolon
  , tableP
  , tableParser
  , tableToVector
  , unicodeTableSize
  )
import qualified Parser as P
import qualified Text.Megaparsec.Error as ME

enumeratedFullP :: (EnumeratedProperty p, Ord e) => Parser e p
enumeratedFullP = enumeratedP fullPropertyValueName

enumeratedAbbrP :: (EnumeratedProperty p, Ord e) => Parser e p
enumeratedAbbrP = enumeratedP abbreviatedPropertyValueName

fetchSimple :: FilePath -> Parser_ a -> IO (Table () () a)
fetchSimple = P.fetchSimple . decoratePath

fetchBinaryMulti :: FilePath -> IO (Map ByteString (Table () () Bool))
fetchBinaryMulti = P.fetchBinaryMulti . decoratePath

fetchGeneral :: ME.ShowErrorComponent e => FilePath -> Parser e a -> IO a
fetchGeneral = P.fetchGeneral . decoratePath

decoratePath :: FilePath -> FilePath
decoratePath = ("data/ucd/" ++)
