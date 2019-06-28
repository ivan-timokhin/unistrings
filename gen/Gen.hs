{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen
  ( Module(Module, moduleC)
  , generateEnum
  ) where

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity)
import Data.Foldable (toList)
import qualified Data.Vector as V

import Trie (TrieDesc(Bottom, Layer))

data Module =
  Module
    { moduleC :: [ByteString]
    }
  deriving (Show)

generateEnum ::
     forall a. Enum a
  => ByteString
  -> TrieDesc Identity a
  -> Module
generateEnum prefix = Module . (cHeader :) . go 0
  where
    go :: Foldable t => Int -> TrieDesc t a -> [ByteString]
    go _ (Bottom xs) = generateBottom xs
    go lv (Layer _ layer rest) =
      B.concat
        [ "HsInt "
        , prefix
        , "_layer_"
        , B.pack (show lv)
        , "[] = {"
        , contents
        , "};"
        ] :
      go (lv + 1) rest
      where
        contents =
          B.intercalate ", " $ map (B.pack . show) $ toList (Compose layer)
    generateBottom :: Foldable t => t (V.Vector a) -> [ByteString]
    generateBottom xs =
      [B.concat ["HsInt ", prefix, "_bottom[] = {", contents, "};"]]
      where
        contents =
          B.intercalate ", " $
          map (B.pack . show . fromEnum) $ toList (Compose xs)

cHeader :: ByteString
cHeader = "#include \"HsFFI.h\"\n"
