{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen
  ( Module(Module, moduleC, moduleHs)
  , generateEnum
  , EnumSpec(..)
  ) where

import Data.Bits (shiftL)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (toList)
import Data.Functor.Compose (Compose(Compose))
import Data.Functor.Identity (Identity(Identity))
import qualified Data.Vector as V
import Numeric (showHex)

import Gen.Type (EnumTy, IntegralType(itC, itHaskell))
import Trie (TrieDesc(Bottom, Layer))

data Module =
  Module
    { moduleC :: [ByteString]
    , moduleHs :: [ByteString]
    }
  deriving (Show)

data EnumSpec =
  EnumSpec
    { esCPrefix :: ByteString
    , esHsType :: ByteString
    , esHsTypeModule :: ByteString
    }

generateEnum :: Enum a => EnumSpec -> TrieDesc EnumTy Identity a -> Module
generateEnum spec trie =
  Module
    { moduleC = generateEnumC (esCPrefix spec) trie
    , moduleHs = generateEnumHs spec trie
    }

generateEnumC ::
     forall a. Enum a
  => ByteString
  -> TrieDesc EnumTy Identity a
  -> [ByteString]
generateEnumC prefix = (cHeader :) . go 0
  where
    go :: Foldable t => Int -> TrieDesc EnumTy t a -> [ByteString]
    go _ (Bottom ty xs) = generateBottom ty xs
    go lv (Layer ty _ layer rest) =
      B.concat
        [ itC ty
        , " const "
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
    generateBottom ::
         Foldable t => IntegralType -> t (V.Vector a) -> [ByteString]
    generateBottom ty xs =
      [B.concat [itC ty, " const ", prefix, "_bottom[] = {", contents, "};"]]
      where
        contents =
          B.intercalate ", " $
          map (B.pack . show . fromEnum) $ toList (Compose xs)

generateEnumHs ::
     forall a. Enum a
  => EnumSpec
  -> TrieDesc EnumTy Identity a
  -> [ByteString]
generateEnumHs spec trie =
  header ++ [""] ++ foreignImports 0 trie ++ [""] ++ function
  where
    foreignImports :: Foldable t => Int -> TrieDesc EnumTy t a -> [ByteString]
    foreignImports _ (Bottom ty _) =
      [ B.concat
          [ "foreign import ccall \"&\" "
          , esCPrefix spec
          , "_bottom :: Ptr "
          , itHaskell ty
          ]
      ]
    foreignImports lv (Layer ty _ _ rest) =
      B.concat
        [ "foreign import ccall \"&\" "
        , esCPrefix spec
        , "_layer_"
        , B.pack (show lv)
        , " :: Ptr "
        , itHaskell ty
        ] :
      foreignImports (lv + 1) rest
    header =
      [ B.concat ["import ", esHsTypeModule spec, " (", esHsType spec, ")"]
      , "import Data.UCD.Internal.Ptr (Ptr, unsafeReadPtr)"
      , "import Data.Bits ((.&.), shiftR, shiftL)"
      , "import Data.Int (Int8, Int16, Int32)"
      , "import Data.Word (Word8, Word16)"
      ]
    function :: [ByteString]
    function =
      sig : "retrieve cp = toEnum val" : " where" : map ("  " <>) locals
      where
        sig = B.concat ["retrieve :: Int -> ", esHsType spec]
        locals =
          case trie of
            Bottom _ (Identity _) ->
              [ B.concat
                  [ "val = fromEnum $ unsafeReadPtr "
                  , esCPrefix spec
                  , "_bottom cp"
                  ]
              ]
            Layer _ nbits _ rest ->
              B.concat
                [ "i0 = fromEnum $ unsafeReadPtr "
                , esCPrefix spec
                , "_layer_0 $ cp `shiftR` "
                , B.pack $ show nbits
                ] :
              go 0 nbits rest
        go :: Int -> Int -> TrieDesc EnumTy V.Vector a -> [ByteString]
        go depth prevBits (Bottom _ _) =
          [ B.concat
              [ "val = fromEnum $ unsafeReadPtr "
              , esCPrefix spec
              , "_bottom $ i"
              , B.pack $ show depth
              , " `shiftL` "
              , B.pack $ show prevBits
              , " + cp .&. "
              , mask prevBits
              ]
          ]
        go depth prevBits (Layer _ bits _ rest) =
          B.concat
            [ "i"
            , B.pack $ show (depth + 1)
            , " = fromEnum $ unsafeReadPtr "
            , esCPrefix spec
            , "_layer_"
            , B.pack $ show (depth + 1)
            , " $ i"
            , B.pack $ show depth
            , " `shiftL` "
            , B.pack $ show (prevBits - bits)
            , " + (cp `shiftR` "
            , B.pack $ show bits
            , ") .&. "
            , mask (prevBits - bits)
            ] :
          go (depth + 1) bits rest

cHeader :: ByteString
cHeader = "#include \"HsFFI.h\"\n"

mask :: Int -> ByteString
mask nbits = B.pack $ "0x" ++ showHex (((1 :: Int) `shiftL` nbits) - 1) ""
