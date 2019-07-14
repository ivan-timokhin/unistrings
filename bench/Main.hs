module Main where

import Control.Exception (evaluate)
import qualified Criterion.Main as C
import qualified Data.Char as Ch
import qualified Data.Text.ICU.Char as ICU
import qualified Data.Vector.Unboxed as V
import System.IO (IOMode(ReadMode), hGetContents, hSetEncoding, utf8, withFile)

import qualified Data.UCD as UCD

main :: IO ()
main =
  C.defaultMain
    [ C.env readUDHR $ \udhr ->
        C.bgroup
          "UDHR"
          [ C.bgroup
              "General category"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.generalCategory
              , C.bench "Data.Char" $ mkBenchmark udhr Ch.generalCategory
              , C.bench "ICU" $
                mkBenchmark udhr (ICU.property ICU.GeneralCategory)
              ]
          , C.bgroup
              "Canonical combining class"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.canonicalCombiningClass
              , C.bench "ICU" $
                mkBenchmark udhr (ICU.property ICU.CanonicalCombiningClass)
              ]
          , C.bgroup
              "Name"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.name
              , C.bench "ICU" $ mkBenchmark udhr ICU.charName
              ]
          , C.bgroup
              "Name aliases"
              [ C.bench "UCD" $
                mkBenchmark udhr (evalPairsList . UCD.nameAliases)
              ]
          , C.bgroup
              "Block"
              [ C.bench "UCD" $ mkBenchmark udhr UCD.block
              -- https://github.com/bos/text-icu/pull/37
              -- , C.bench "ICU" $ mkBenchmark udhr ICU.blockCode
              ]
          , C.bench "No-op" $ mkBenchmark udhr id
          ]
    ]

mkBenchmark :: V.Vector Char -> (Char -> a) -> C.Benchmarkable
{-# INLINE mkBenchmark #-}
mkBenchmark vals f = C.nf (V.foldl' (\() c -> f c `seq` ()) ()) vals

evalPairsList :: [(a, b)] -> [(a, b)]
{-# INLINE evalPairsList #-}
evalPairsList xs = go xs `seq` xs
  where
    go ((a, b):rest) = a `seq` b `seq` go rest
    go [] = ()

readUDHR :: IO (V.Vector Char)
readUDHR =
  withFile "data/udhr/full_all.txt" ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    evaluate $ V.fromList contents
