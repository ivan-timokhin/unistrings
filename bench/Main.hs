module Main where

import Control.Exception (evaluate)
import qualified Criterion.Main as C
import qualified Data.Char as Ch
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
              ]
          , C.bgroup
              "Canonical combining class"
              [C.bench "UCD" $ mkBenchmark udhr UCD.canonicalCombiningClass]
          ]
    ]

mkBenchmark :: V.Vector Char -> (Char -> a) -> C.Benchmarkable
{-# INLINE mkBenchmark #-}
mkBenchmark vals f = C.nf (V.foldl' (\() c -> f c `seq` ()) ()) vals

readUDHR :: IO (V.Vector Char)
readUDHR =
  withFile "data/udhr/full_all.txt" ReadMode $ \h -> do
    hSetEncoding h utf8
    contents <- hGetContents h
    evaluate $ V.fromList contents
