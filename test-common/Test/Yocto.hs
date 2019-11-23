{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Yocto
  ( Test
  , require
  , expect
  , predicate
  , equal
  , (=?)
  , (/=?)
  , (<?)
  , (>?)
  , (<=?)
  , (>=?)
  , Assertion
  , requireJust
  , criticalFailure
  , failure
  , acquire
  , Suite
  , test
  , group
  , test_
  , group_
  , label
  , with
  , with_
  , defaultMain
  , withT_
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import GHC.Stack
  ( CallStack
  , HasCallStack
  , SrcLoc
  , callStack
  , getCallStack
  , prettySrcLoc
  , withFrozenCallStack
  )
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Level
  = Expected
  | Required
  deriving (Show, Eq, Ord, Bounded, Enum)

data Failure =
  Failure
    { fLevel :: Level
    , fMsg :: String
    , fLoc :: Maybe SrcLoc
    }

newtype Test a =
  Test
    { runTest :: (Failure -> IO ()) -> (a -> IO ()) -> IO ()
    }
  deriving (Functor)

instance Applicative Test where
  pure x = Test $ \_ cont -> cont x
  f <*> x =
    Test $ \report cont ->
      runTest f report $ \f' -> runTest x report $ cont . f'

instance Monad Test where
  x >>= f =
    Test $ \report cont -> runTest x report $ \x' -> runTest (f x') report cont

instance MonadFail Test where
  fail = withFrozenCallStack criticalFailure

instance MonadIO Test where
  liftIO e = Test $ const (e >>=)

newtype Suite =
  Suite
    { runSuite :: ([String] -> Failure -> IO ()) -> IO ()
    }
  deriving (Semigroup, Monoid)

test :: String -> Test a -> Suite
{-# INLINE test #-}
test name = label name . test_

test_ :: Test a -> Suite
{-# INLINE test_ #-}
test_ t = Suite $ \report -> runTest t (report []) (const $ pure ())

group :: Foldable f => String -> f Suite -> Suite
{-# INLINE group #-}
group name = label name . group_

group_ :: Foldable f => f Suite -> Suite
{-# INLINE group_ #-}
group_ suites = Suite $ \report -> for_ suites $ \suite -> runSuite suite report

label :: String -> Suite -> Suite
{-# INLINE label #-}
label name suite = Suite $ \report -> runSuite suite $ report . (name :)

with :: IO a -> (a -> IO b) -> (a -> Suite) -> Suite
{-# INLINE with #-}
with acq rel suite =
  Suite $ \report -> bracket acq rel $ \a -> runSuite (suite a) report

with_ :: IO Suite -> Suite
{-# INLINE with_ #-}
with_ acq =
  Suite $ \report -> do
    suite <- acq
    runSuite suite report

withT_ :: Test Suite -> Suite
{-# INLINE withT_ #-}
withT_ ts =
  Suite $ \report -> runTest ts (report []) $ \suite -> runSuite suite report

defaultMain :: Int -> Suite -> IO ()
{-# INLINE defaultMain #-}
defaultMain maxErrors suite = do
  critFailures <- newIORef (0 :: Int)
  failures <- newIORef (0 :: Int)
  toReport <- newIORef maxErrors
  runSuite suite $ \context failureReport -> do
    (doReport, final) <-
      atomicModifyIORef' toReport (\c -> (c - 1, (c > 0, c == 1)))
    case fLevel failureReport of
      Expected -> do
        when doReport $ hPutStrLn stderr "Failure"
        atomicModifyIORef' failures (\c -> (c + 1, ()))
      Required -> do
        when doReport $ hPutStrLn stderr "Critical failure"
        atomicModifyIORef' critFailures (\c -> (c + 1, ()))
    when doReport $ do
      hPutStrLn stderr $ "  in " ++ intercalate "/" context
      for_ (fLoc failureReport) $ \loc ->
        hPutStrLn stderr $ "  at " ++ prettySrcLoc loc
      hPutStrLn stderr $ "  " ++ fMsg failureReport
      when final $
        hPutStrLn stderr $
        "Reached limit of " ++
        show maxErrors ++ " errors; suppressing further printing"
  critFailuresTotal <- readIORef critFailures
  failuresTotal <- readIORef failures
  putStrLn $ "Regular failures: " ++ show failuresTotal
  putStrLn $ "Critical failures: " ++ show critFailuresTotal
  when (critFailuresTotal + failuresTotal /= 0) exitFailure

locOf :: CallStack -> Maybe SrcLoc
{-# INLINE locOf #-}
locOf = fmap snd . listToMaybe . getCallStack

criticalFailure :: HasCallStack => String -> Test a
{-# INLINE criticalFailure #-}
criticalFailure msg =
  Test $ \report _ ->
    report Failure {fLevel = Required, fMsg = msg, fLoc = locOf callStack}

failure :: HasCallStack => String -> Test ()
{-# INLINE failure #-}
failure msg =
  Test $ \report cont -> do
    report Failure {fLevel = Expected, fMsg = msg, fLoc = locOf callStack}
    cont ()

newtype Assertion =
  Assertion
    { runAssertion :: (String -> Test ()) -> ShowS -> Test ()
    }

require :: HasCallStack => String -> Assertion -> Test ()
{-# INLINE require #-}
require name assert =
  withFrozenCallStack $ runAssertion assert criticalFailure (showString name)

expect :: HasCallStack => String -> Assertion -> Test ()
{-# INLINE expect #-}
expect name assert =
  withFrozenCallStack $ runAssertion assert failure (showString name)

requireJust :: HasCallStack => String -> Maybe a -> Test a
{-# INLINE requireJust #-}
requireJust _ (Just a) = pure a
requireJust msg Nothing = withFrozenCallStack $ criticalFailure msg

predicate :: Bool -> Assertion
{-# INLINE predicate #-}
predicate p = Assertion $ \report msg -> unless p $ report $ msg ""

equal :: (Eq a, Show a) => a -> a -> Assertion
{-# INLINE equal #-}
equal x y =
  Assertion $ \report msg ->
    if x /= y
      then report $
           msg $
           showString ": " $
           showsPrec 4 x $ showString " /= " $ showsPrec 4 y ""
      else pure ()

acquire :: IO a -> (a -> IO ()) -> Test a
{-# INLINE acquire #-}
acquire acq rel = Test $ \_ -> bracket acq rel

(=?) :: (Eq a, Show a) => a -> a -> Assertion
{-# INLINE (=?) #-}
(=?) x y =
  Assertion $ \report msg ->
    when (x /= y) $
    report $
    msg $
    showString ": expected " $
    showsPrec 10 x $ showString ", got " $ showsPrec 10 y ""

(/=?) :: (Show a, Eq a) => a -> a -> Assertion
{-# INLINE (/=?) #-}
(/=?) = assertRelation "not equal to" (/=)

(<?) :: (Show a, Ord a) => a -> a -> Assertion
{-# INLINE (<?) #-}
(<?) = assertRelation "greater than" (<)

(>?) :: (Show a, Ord a) => a -> a -> Assertion
{-# INLINE (>?) #-}
(>?) = assertRelation "less than" (>)

(<=?) :: (Show a, Ord a) => a -> a -> Assertion
{-# INLINE (<=?) #-}
(<=?) = assertRelation "no less than" (<=)

(>=?) :: (Show a, Ord a) => a -> a -> Assertion
{-# INLINE (>=?) #-}
(>=?) = assertRelation "no greater than" (>=)

assertRelation ::
     (Show a, Show b) => String -> (a -> b -> Bool) -> a -> b -> Assertion
{-# INLINE assertRelation #-}
assertRelation relation cmp expected actual =
  Assertion $ \report msg ->
    unless (expected `cmp` actual) $
    report $ msg $ violatedRelation relation expected actual

violatedRelation :: (Show a, Show b) => String -> a -> b -> String
{-# INLINE violatedRelation #-}
violatedRelation relation expected actual =
  showString ": expected value " $
  showString relation $
  showString " " $
  showsPrec 10 expected $ showString ", got " $ showsPrec 10 actual ""
