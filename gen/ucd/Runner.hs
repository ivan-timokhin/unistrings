{-# LANGUAGE CPP #-}

module Runner
  ( both_
  , for_
  , traverse_
  ) where
#if defined(THREADED)
import Control.Concurrent.Async
  ( concurrently_
  , forConcurrently_
  , mapConcurrently_
  )

both_ :: IO a -> IO b -> IO ()
both_ = concurrently_

for_ :: Foldable f => f a -> (a -> IO b) -> IO ()
for_ = forConcurrently_

traverse_ :: Foldable f => (a -> IO b) -> f a -> IO ()
traverse_ = mapConcurrently_
#else
import Data.Foldable (for_, traverse_)
import Data.Functor (void)

both_ :: IO a -> IO b -> IO ()
both_ x y = void $ x *> y
#endif
