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
