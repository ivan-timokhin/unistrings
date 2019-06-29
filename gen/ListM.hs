{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module ListM where

data ListM m a
  = Nil
  | Cons a (m (ListM m a))

deriving instance
         (Show (m (ListM m a)), Show a) => Show (ListM m a)

fromList :: Applicative f => [a] -> ListM f a
fromList = foldr (\x xs -> Cons x (pure xs)) Nil

generatePartitionings :: Int -> Int -> Int -> [ListM [] Int]
generatePartitionings depth lowBound hiBound
  | depth <= 0 = [Nil]
  | otherwise =
    Nil :
    [ Cons b $ generatePartitionings (depth - 1) lowBound (b - 1)
    | b <- [lowBound .. hiBound]
    ]