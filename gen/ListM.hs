module ListM where

data ListM m a
  = Nil
  | Cons a (m (ListM m a))

fromList :: Applicative f => [a] -> ListM f a
fromList = foldr (\x xs -> Cons x (pure xs)) Nil
