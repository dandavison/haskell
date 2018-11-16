class MyFunctor f where
  fmap' :: (a -> b) -> f a -> f b

  (<$) :: b -> f a -> f b
  (<$) = fmap' . const

  -- const :: b -> a -> b
  -- (fmap' . const) b :: f a -> f b
  -- (fmap' . const) b (f a) :: f b
  -- therefore
  -- (fmap' . const) :: b -> (f a) -> f b

instance MyFunctor [] where

  -- fmap' :: (a -> b) -> [a] -> [b]
  fmap' _ [] = []
  fmap' g (x:xs) = g x : fmap' g xs


instance MyFunctor Maybe where

  -- fmap' :: (a -> b) -> Maybe a -> Maybe b
  fmap' g (Just x) = Just (g x)
  fmap' _ Nothing = Nothing

  -- (<$) :: a -> Maybe b -> Maybe a
  (<$) x (Just y) = Just x
  (<$) x Nothing = Nothing


instance MyFunctor (Either e) where

  -- fmap' :: (a -> b) -> Either e a -> Either e b
  fmap' f x = case x of
    Left e -> Left e
    Right y -> Right (f y)

  -- (<$) :: a -> Either e b -> Either e a
  (<$) x (Left e) = Left e
  (<$) x (Right y) = Right x


instance MyFunctor ((->) e) where

  -- fmap' :: (a -> b) -> (e -> a) -> (e -> b)
  fmap' = (.)

  -- (<$) :: a -> (e -> b) -> (e -> b)
  (<$) x _ = const x


main = do
  print "MyFunctor []"
  print ("fmap' (+1) [1, 2] == " ++ (show $ fmap' (+1) [1, 2]))
  -- fmap' (+1) [1, 2] == [2,3]

  print "myFunctor Maybe"
  print ("fmap' (+1) (Just 1) == " ++ (show $ fmap' (+1) (Just 1)))
  -- fmap' (+1) (Just 1) == Just 2

  print "MyFunctor (Either e)"
  print ("fmap' (+1) (Left 1) == " ++ (show $ fmap' (+1) (Left 1)))
  -- fmap' (+1) (Left 1) == Left 1

  -- print ("fmap' (+1) (Right 1) == " ++ (show $ fmap' (+1) (Right 1)))
  -- Ambiguous type variable ‘a0’ arising from a use of ‘show’

  print "MyFunctor ((->) e)"
  print ("(fmap' (+1) (+99)) 1 == " ++ (show $ (fmap' (+1) (+99)) 1))
  -- (fmap' (+1) (+99)) 1 == 101
