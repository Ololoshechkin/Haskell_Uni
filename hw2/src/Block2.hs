{-# OPTIONS_GHC -ddump-deriv #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE InstanceSigs #-}
module Block2 where
import           Control.Applicative (liftA2)
import           Data.List
import           Data.Maybe          ()
import           Text.Read

stringSum :: String -> Maybe Int
stringSum s = foldl (liftA2 (+)) (Just 0) (words s >>= \t -> ((: []) . readMaybe) t)

newtype Optional a = Optional (Maybe (Maybe a))



instance Foldable Optional where
    foldMap f (Optional (Just (Just x))) = f x
    foldMap _ _                          = mempty


instance Functor Optional where
    fmap _ (Optional Nothing)         = Optional Nothing
    fmap _ (Optional (Just Nothing))  = Optional (Just Nothing)
    fmap f (Optional (Just (Just b))) = Optional (Just (Just (f b)))


instance Applicative Optional where
    pure a = Optional (Just (Just a))
--     (<*>) :: f (a -> b) -> f a -> f b
    (<*>) _ (Optional Nothing)        = Optional Nothing
    (<*>) (Optional Nothing) _        = Optional Nothing
    (<*>) _ (Optional (Just Nothing)) = Optional (Just Nothing)
--     (<*>) (Optional (Just Nothing)) _ = Optional (Just Nothing)
    (<*>) (Optional (Just (Just f))) (Optional (Just (Just y))) = Optional (Just (Just (f y)))



instance Monad Optional where
  return = pure
  Optional (Just (Just d)) >>= a = a d
  Optional (Just Nothing) >>= _ = Optional (Just Nothing)
  _ >>= _ = Optional Nothing

instance Traversable Optional where
--   traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
  traverse f (Optional (Just (Just x))) = fmap pure (f x)
  --     traverse f (Optional (Just (Just b)))  =  pure  (Optional (Just (Just (b))))
  traverse _ (Optional (Just Nothing))  = pure $ Optional $ Just Nothing
  traverse _ (Optional Nothing)         = pure $ Optional Nothing


data NonEmpty a = a :| [a]


instance Functor NonEmpty where
    fmap f (a:| xs) = f a :| fmap f xs


instance Applicative NonEmpty where
    pure a = a:|[]
    (<*>) (a:|xas) (b:|xbs) =  a b :| drop 1 [aa bb | aa <- a:xas, bb <- b:xbs]
--                         in (head t) :| (drop 1 t)

instance Foldable NonEmpty where
    foldMap f (s :| l) = foldl mappend (f s) (map f l)
    foldr f acc (s :| l) = f s (foldr f acc l)

instance Monad NonEmpty where
-- (>>=)  :: m a -> (a -> m b) -> m b
    (>>=) (a:|xs) f =  f a
    return a = a:|[]

instance Traversable NonEmpty where
  traverse f (l:|ls) = fmap (:|) (f l) <*> traverse f ls
