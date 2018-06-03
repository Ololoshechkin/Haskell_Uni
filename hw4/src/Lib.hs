{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE Rank2Types      #-}

module Lib where

import           Data.Functor.Const    (Const (..), getConst)
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Language.Haskell.TH
import           Control.Monad       (replicateM)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n indices = do
    l <- replicateM n (newName "a")
    let f i = varE $ l !! i
    lamE [tupP (map varP l)] (tupE (map f indices))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

set :: Lens' s a -> a -> s -> s
set l b = runIdentity . l (const $ Identity b)

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

view :: Lens' s a -> s -> a
view l s = getConst (l Const s)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (\b -> (b, x)) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, b) = (\a -> (x, a)) <$> f b

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter func obj = let fb = func (getter obj) in fmap (setter obj) fb


choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens getter setter
  where
    getter (Left s1)  = getConst $ l1 Const s1
    getter (Right s2) = getConst $ l2 Const s2

    setter (Left s1) b  = Left $ runIdentity $ l1 (const $ Identity b) s1
    setter (Right s2) b = Right $ runIdentity $ l2 (const $ Identity b) s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = let t = runIdentity $ l (Identity . f) s in (f $ getConst $ l Const s, t)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = let t = runIdentity $ l (Identity . f) s in (getConst $ l Const s, t)

