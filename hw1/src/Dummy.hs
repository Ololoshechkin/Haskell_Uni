{-# LANGUAGE InstanceSigs #-}

module Dummy where
import           Data.List
import           Data.Maybe     ()
import           Data.Semigroup
-- import           System.Random  (newStdGen, randomRs)


-- 1.------------------------------------------------------------

order3 ::  (Int, Int, Int) -> (Int, Int, Int)
order3 (a, b, c) = let l = sort [a, b, c] in (head l, l !! 1, l !! 2)
tOrder3 :: [Int] -> Bool
tOrder3 l =
    let (v1, v2, v3) = order3 (head l, l !! 1, l !! 2) in
    let s = sort l in
    v1 == head s && v2 == s !! 1 && v3 == s !! 2

smartReplicate :: [Int] -> [Int]
smartReplicate = concatMap (\x ->  replicate x x)
tSmartReplicate :: Bool
tSmartReplicate =
    let l = [1, 3, 1] in
    let res = smartReplicate l in
    head res == 1 && res !! 1 == 3 && res !! 2 == 3 && res !! 3 == 3 && res !! 4 == 1

contains :: (Foldable t, Eq a) => a -> [t a] -> [t a]
contains = filter . elem
tContains :: Bool
tContains =
   let l = [[1..5], [2, 4..8], [8, 16]] in
   let v = 8 in
   let res = contains v l in
   res == [[2:: Int, 4..8], [8:: Int, 16]]

stringSum :: String -> Int
stringSum = sum . map read . words

tStringSum :: Bool
tStringSum = stringSum "1 3 \t123\t 010 -1 -3 \n1\t\n3 55 -1\n\n\n-5" == 186

t_1 :: Bool
t_1 = tOrder3 [3, 4, 1] && tSmartReplicate && tContains && tStringSum
-- 2.------------------------------------------------------------


deleteNth :: Int -> [a] -> [a]
deleteNth n xs = let (a, b) = splitAt n xs in a ++ drop 1 b
tDeleteNth :: Bool
tDeleteNth = let l = [1, 2, 3, 4] in
    deleteNth 2 l == [1::Int, 2, 4]

merge2 :: Ord a => [a] -> [a] -> [a]
merge2 x y = case (x, y) of
    ([], _)        -> y
    (_, [])        -> x
    (h1:t1, h2:t2) -> if (<) h1 h2 then h1: merge2 t1 y else h2: merge2 x t2

-- mySplit :: Ord a => [a] -> [a] -> [a] -> ([a], [a])
-- mySplit x y z = case x of
--     []      -> (y,z)
--     h:other -> mySplit other z (h:y)

mergeSort :: Ord a => [a] -> [a]
mergeSort x = case x of
     [] -> x
     [_] -> x
     _ -> let (firstPart, secondPart) = splitAt (quot (length x) 2) x in
        merge2 (mergeSort firstPart) (mergeSort secondPart)

-- randomIntList :: Int -> Int -> Int -> IO [Int]
-- randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

-- example <- randomIntList 5 (-10) 10
-- mergeSort example
-- 3.1------------------------------------------------------------

data Day = Sun | Mon | Tue | Wen | Fith | Fri | Sat deriving (Eq, Enum, Show, Read)

-- nextDay (read "Sun" :: Day)

dayIndexPlusX :: Enum a => a -> Int -> Int
dayIndexPlusX day x = (fromEnum day + x) `mod` 7

nextDay :: (Enum a1, Enum a2) => a2 -> a1
nextDay day = toEnum (dayIndexPlusX day 1)
tNextDay :: Bool
tNextDay = nextDay Sun == Mon && nextDay Sat == Sun

afterDays :: (Enum a1, Enum a2) => a2 -> Int -> a1
afterDays day x = toEnum (dayIndexPlusX day x)
tAfterDays :: Bool
tAfterDays =
    let day = Tue in
    let x = 5 in
    Sun == afterDays day x

isWeekend :: Day -> Bool
isWeekend day = day == Sat || day == Sun
tIsWeekend :: Bool
tIsWeekend = isWeekend Sat && isWeekend Sun && not (isWeekend Mon)

daysToParty :: Enum a => a -> Int
daysToParty day =
    let i =  dayIndexPlusX day 0 in
    if i > 5 then 5 + 7 - i else 5 - i
tDaysToParty :: Bool
tDaysToParty = 0 == daysToParty Fri && 1 == daysToParty Fith && 6 == daysToParty Sat

t_3_1 :: Bool
t_3_1 = tNextDay && tAfterDays && tIsWeekend && tDaysToParty

-- 3.2------------------------------------------------------------
data Recreation = Library | Chirch | None deriving (Eq, Enum, Show, Read)

data Lord = IsLord | NoLord deriving (Eq, Show, Read)

newtype Castle = Castle {lord :: Lord} deriving (Eq, Show, Read)

newtype House = House {family :: Family} deriving (Eq, Show, Read)

data Wall = IsWall | NoWall deriving (Eq, Show, Read)

data CastleAndWall = CastleAndWall
                            { castle :: Castle
                            , wall   :: Wall
                            }
                            | NoCastle deriving (Eq, Show, Read)

data Person = IsPerson | NoPerson deriving (Eq, Show, Read)

data FourBoundedInt =   One | Two | Three | Four deriving (Eq, Enum, Show, Read)

newtype Family = Family {pa :: FourBoundedInt} deriving (Eq, Show, Read)

data NonEmpty a =  a :|  [a] deriving (Eq, Show, Read)

data City = CityWithCastle
                            { castleOrNot  ::  CastleAndWall
                            , recreation   :: Recreation
                            , buildings    :: NonEmpty House
                            , personAmount :: Int
                            } deriving (Eq, Show, Read)

data ChangedCity =
     ChangedCity {city       :: City
                , wasChanged :: Bool} deriving (Eq, Show, Read)

createCastle :: City -> ChangedCity
createCastle city_in = if castleOrNot city_in == NoCastle
                    then  ChangedCity {city = city_in {castleOrNot = CastleAndWall
                                        { castle = Castle {lord = NoLord}
                                        , wall = NoWall
                                        }}
                                       , wasChanged = True
                    }
                    else ChangedCity {city = city_in, wasChanged = False}
tCreateCastle :: Bool
tCreateCastle =
    let city1 = CityWithCastle {castleOrNot = NoCastle, buildings = House{family = Family{pa = One}}:| [House{family = Family{pa = One}}], personAmount = 1, recreation = None} in
    let city2 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = NoLord}, wall = NoWall}} in
    let a1 = createCastle city1 in
    let a2 = createCastle city2 in
    wasChanged a1 && not (wasChanged a2)

createRecreation :: City -> ChangedCity
createRecreation city_in = if recreation city_in == None
                        then ChangedCity {city = city_in {recreation = Library}, wasChanged = True}
                        else ChangedCity {city = city_in, wasChanged = False}
tCreateRecreation :: Bool
tCreateRecreation =
    let city1 = CityWithCastle {castleOrNot = NoCastle, buildings = House{family = Family{pa = One}}:| [House{family = Family{pa = One}}], personAmount = 0, recreation = None} in
    let city2 = city1 {recreation = Library} in
    let a1 = createRecreation city1 in
    let a2 = createRecreation city2 in
    wasChanged a1 && not (wasChanged a2)

createHouse :: City -> Family -> City
createHouse (CityWithCastle a b (h1 :| hm) c) family_ = CityWithCastle a b  (h1 :| (House{family = family_} : hm)) (c + fromEnum (pa family_))

runLordRun :: City -> Lord ->  Either String City
runLordRun city_ lord_
    | castleOrNot city_ == NoCastle = Left "No castle"
    | lord (castle (castleOrNot city_)) == NoLord =
      Right
        city_ {castleOrNot =
               CastleAndWall{castle = Castle{lord = lord_},
                             wall = wall (castleOrNot city_)}}
    | otherwise = Left "Already have a lord"
tRunLordRun :: Bool
tRunLordRun =
    let city1 = CityWithCastle {castleOrNot = NoCastle,buildings = House{family = Family{pa = One}}:| [House{family = Family{pa = One}}], personAmount = 0, recreation = None} in
    let city2 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = NoLord}, wall = NoWall}} in
    let city3 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = IsLord}, wall = NoWall}} in
    let a1 = runLordRun city1 IsLord in
    let a2 = runLordRun city2 IsLord in
    let a3 = runLordRun city3 IsLord in
    a1 == Left "No castle"
    && a2 == Right city1 {castleOrNot = CastleAndWall {castle = Castle {lord = IsLord}, wall = NoWall}}
    && a3 == Left "Already have a lord"



createWall :: City -> ChangedCity
createWall city_in = if castleOrNot city_in /= NoCastle &&
                       lord (castle (castleOrNot city_in)) == IsLord &&
                       personAmount city_in >= 10
                  then ChangedCity {
                                    city = city_in {castleOrNot = CastleAndWall {wall = IsWall
                                                         , castle = castle (castleOrNot city_in)}}
                                    , wasChanged = True}
                  else  ChangedCity {city = city_in, wasChanged = False}
tCreateWall :: Bool
tCreateWall =
    let city1 = CityWithCastle {castleOrNot = NoCastle, buildings = House{family = Family{pa = One}}:| [House{family = Family{pa = One}}], personAmount = 0, recreation = None} in
    let city2 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = NoLord}, wall = NoWall}} in
    let city3 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = IsLord}, wall = NoWall}} in
    let city4 = city1 {castleOrNot = CastleAndWall {castle = Castle {lord = IsLord}, wall = NoWall}} in
    let city5 = createHouse city4 Family {pa = Four} in
    let city6 = createHouse city5 Family {pa = Four} in
    let city7 = createHouse city6 Family {pa = Four} in
    let a1 = createWall city1 in
    let a2 = createWall city2 in
    let a3 = createWall city3 in
    let a4 = createWall city7 in
    wasChanged a4 && not (wasChanged a1) && not (wasChanged a2) && not (wasChanged a3)




createCity :: City
createCity = CityWithCastle { castleOrNot = NoCastle
                            , recreation = None
                            , buildings = House{family = Family{pa = One}}:| [House{family = Family{pa = One}}]
                            , personAmount = 0
                            }

-- 3.3------------------------------------------------------------

data Nat = Z | S Nat

numToNat :: (Num t, Eq t) => t -> Nat
numToNat n = let makeFromNum num nat_n = case num of
                                       0 -> nat_n
                                       x -> S (makeFromNum (x - 1) nat_n)
                                       in makeFromNum n Z


natToNum :: Num t => Nat -> t -> t
natToNum nat_n n = case nat_n of
    Z   -> (+) 0 n
    S x -> natToNum x ((+) n 1)


instance Show Nat where
    show a = show (natToNum a (0::Int))

add :: Nat -> Nat -> Nat
add x y = case y of
    Z    -> x
    S yy -> S (add x yy)

addNum :: (Eq t1, Eq t2, Num t1, Num t2) => t1 -> t2 -> Nat
addNum x y = add (numToNat x) (numToNat y)
tAdd :: (Eq a, Num a) => a -> a -> Bool
tAdd x y = natToNum (addNum x y) 0 == x + y

sub :: Nat -> Nat -> Nat
sub x y = case (x, y) of
    (xx, Z)      -> xx
    (Z, _)       -> Z
    (S xx, S yy) -> sub xx yy
tSub :: (Eq a, Num a) => a -> a -> Bool
tSub x y = natToNum (sub (numToNat x) (numToNat y)) 0 == x - y


mul :: Nat -> Nat -> Nat
mul x y = case (x, y) of
    (_, Z)     -> Z
    (Z, _)     -> Z
    (xx, S yy) -> add (mul xx yy) xx
tMul :: (Eq a, Num a) => a -> a -> Bool
tMul x y = natToNum (mul (numToNat x) (numToNat y)) 0 == x * y


comp :: Nat -> Nat -> Ordering
comp Z Z         = EQ
comp _ Z         = GT
comp Z _         = LT
comp (S a) (S b) = comp a b

instance Ord Nat where
  compare :: Nat -> Nat -> Ordering
  compare = comp

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  x == y = comp x y == EQ

isEven :: Nat -> Bool
isEven a = even (natToNum a (0::Int))

tComp :: (Num a, Ord a) => a -> a -> Bool
tComp x y = ((numToNat x < numToNat y) == (x < y)) && ((numToNat x > numToNat y) == (x > y)) && ((numToNat x == numToNat y) == (x == y))

tIsEven :: Integral a => a -> Bool
tIsEven x = isEven (numToNat x) == even x

t_3_3 :: Integral a => a -> a -> Bool
t_3_3 x y = tAdd x y && tSub x y && tMul x y && tComp x y && tIsEven x && tIsEven y

-- 3.4------------------------------------------------------------


data Tree a = Leaf |  Node [a] (Tree a) (Tree a)  deriving (Show, Read)

isEmpty :: Tree a -> Bool
isEmpty tree = case tree of
                Leaf -> True
                _    -> False
tIsEmpty :: Bool
tIsEmpty = isEmpty Leaf && not (isEmpty (Node [1::Int] Leaf Leaf))

size :: Tree a -> Int
size tree = case tree of
    Leaf         -> 0
    (Node c a b) -> length c + size a + size b
tSize :: Bool
tSize =
    0 == size Leaf
    && 1 == size (Node [1::Int] Leaf Leaf)
    && 2 == size (Node [2::Int] (Node [1] Leaf Leaf) Leaf)

findTree :: (Ord a) => Tree a -> a -> Bool
findTree Leaf _ = False
findTree (Node v t1 t2) x
    | x == head v = True
    | x < head v = findTree t1 x
    | otherwise = findTree t2 x
tFindTree :: Bool
tFindTree =
    let t = Node [2] (Node [1] Leaf Leaf) Leaf in
    findTree t (1::Int) && not (findTree t 3) && findTree t 2


insertTree :: (Ord a) => Tree a -> a -> Tree a
insertTree Leaf x = Node [x] Leaf Leaf
insertTree (Node v t1 t2) x
    | x == head v = Node (x:v) t1 t2
    | x > head v = Node v t1 (insertTree t2 x)
    | otherwise = Node v (insertTree t1 x) t2
tInsertTree :: Bool
tInsertTree =
    let t = insertTree (Node [2] Leaf Leaf) 1 in
    findTree t (1::Int) && not (findTree t 3)



deleteTree :: (Ord a) => Tree a -> a -> Tree a
deleteTree Leaf _ = Leaf
deleteTree (Node v t1 t2) x
    | x == head v = if length v > 1 then Node (replicate (length v - 1) (head v)) t1 t2 else deleteX (Node v t1 t2)
    | x < head v = Node v (deleteTree t1 x) t2
    | otherwise = Node v t1 (deleteTree t2 x)
tDeleteTree :: Bool
tDeleteTree =
    let t = deleteTree (Node [2] (Node [1] Leaf Leaf) Leaf) 1 in
    findTree t (2::Int) && not (findTree t 1)



deleteX :: (Ord a) => Tree a -> Tree a
deleteX (Node _ Leaf t2) = t2
deleteX (Node _ t1 Leaf) = t1
deleteX (Node _ t1 t2)   = Node (leftistElement t2) t1 t2
deleteX Leaf             = Leaf


leftistElement :: (Ord a) => Tree a -> [a]
leftistElement (Node v Leaf _) = v
leftistElement (Node _ t1 _)   = leftistElement t1
leftistElement Leaf            = []

-- -- 4.------------------------------------------------------------
data Pair a = Pair a a
instance Foldable Pair where
    foldMap f (Pair a b) =  f a `mappend` f b
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f acc (Pair a b) = f a (f b acc)
tPairFoldr :: (Eq a, Num a) => a -> Bool
tPairFoldr a = foldl (+) 0 (Pair a a) == a + a

instance Foldable NonEmpty where
    foldMap f (s :| l) = foldl mappend (f s) (map f l)
    foldr f acc (s :| l) = f s (foldr f acc l)
tNonEmptyFoldr :: (Eq a, Num a) => NonEmpty a -> Bool
tNonEmptyFoldr (a:|l) = foldl (+) 0 (a:|l) == (a + sum l)

splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn del =  foldr (\d (x:xs) -> if d == del then []:x:xs else (d:x):xs) [[]]
tSplitOn :: Bool
tSplitOn  = let del = '/' in
            let str = "path/to/file" in
            splitOn del str == ["path", "to", "file"]

instance Foldable Tree where
   foldr _ z Leaf         = z
   foldr f z (Node v l r) = foldr f (foldr f (foldr f z r) v) l
   foldMap _ Leaf = mempty
   foldMap f (Node v l r) =  foldl mappend (foldMap f l) (map f v) `mappend` foldMap f r




-- -- 5.------------------------------------------------------------
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat l = case l of
    (Just x : xs) -> x ++ maybeConcat xs
    (Nothing: xs) -> maybeConcat xs
    []            -> []

tMaybeConcat :: Bool
tMaybeConcat = let t = [Just[1, 4, 5], Just[1], Nothing, Just[]] in
    maybeConcat t == [1::Int, 4, 5, 1]

data ThisOrThat a b = This a | That b | Both a b

instance (Monoid a, Monoid b) => Semigroup (ThisOrThat a b) where
    This a <> This b = This $ mappend a b
    That a <> That b = That (mappend a b)
    This a <> That b = Both a b
    That a <> This b = Both b a
    This a <> Both c d = Both (mappend a c) d
    Both c d <> This a = Both (mappend c a) d
    That a <> Both c d = Both c (mappend a d)
    Both c d <> That a = Both c (mappend d a)
    Both a b <> Both c d = Both (mappend a c) (mappend b d)


instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    (a :| as) <> (b :| bs) = a :| (as ++ b : bs)

-- turn to One_ cause already have constructor One
data Builder = One_ Char | Many [Builder]

instance Semigroup Builder where
    (<>) :: Builder -> Builder -> Builder
    Many [] <> b = b
    a <> Many [] = a -- todo is redundant
    f <> s = Many [f, s]



instance Monoid Builder where
    mempty :: Builder
    mempty = Many []
    mappend :: Builder -> Builder -> Builder
    mappend = (<>)


fromString :: String -> Builder
fromString []  = Many []
fromString [a] = One_ a
fromString l   = Many (map One_ l)


foldFun  :: Builder -> String ->String
foldFun (One_ a) b =  a : b
foldFun (Many a) b = foldr foldFun b a

toString :: Builder -> String
toString a = foldFun a ""
tFromToString :: String -> Bool
tFromToString a = toString (fromString a) == a

tAll :: Bool
tAll = t_1 && tFromToString "hdsjh" && tMaybeConcat && tSplitOn && tDeleteTree && tInsertTree && tFindTree && tSize && tIsEmpty && t_3_3 (4::Int) 2 && t_3_1