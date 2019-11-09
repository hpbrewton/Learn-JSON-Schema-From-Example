module Util (
    orElse,
    slotterVec,
    filterIOHSet,
    mapTextKeys,
    gPartition,
    uniquePairs,
    equivalenceClass,
    rotations,
    range,
    averge,
    epsilon,
    upperBound,
    lowerBound,
    swap,
    showT,
    evens
    )
    where 

import qualified Data.Vector as Vec
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Scientific



orElse :: Maybe a -> a -> a
orElse (Just a) _ = a 
orElse Nothing a = a

slotterVec :: Vec.Vector a -> Vec.Vector (a -> Vec.Vector a)
slotterVec = Vec.fromList . map (Vec.fromList .) . slotter . Vec.toList
    where
        slotter :: [a] -> [a -> [a]]
        slotter [] = []
        slotter (x:xs) = (flip (:) xs) : (map ((x:).) $ slotter xs)

filterIOHSet :: (T.Text -> IO Bool) -> HS.HashSet T.Text -> IO (HS.HashSet T.Text)
filterIOHSet f s = do 
    pairs <- sequence $ Prelude.map ioTrans $ Prelude.zip list $ Prelude.map f list 
    return $ HS.fromList $ Prelude.map fst $ Prelude.filter snd pairs
    where 
        list = HS.toList s
        ioTrans (a, iob) = do 
            b <- iob
            return (a, b)

mapTextKeys :: (T.Text -> T.Text) -> HM.HashMap T.Text c -> HM.HashMap T.Text c 
mapTextKeys f = HM.foldrWithKey (\k v m -> HM.insert (f k) v m) HM.empty 

gPartition :: (b -> Bool) -> HM.HashMap T.Text b -> (HM.HashMap T.Text b, HM.HashMap T.Text b)
gPartition f = HM.foldrWithKey (\k v (l, r) -> if f v 
    then (HM.insert k v l, r)
    else (l, HM.insert k v r)) (HM.empty, HM.empty)

uniquePairs :: [a] -> [(a, a)]
uniquePairs xs = [(a, b) | a <- xs, b <- xs]

equivalenceClass :: (Foldable f) => (a -> a -> Bool) -> f a -> [[a]]
equivalenceClass r = foldr insertOrNew []
    where 
        -- insertOrNew :: forall a. [[a]] -> a -> [[a]]
        insertOrNew y [] = [[y]]
        insertOrNew y (x:xs) = if all (\z -> r z y && r y z) x 
            then ((y:x):xs)
            else x: insertOrNew y xs

rotations :: Vec.Vector a -> [Vec.Vector a]
rotations vector = [(Vec.slice n (length-n) vector) Vec.++ (Vec.slice 0 n vector) | n <- [0..(length-1)]]
    where 
        length = Vec.length vector

class (Num a, Ord a) => Ranger a where 
    averge :: a -> a -> a 
    epsilon :: a 
    upperBound :: a 
    lowerBound :: a 


instance Ranger Scientific where 
    averge a b = (a+b)/2
    epsilon = 0.005
    upperBound = 9001
    lowerBound = -9001

instance Ranger Int where 
    averge a b = (a+b) `quot` 2 
    epsilon = 1
    upperBound = 10 
    lowerBound = 0

range :: (Ranger a) => (a, a) -> (a, a) -> (a -> IO Bool) -> IO (a, a)
range (lb, rb) (ls, rs) o
    | (ls - lb) <= epsilon && (rb - rs) <= epsilon = return (ls, rs)
    | otherwise = do 
        let lm = lb `averge` ls
        let rm = rb `averge` rs
        keepLb <- o lm 
        keepUb <- o rm 
        if ((lm == lb) || (lm == ls)) && ((rm == rb) || (rm == rs))
            then case (keepLb, keepUb) of 
                (True, True) -> return (lb, rb)
                (True, False) -> return (lb, rs)
                (False, True) -> return (ls, rb)
                (False, False) -> return (ls, rs)
            else case (keepLb, keepUb) of 
                (True, True) -> range (lb, rb) (lm, rm) o
                (True, False) -> range (lb, rm) (lm, rs) o
                (False, True) -> range (lm, rb) (ls, rm) o
                (False, False) -> range (lm, rm) (ls, rs) o

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)



showT :: (Show a) => a -> T.Text 
showT = T.pack . show 

evens :: [a] -> [a]
evens = map snd . filter ((==1) . fst) . zip (cycle [1, 2])