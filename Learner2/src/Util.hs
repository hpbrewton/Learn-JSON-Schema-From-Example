module Util (
    orElse,
    slotterVec,
    filterIOHSet,
    mapTextKeys,
    gPartition,
    uniquePairs,
    equivalenceClass
    )
    where 

import qualified Data.Vector as Vec
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

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
