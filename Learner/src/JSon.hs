{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module JSon (
    JSON (..),
    JSONLattice (..),
    lazyObject,
    compress,
    equivalanceClasses,
    equivalenceRelation,
    indexJSONLattice,
    indexJSONLatticeReffed,
    refinedEquivalenceRelation,
    exampleJSONLat,
    indexOracles,
    indexJSON,
    nuleq,
    fromJSON
    )
    where 

import Lattice 
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S
import Object
import List 
import Float
import String
import qualified Data.Text as Text 
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Control.Lens
import qualified Data.Ratio as Rat
import qualified Data.Scientific as Sci
import qualified Data.Vector as Vec 


import qualified Test.QuickCheck.Gen as Gen


isJust :: Maybe a -> Bool
isJust Nothing = False 
isJust _ = True

type UUID = Int

data JSON = N Float
    | J (M.Map String JSON)
    | L [JSON]
    | S String
    | Null
    deriving (Show, Eq)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

instance A.ToJSON JSON where
    toJSON (N num) = A.toJSON num 
    toJSON (J object) = A.toJSON $ HashMap.fromList $ map (mapFst Text.pack) $ M.toList $ M.map A.toJSON object
    toJSON (L l) = A.toJSON l 
    toJSON (S str) = A.toJSON str
    toJSON Null = A.Null


fromJSON (A.String text) = S $ Text.unpack text
fromJSON (A.Object obj) = J $ M.fromList $ map (mapFst Text.unpack) $ HashMap.toList $ HashMap.map fromJSON obj
fromJSON (A.Number s) = N $ Sci.toRealFloat s
fromJSON (A.Array v) = L $ Vec.toList $ fmap fromJSON v
fromJSON A.Null = Null 

data JSONLattice = JSONNumberLattice Range 
    | JSONObjectLattice (ObjectLattice JSONLattice) 
    | JSONListLattice (ListLattice JSONLattice) 
    | JSONStringLattice StringLattice
    | ForwardRef UUID
    | ForwardStringRef String
    | Nullable JSONLattice
    | AnyOf [JSONLattice]
    | JustNull
    deriving (Eq, Ord, Show)

exampleJSONLat :: JSONLattice -> Gen.Gen JSON 
exampleJSONLat (JSONStringLattice s) = fmap S $ latticeExample s
exampleJSONLat (JSONNumberLattice r) = fmap N $ latticeExample r
exampleJSONLat (JSONObjectLattice o) = fmap J $ latticeExample o
exampleJSONLat (JSONListLattice l) = fmap L $ latticeExample l 
exampleJSONLat (Nullable l) = Gen.oneof [return Null, exampleJSONLat l]
exampleJSONLat (JustNull) = return Null
exampleJSONLat (AnyOf children) = Gen.oneof $ map exampleJSONLat children
exampleJSONLat o = error $ show o

checkNullability :: (JSON -> IO Bool) -> JSONLattice -> IO JSONLattice
checkNullability o l = do 
    b <- o Null
    return (if b then Nullable l else l)

growJSONLattice :: JSON -> (JSON -> IO Bool) -> IO JSONLattice
growJSONLattice (N f) o = (checkNullability o) =<< (fmap JSONNumberLattice $ grow f (o . N))
growJSONLattice (J obj) o = (checkNullability o) =<< (fmap JSONObjectLattice $ grow obj (o . J))
growJSONLattice (L xs) o = (checkNullability o) =<< (fmap JSONListLattice $ grow xs (o . L))
growJSONLattice (S s) o = (checkNullability o) =<< (fmap JSONStringLattice $ grow s (o . S))
growJSONLattice Null o = return JustNull

jsonAccepted :: JSONLattice -> JSON -> Bool 
jsonAccepted (JSONNumberLattice rl) (N f) = accepted rl f 
jsonAccepted (JSONObjectLattice objl) (J o) = accepted objl o 
jsonAccepted (JSONListLattice ll) (L l) = accepted ll l
jsonAccepted JustNull o = o == Null 
jsonAccepted (Nullable l) j = accepted l j 
jsonAccepted (JSONStringLattice sl) (S string) = accepted sl string
jsonAccepted v w = False

instance Lattice JSONLattice JSON where 
    grow = growJSONLattice
    accepted = jsonAccepted
    latticeExample = exampleJSONLat

exampleObj :: JSON 
exampleObj = J $ M.fromList [ ("firstName", N 20) ]

oracle :: JSON -> Bool 
oracle (J m) = (case (m M.! "b") of 
    N num -> num <= 76
    _ -> False) && oracle (m M.! "a")
oracle Null = True
oracle _ = False 

oracleI :: JSON -> IO Bool
oracleI = return . oracle

learned :: IO JSONLattice
learned = grow exampleObj oracleI

recall :: JSONLattice -> [JSON] -> Rat.Ratio Int
recall l xs = truePositives Rat.% relevantExamples
    where 
        truePositives = (length $ filter (accepted l) xs)
        relevantExamples = (length xs)


indexJSON :: Int -> M.Map Int JSON -> JSON -> (Int, M.Map Int JSON)
indexJSON n m j@(J obj) = (cn + 1, M.insert cn j cm)
    where 
        (cn, cm) = foldl (\(n', m') j' -> indexJSON n' m' j') (n, m) obj
indexJSON n m j@(L (x:xs)) = (cn+1, M.insert cn j cm)
    where 
        (cn, cm) = (indexJSON n m x)
indexJSON n m j = (n + 1, M.insert n j m) 

type Oracle a = a -> IO Bool

indexOracles :: Int -> M.Map Int (Oracle JSON) -> Oracle JSON -> JSON -> (Int, M.Map Int (Oracle JSON))
indexOracles n m o j@(J obj) = (cn + 1, M.insert cn o cm)
    where 
        (cn, cm) = M.foldlWithKey (\(n', m') k' j' -> indexOracles n' m' (\v -> o $ J $ M.insert k' v obj) j') (n, m) obj
indexOracles n m o j@(L (x:xs)) = (cn+1, M.insert cn o cm)
    where 
        (cn, cm) = indexOracles n m (\v -> o $ L $ v:xs) x 
indexOracles n m o j = (n+1, M.insert n o m)


indexJSONLattice :: Int -> M.Map Int JSONLattice -> JSONLattice -> (Int, M.Map Int JSONLattice)
indexJSONLattice n m jl@(JSONObjectLattice (ObjectLattice l)) = (cn + 1, M.insert cn jl cm)
    where 
        (cn, cm) = foldl (\(n', m') j' -> indexJSONLattice n' m' j') (n, m) l
indexJSONLattice n m jl@(Nullable l) = (cn, M.insert (cn-1) jl cm)
    where 
        (cn, cm) = indexJSONLattice n m l
indexJSONLattice n m jl@(JSONListLattice List.Empty) = (n + 1, M.insert n jl m)
indexJSONLattice n m jl@(JSONListLattice (List.Any x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (indexJSONLattice n m x)
indexJSONLattice n m jl@(JSONListLattice (NonEmpty x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (indexJSONLattice n m x)
indexJSONLattice n m l = (n + 1, M.insert n l m)

indexJSONLatticeReffed :: Int -> M.Map Int JSONLattice -> JSONLattice -> (Int, M.Map Int JSONLattice)
indexJSONLatticeReffed n m jl@(JSONObjectLattice (ObjectLattice l)) = (cn + 1, M.insert cn (JSONObjectLattice (ObjectLattice creffedObj)) cm)
    where 
        ((cn, cm), creffedObj)= M.foldlWithKey (\((n', m'), reffedObj) k j' -> (indexJSONLatticeReffed n' m' j', M.insert k (ForwardRef (n')) reffedObj)) ((n, m), M.empty) l
indexJSONLatticeReffed n m jl@(Nullable l) = (cn, M.insert (cn-1) jl cm)
    where 
        (cn, cm) = indexJSONLatticeReffed n m l
indexJSONLatticeReffed n m jl@(JSONListLattice List.Empty) = (n + 1, M.insert n jl m)
indexJSONLatticeReffed n m jl@(JSONListLattice (List.Any x)) = (cn + 1, M.insert cn (JSONListLattice (List.Any (ForwardRef (cn-1)))) cm)
    where
        (cn, cm) = (indexJSONLatticeReffed n m x)
indexJSONLatticeReffed n m jl@(JSONListLattice (NonEmpty x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (indexJSONLatticeReffed n m x)
indexJSONLatticeReffed n m l = (n + 1, M.insert n l m)

sim :: Int -> (JSONLattice, JSON -> IO Bool) -> (JSONLattice, JSON -> IO Bool) -> IO Bool
sim n (al, ao) (bl, bo) = do 
    aexs <- sequence $ map Gen.generate $ [exampleJSONLat al | _ <- [1..n]]
    bexs <- sequence $ map Gen.generate $ [exampleJSONLat bl | _ <- [1..n]]

    alla <- fmap and $ sequence $ map bo aexs
    allb <- fmap and $ sequence $ map ao bexs 
    return (alla && allb)

-- compressPrime ::

compress :: Int -> (Int -> Int) -> M.Map Int JSONLattice -> JSONLattice  -> (Int, M.Map Int JSONLattice)
compress n f m jl@(JSONObjectLattice (ObjectLattice l)) = (cn + 1, M.insert cn (JSONObjectLattice $ ObjectLattice $ cjl) cm)
    where 
        tuck :: (Int, b) -> (Int -> c) -> (Int, b, c)
        tuck (a, b) f = (a, b, f a)
        (cn, cm, cjl) = M.foldlWithKey (\(n', m', jl') k j' -> tuck (compress n' f m' j') (\n'' -> M.insert k (ForwardRef (n'' - 1)) jl')) (n, m, M.empty) l
compress n f m jl@(Nullable l) = (cn, M.adjust Nullable (cn-1) cm)
    where 
        (cn, cm) = compress n f m l
compress n f m jl@(JSONListLattice List.Empty) = (n + 1, M.insert n jl m)
compress n f m jl@(JSONListLattice (List.Any x)) = (cn + 1, M.insert cn (if f cn == cn then (JSONListLattice $ List.Any $ ForwardRef $ f (cn - 1)) else ForwardRef $ f cn) cm)
    where
        (cn, cm) = (compress n f m x)
compress n f m jl@(JSONListLattice (NonEmpty x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (compress n f m x)
compress n f m l = (n + 1, M.insert n (if f n == n then l else ForwardRef $ f n) m)

equivalenceRelation :: (Ord a) => (b -> b -> Bool) -> M.Map a b -> (a -> a -> Bool)
equivalenceRelation e m = (\a b -> (m M.! a) `e` (m M.! b))

refinedEquivalenceRelation :: M.Map Int (Oracle JSON) -> M.Map Int JSON -> (Int -> Int -> Bool) -> (Int -> Int -> IO Bool)
refinedEquivalenceRelation oi ji eq  = \ a b -> if eq a b 
    then do 
        l <- (oi M.! a) (ji M.! b) 
        r <- (oi M.! a) (ji M.! b) 
        return $ l && r
    else return $ False 


equivalanceClasses :: (Ord a) => [(a, a)] -> (a -> a) 
equivalanceClasses xs = (\ v -> case M.lookup v $ (foldl (\m (a, b) -> M.insert b a m) M.empty) $ filter (\(a, b) -> a > b) xs of 
    Just c -> c 
    Nothing -> v)

nuleq :: JSONLattice -> JSONLattice -> Bool 
nuleq (Nullable a) JustNull = True 
nuleq JustNull (Nullable a) = True
nuleq JustNull JustNull = False 
nuleq a b = a == b

lazyObject :: M.Map Int JSONLattice -> JSONLattice -> JSONLattice
lazyObject m (JSONObjectLattice (ObjectLattice l)) = JSONObjectLattice $ ObjectLattice $ M.map (lazyObject m) l 
lazyObject m (JSONListLattice (List.Any l)) = JSONListLattice $ List.Any $ lazyObject m l 
lazyObject m (JSONListLattice (NonEmpty l)) = JSONListLattice $ NonEmpty $ lazyObject m l 
lazyObject m (Nullable l) = Nullable $ lazyObject m l
lazyObject m (ForwardRef ref) = case M.lookup ref m of 
    Just v -> lazyObject m v 
    Nothing -> error $ (show m ) ++ (show ref)
lazyObject m (AnyOf xs) = AnyOf $ map (lazyObject m) xs
lazyObject _ o = o
