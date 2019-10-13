{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
import Lattice 
import qualified Data.Aeson as A
import qualified Data.Map as M
import qualified Data.Set as S
import Object
import List 
import Float
import qualified Data.Text as Text 
import qualified Data.HashMap.Strict as HashMap
import Text.Printf
import Network.Wreq as Wreq
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
    | Null
    deriving (Show, Eq)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

instance A.ToJSON JSON where
    toJSON (N num) = A.toJSON num 
    toJSON (J object) = A.toJSON $ HashMap.fromList $ map (mapFst Text.pack) $ M.toList $ M.map A.toJSON object
    toJSON (L l) = A.toJSON l 
    toJSON Null = A.Null


fromJSON (A.Object obj) = J $ M.fromList $ map (mapFst Text.unpack) $ HashMap.toList $ HashMap.map fromJSON obj
fromJSON (A.Number s) = N $ Sci.toRealFloat s
fromJSON (A.Array v) = L $ Vec.toList $ fmap fromJSON v
fromJSON A.Null = Null 

data JSONLattice = JSONNumberLattice Range 
    | JSONObjectLattice (ObjectLattice JSONLattice) 
    | JSONListLattice (ListLattice JSONLattice) 
    | ForwardRef UUID
    | Nullable JSONLattice
    | JustNull
    deriving (Eq, Ord, Show)

exampleJSONLat :: JSONLattice -> Gen.Gen JSON 
exampleJSONLat (JSONNumberLattice r) = fmap N $ latticeExample r
exampleJSONLat (JSONObjectLattice o) = fmap J $ latticeExample o
exampleJSONLat (JSONListLattice l) = fmap L $ latticeExample l 
exampleJSONLat (Nullable l) = Gen.oneof [return Null, exampleJSONLat l]
exampleJSONLat (JustNull) = return Null

checkNullability :: (JSON -> IO Bool) -> JSONLattice -> IO JSONLattice
checkNullability o l = do 
    b <- o Null
    return (if b then Nullable l else l)

growJSONLattice :: JSON -> (JSON -> IO Bool) -> IO JSONLattice
growJSONLattice (N f) o = (checkNullability o) =<< (fmap JSONNumberLattice $ grow f (o . N))
growJSONLattice (J obj) o = (checkNullability o) =<< (fmap JSONObjectLattice $ grow obj (o . J))
growJSONLattice (L xs) o = (checkNullability o) =<< (fmap JSONListLattice $ grow xs (o . L))
growJSONLattice Null o = return JustNull

jsonAccepted :: JSONLattice -> JSON -> Bool 
jsonAccepted (JSONNumberLattice rl) (N f) = accepted rl f 
jsonAccepted (JSONObjectLattice objl) (J o) = accepted objl o 
jsonAccepted (JSONListLattice ll) (L l) = accepted ll l
jsonAccepted JustNull o = o == Null 
jsonAccepted (Nullable l) j = accepted l j 

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

oracleN :: JSON -> IO Bool
oracleN json = do 
    resp <- Wreq.post "http://127.0.0.1:8080/verify" (A.toJSON json) 
    putStrLn $ show $ (resp ^. Wreq.responseBody)
    return $ (=="\"true\"") $ show $ (resp ^. Wreq.responseBody)

oracleExamples :: IO [JSON]
oracleExamples = do 
    responses <- sequence $ replicate 100 $ Wreq.get "http://127.0.0.1:8080/generate"
    let successes = map (\(Just s) -> s) $ filter isJust $ map A.decode $ map (\v -> v ^. Wreq.responseBody) responses
    return $ map fromJSON $ successes

grown :: IO JSONLattice
grown = grow exampleObj oracleN

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
indexJSONLattice n m jl@(JSONListLattice (Any x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (indexJSONLattice n m x)
indexJSONLattice n m jl@(JSONListLattice (NonEmpty x)) = (cn + 1, M.insert cn jl cm)
    where
        (cn, cm) = (indexJSONLattice n m x)
indexJSONLattice n m l = (n + 1, M.insert n l m)

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
compress n f m jl@(JSONListLattice (Any x)) = (cn + 1, M.insert cn (if f cn == cn then (JSONListLattice $ Any $ ForwardRef $ f (cn - 1)) else ForwardRef $ f cn) cm)
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

extract3TupleIO :: (a, b, IO c) -> IO (a, b, c)
extract3TupleIO (a, b, cw) = do 
    c <- cw 
    return (a, b, c)

equivalanceClasses :: (Ord a) => [(a, a)] -> (a -> a) 
equivalanceClasses xs = (\ v -> case M.lookup v $ (foldl (\m (a, b) -> M.insert b a m) M.empty) $ filter (\(a, b) -> a > b) xs of 
    Just c -> c 
    Nothing -> v)

nuleq :: JSONLattice -> JSONLattice -> Bool 
nuleq (Nullable a) JustNull = True 
nuleq JustNull (Nullable a) = True
nuleq JustNull JustNull = False 
nuleq a b = a == b

{-
data JSONLattice = JSONNumberLattice Range 
    | JSONObjectLattice (ObjectLattice JSONLattice) 
    | JSONListLattice (ListLattice JSONLattice) 
    | ForwardRef UUID
    | Nullable JSONLattice
    | JustNull
    deriving (Eq, Ord, Show)
-}

lazyObject :: M.Map Int JSONLattice -> JSONLattice -> JSONLattice
lazyObject m (JSONObjectLattice (ObjectLattice l)) = JSONObjectLattice $ ObjectLattice $ M.map (lazyObject m) l 
lazyObject m (JSONListLattice (Any l)) = JSONListLattice $ Any $ lazyObject m l 
lazyObject m (JSONListLattice (NonEmpty l)) = JSONListLattice $ NonEmpty $ lazyObject m l 
lazyObject m (Nullable l) = Nullable $ lazyObject m l
lazyObject m (ForwardRef ref) = lazyObject m (m M.! ref) 
lazyObject _ o = o

data CompressedLattice = CompressedLattice Int (M.Map Int JSONLattice) 
    deriving (Show)

main :: IO ()
main = do 
    l <- grown
    let (_, indexedObjs) = indexJSON 0 M.empty exampleObj
    let (_, indexedOracles) = indexOracles 0 M.empty oracleN exampleObj
    let (n, indexedLatitce) = indexJSONLattice 0 M.empty l
    m <- sequence $ M.intersectionWith ($) indexedOracles indexedObjs
    let ir = equivalenceRelation nuleq indexedLatitce
    let r = refinedEquivalenceRelation indexedOracles indexedObjs ir
    pairs <- sequence $ [extract3TupleIO (i, j, r i j) | i <- [0..(n-1)], j <- [0 .. (n-1)]]
    let er = map (\(a, b, _) -> (a, b)) $ filter (\(_, _, b) -> b) pairs
    let ec = equivalanceClasses $ er
    let (top, compressed) = compress 0 ec M.empty l
    putStrLn $ show l
    examples <- Gen.sample' $ ((latticeExample (lazyObject compressed (compressed M.! (top-1)))) :: Gen.Gen JSON)
    results <- fmap (filter id) $ sequence $ map oracleN examples
    putStrLn $ show $ length results
    putStrLn $ show $ length examples
    oExamples <- oracleExamples
    let lo = (lazyObject compressed (compressed M.! (top-1)))
    putStrLn $ show $ length $ filter (not . accepted lo) oExamples

    
