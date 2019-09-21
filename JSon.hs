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

type UUID = Int

data JSON = N Float
    | J (M.Map String JSON)
    | L [JSON]
    | Null
    deriving (Show)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

instance A.ToJSON JSON where
    toJSON (N num) = A.toJSON num 
    toJSON (J object) = A.toJSON $ HashMap.fromList $ map (mapFst Text.pack) $ M.toList $ M.map A.toJSON object
    toJSON (L l) = A.toJSON l 
    toJSON Null = A.Null

data JSONLattice = JSONNumberLattice Range 
    | JSONObjectLattice (ObjectLattice JSONLattice) 
    | JSONListLattice (ListLattice JSONLattice) 
    | ForwardRef UUID
    | Nullable JSONLattice
    | JustNull
    deriving (Eq, Ord, Show)

checkNullability :: (JSON -> Bool) -> JSONLattice -> JSONLattice
checkNullability o = (if o Null
    then Nullable
    else id) 

growJSONLattice :: JSON -> (JSON -> Bool) -> JSONLattice
growJSONLattice (N f) o = checkNullability o $ JSONNumberLattice $ grow f (o . N)
growJSONLattice (J obj) o = checkNullability o $ JSONObjectLattice $ grow obj (o . J)
growJSONLattice (L xs) o = checkNullability o $ JSONListLattice $ grow xs (o . L)
growJSONLattice Null o = JustNull


instance Lattice JSONLattice JSON where 
    grow = growJSONLattice

example :: JSON 
example = J $ M.fromList [
    ("a", J $ M.fromList [
        ("a", Null),
        ("b", N 54)
        ]),
    ("b", N 32)
    ]

oracle :: JSON -> Bool 
oracle (J m) = (case (m M.! "b") of 
    N num -> num <= 76
    _ -> False) && oracle (m M.! "a")
oracle Null = True
oracle _ = False 

objects :: JSONLattice -> [JSONLattice]
objects (JSONListLattice lat) = case lat of 
    Empty -> [] 
    NonEmpty l -> objects l 
    Any l -> objects l
objects jol@(JSONObjectLattice (ObjectLattice lat)) =  jol : (concatMap objects $ M.elems lat)
objects (Nullable a) = objects a
objects _ = []

indexer :: UUID -> M.Map UUID JSON -> JSON -> (UUID, M.Map UUID JSON)
indexer id m a@(N n) = ((id+1), M.insert (id+1) a m)
indexer id m a@(J o) = ((id'+1), M.insert (id'+1) a m')
    where
        (id', m') = M.foldrWithKey (\k v (id, m) -> indexer id m v) (id, m) o 
indexer id m a@Null = (id+1, M.insert (id+1) a m)

foiler :: (JSON -> Bool) -> UUID -> M.Map UUID (JSON -> Bool) -> JSON -> (UUID, M.Map UUID (JSON -> Bool))
foiler o id m a@(N n) = ((id+1), M.insert (id+1) o m)
foiler o id m a@(J obj) = ((id'+1), M.insert (id'+1) o m')
    where
        (id', m') = M.foldrWithKey (\k v (id, m) -> foiler (o . (\t -> J $ M.insert k t obj)) id m v) (id, m) obj 
foiler o id m a@Null = (id+1, M.insert (id+1) o m)

types :: UUID -> M.Map UUID JSONLattice -> JSONLattice -> (UUID, M.Map UUID JSONLattice)
types id m num@(JSONNumberLattice r) =  ((id+1), M.insert (id+1) num m)
types uuid m (JSONObjectLattice (ObjectLattice obj)) = ((uuid'+1), M.insert (uuid'+1) (JSONObjectLattice wrappedRefs) m')
    where
        c2to3 :: (a, b) -> c -> (a, b, c)
        c2to3 (a, b) c = (a, b, c)
        (uuid', m', ktu) = M.foldrWithKey (\k v (uuid, m, xs) -> let 
                (newUUID, newMap) = (types uuid m v)
                in
                    c2to3 (newUUID, newMap) ((k, newUUID):xs)) 
            (uuid, m, []) obj
        wrappedRefs = ObjectLattice $ M.fromList $ map (\(k, uuid) -> (k, ForwardRef uuid)) ktu
types uuid m (Nullable l) = (uuid', M.adjust Nullable uuid' m')
    where 
        (uuid', m') = types uuid m l 
types uuid m (JustNull) = (uuid+1, M.insert (uuid+1) JustNull m)

display :: JSONLattice -> String 
display (JSONNumberLattice (Interval l r)) = printf "(%f, %f)" l r 
display (JSONObjectLattice (ObjectLattice m)) = concatMap (\(k, ForwardRef r) -> printf "%s: t%d" k r)  $ M.toList m
display (Nullable l) = printf "Maybe %s" $ display l
display (ForwardRef d) = printf "t%d" d
display (JustNull) = printf "null"

prettyDisplay :: (UUID, M.Map UUID JSONLattice) -> IO () 
prettyDisplay (uuid, m) = do 
    putStrLn $ printf "t%d where: " uuid
    foldr1 (>>) $ map (putStrLn . \(k, t) -> printf "t%d := %s" k (display t)) $ M.toList m

removeDuplicates :: (UUID, M.Map UUID JSONLattice) -> (UUID, M.Map UUID JSONLattice)
removeDuplicates (uuid, m) = (uuid, M.mapWithKey (\k v -> let 
    uniqueKey = (reversedMap M.! v) 
    in
        if uniqueKey == k 
            then v 
            else (ForwardRef uniqueKey)) m)
    where 
        reversedMap = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList m 

equivalenceRelation :: JSON -> (JSON -> Bool) -> (UUID, M.Map UUID JSONLattice) -> [(UUID, UUID)]
equivalenceRelation ex o (_, m) = [(fst a, fst b) | a <- typeList, b <- typeList, sim a b]
    where 
        (_, foiledMap) = foiler o 0 M.empty ex 
        (_, exampleMap) = indexer 0 M.empty ex 
        typeList = M.toList m
        sim (id1, v1) (id2, v2) 
            | v1 == v2 = True 
            | (foiledMap M.! id1) (exampleMap M.! id2) = True
            | otherwise = False

-- | this ought to be checked
mapToEquivalanceClass :: (Ord a, Ord b) => M.Map a b -> M.Map a a
mapToEquivalanceClass m = M.map (\v -> pseudoInverse M.! v) m 
    where 
        pseudoInverse = M.fromList $ map (\(a, b) -> (b, a)) $ M.toList m 

setOfClasses :: (Ord a) => M.Map a a -> S.Set a 
setOfClasses = S.fromList . M.elems 

simplyWithEquivalenceClass :: M.Map UUID UUID -> (UUID, M.Map UUID JSONLattice) -> (UUID, M.Map UUID JSONLattice)
simplyWithEquivalenceClass ec (uuid, ts) = (uuid, tightMap)
    where 
        simplify :: JSONLattice -> JSONLattice
        simplify (JSONObjectLattice (ObjectLattice m)) = JSONObjectLattice $ ObjectLattice $ fmap simplify m 
        simplify (Nullable l) = Nullable $ simplify l 
        simplify (ForwardRef d) = (ForwardRef (ec M.! d))
        simplify (JustNull) = JustNull
        simplify r = r 

        tightMap = M.map simplify $ M.filterWithKey (\k _ -> S.member k classees)  ts
        classees = setOfClasses ec 
        