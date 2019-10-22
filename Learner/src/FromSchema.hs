{-# LANGUAGE OverloadedStrings #-}

module FromSchema (
    mLatticeGenerator
    ) where 

import Debug.Trace
import JSon
import Float
import String
import List
import Object
import Lattice 
import Control.Monad (join)
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Test.QuickCheck.Gen as Gen
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Text as Text
import qualified Data.Vector as Vec 
import Data.List (nub)

convert = BS.pack . map BSI.c2w

convertToLazyLattice :: A.Value -> Maybe JSONLattice
convertToLazyLattice rvalue = do 
    justRawLattice <- rawLattice
    justDefinitions <- definitions
    let (intDefinitions, strictLattice) = convertStringRefs justDefinitions justRawLattice
    return $ lazyObject intDefinitions strictLattice
    where 
        createLattice :: A.Value -> Maybe JSONLattice
        createLattice (A.Object m) = case HM.lookup "type" m of 
            (Just (A.String "number")) -> return $ JSONNumberLattice (Interval (0) 120)
            (Just (A.String "string")) -> return $ JSONStringLattice String.Any
            (Just (A.String "array")) -> case HM.lookup "items" m of 
                (Just obj) -> fmap (JSONListLattice . List.Any) $ createLattice obj
            (Just (A.String "object")) -> case HM.lookup "properties" m of 
                (Just (A.Object obj)) -> fmap (JSONObjectLattice . ObjectLattice) $ sequence $ HM.foldrWithKey (\k v -> M.insert (Text.unpack k) v) M.empty $ HM.map createLattice obj 
            Nothing -> case HM.lookup "$ref" m of 
                (Just (A.String ref)) -> return $ ForwardStringRef $ Text.unpack ref
                Nothing -> case HM.lookup "anyOf" m of 
                    (Just (A.Array items)) -> fmap AnyOf $ sequence $ map createLattice $ Vec.toList items
                    Nothing -> error $ show m
            _ -> error $ "Unknown" ++ show m
        createLattice (A.Null) = return $ JustNull
        createLattice _ = error "ya yeet?" --Nothing

        rawLattice = createLattice rvalue

        getDefinitions :: A.Value -> Maybe (M.Map String JSONLattice)
        getDefinitions (A.Object obj) = case HM.lookup "definitions" obj of 
            (Just (A.Object m)) -> sequence $ HM.foldrWithKey (\k v -> M.insert ("#/definitions/" ++ Text.unpack k) v) M.empty $ HM.map createLattice m
            _ -> Just M.empty
        getDefinitions _ = Nothing

        definitions = getDefinitions rvalue

        convertStringRefs :: M.Map String JSONLattice -> JSONLattice -> (M.Map Int JSONLattice, JSONLattice)
        convertStringRefs defs lattice = (intDefinitions, strToIntRefs lattice)
            where 
                getStringRefs :: JSONLattice -> [String]
                getStringRefs (JSONObjectLattice (ObjectLattice o)) = concatMap getStringRefs $ M.elems o
                getStringRefs (JSONListLattice (List.Any l)) = getStringRefs l
                getStringRefs (JSONListLattice (List.NonEmpty l)) = getStringRefs l 
                getStringRefs (ForwardStringRef ref) = return ref 
                getStringRefs (Nullable l) = getStringRefs l 
                getStringRefs (AnyOf l) = concatMap getStringRefs l
                getStringRefs _ = [] 

                stringRefs :: [String]
                stringRefs = nub (mainrefs ++ defrefs)
                    where 
                        defrefs = concat $ M.elems $ M.map getStringRefs defs 
                        mainrefs = getStringRefs lattice

                strToIntMap = M.fromList $ zip stringRefs [0..]

                strToIntRefs :: JSONLattice -> JSONLattice
                strToIntRefs (JSONObjectLattice (ObjectLattice o)) = JSONObjectLattice $ ObjectLattice $ M.map strToIntRefs o 
                strToIntRefs (JSONListLattice (List.Any l)) = JSONListLattice $ List.Any $ strToIntRefs l 
                strToIntRefs (JSONListLattice (List.NonEmpty l)) = JSONListLattice $ List.NonEmpty $ strToIntRefs l
                strToIntRefs (ForwardStringRef ref) = ForwardRef (strToIntMap M.! ref )
                strToIntRefs (Nullable l) = Nullable $ strToIntRefs l 
                strToIntRefs (AnyOf l) = AnyOf $ map strToIntRefs l 
                strToIntRefs a = a 

                intDefinitions = M.mapKeys (strToIntMap M.!) $ M.map strToIntRefs $ defs


fromSchema :: String -> Maybe JSONLattice 
fromSchema = join . fmap convertToLazyLattice . A.decode . convert

mLatticeGenerator :: String -> Maybe (Gen.Gen JSON)
mLatticeGenerator s = do 
    lattice <- fromSchema s
    return $ exampleJSONLat lattice

main :: IO ()
main = do 
    putStrLn $ show $ fromSchema $ traceShowId "{\"type\":\"number\"}"
