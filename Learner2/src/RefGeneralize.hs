{-# LANGUAGE OverloadedStrings #-}
module RefGeneralize (
    refer
) where

import Schema
import Util
import Data.Aeson
import qualified Data.Text as Text
import Data.Scientific
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Test.QuickCheck as Gen
import qualified Data.Vector as Vec
import Generalizer

import Debug.Trace

sep :: Text.Text 
sep = "."

pushRef :: Text.Text -> Schema -> Schema
pushRef prefix (DefinitionSchema top submap) = DefinitionSchema (prefix <> top) $ fmap (pushRef prefix) $ mapTextKeys (prefix <>) submap
pushRef prefix (RefSchema ref) = RefSchema (prefix <> ref)
pushRef prefix (TupleSchema vec) = TupleSchema $ fmap (pushRef prefix) vec 
pushRef prefix (ObjectSchema object required) = ObjectSchema (fmap (pushRef prefix) object) required
pushRef prefix (ArraySchema schema lb ub) = ArraySchema (pushRef prefix schema) lb ub
pushRef prefix (SchemaWithOracle schema oracle) = SchemaWithOracle (pushRef prefix schema) oracle
pushRef prefix (AnyOf schemas) = AnyOf $ fmap (pushRef prefix) schemas
pushRef prefix other = other 

-- always returns a DefinitionSchema, 
-- also should always be used with bottom up such that children are definition schema first
refer :: Generalizer
refer StringSchema _ = return $ DefinitionSchema (sep<>"T") $ HM.singleton (sep<>"T") StringSchema
refer BooleanSchema _ = return $ DefinitionSchema (sep<>"T") $ HM.singleton (sep<>"T") BooleanSchema
refer NullSchema _ = return $ DefinitionSchema (sep<>"T") $ HM.singleton (sep<>"T") NullSchema 
refer schema@(NumberSchema _ _) _ = return $ DefinitionSchema (sep<>"T") $ HM.singleton (sep<>"T") schema
refer schema@(TupleSchema vector) _ = return $ DefinitionSchema t $ HM.insert t (TupleSchema refs) $ unionOfMaps 
    where 
        t = sep <> "Tuple"
        updatedSchemae :: Vec.Vector Schema
        updatedSchemae = Vec.zipWith (\s k -> pushRef k s) vector $ Vec.fromList $ [t <> sep <> (Text.pack $ show ix) | ix <- [0..]]
        (refs, unionOfMaps) = 
            Vec.foldr (\(DefinitionSchema top submap) (rs, ms) -> (Vec.cons (RefSchema top) rs, HM.union ms submap)) (Vec.empty, HM.empty) updatedSchemae
refer (SchemaWithOracle schema oracle) _ = case schema of 
    DefinitionSchema top defs -> return $ DefinitionSchema top $ HM.insert top (SchemaWithOracle (defs HM.! top) oracle) defs
    _ -> error $ show (SchemaWithOracle schema oracle)
refer (ObjectSchema object required) _ = return $ DefinitionSchema o $ HM.insert o (ObjectSchema refs required) $ unionOfMaps
    where
        o = sep <> "Object"
        updatedSchemae :: HM.HashMap Text.Text Schema
        updatedSchemae = HM.mapWithKey (\k v -> pushRef (o <> sep <> k) v) object
        (refs, unionOfMaps) =
            HM.foldrWithKey (\k (DefinitionSchema top submap) (rs, ms) -> (HM.insert k (RefSchema top) rs, HM.union submap ms)) (HM.empty, HM.empty) updatedSchemae
refer (AnyOf schemas) _ = return $ DefinitionSchema t $ HM.insert t (AnyOf $ map RefSchema schemNames) unionOfMaps
    where 
        t = sep <> "AnyOf"
        updatedSchemas = map (\(k, s) -> pushRef (t <> sep <> (Text.pack $ show k)) s) $ zip [1..] schemas
        (schemNames, unionOfMaps) = foldr (\(DefinitionSchema t m') (l, m) -> (t : l, HM.union m' m)) ([], HM.empty) updatedSchemas
refer (ArraySchema schema lb ub) _ = return $ DefinitionSchema t $ HM.insert t (ArraySchema (RefSchema ref) lb ub) childMap 
    where 
        t = sep <> "Array"
        (DefinitionSchema ref childMap) = pushRef t schema
