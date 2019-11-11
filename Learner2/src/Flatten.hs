{-# LANGUAGE OverloadedStrings #-}
module Flatten (
    flatten 
) where 

import Schema 
import Data.Aeson
import qualified Data.Vector as Vec 
import qualified Data.HashMap.Strict as HM
import Data.Text 
import Util

sep :: Text
sep = "."

terminal :: Text -> Schema -> Schema
terminal prefix = DefinitionSchema ref . HM.singleton ref 
    where 
        ref = prefix <> sep <> "T"

flatten :: Text -> Schema -> Schema 
flatten prefix (SchemaWithOracle schema oracle) = DefinitionSchema t $ HM.insert t (SchemaWithOracle (m `Util.errorLookup` t) oracle) m
    where 
        DefinitionSchema t m = flatten prefix schema
flatten prefix StringSchema = terminal prefix StringSchema
flatten prefix (SingStringSchema a) = terminal prefix (SingStringSchema a)
flatten prefix BooleanSchema = terminal prefix BooleanSchema
flatten prefix NullSchema = terminal prefix NullSchema
flatten prefix (NumberSchema a b) = terminal prefix (NumberSchema a b)
flatten prefix (ArraySchema (AnyOf schemas) lb ub) = DefinitionSchema top $ HM.insert top (ArraySchema (AnyOf refs) lb ub) m
    where 
        top = prefix <> sep <> "Array"
        defSchemas = fmap (\(i, s) -> flatten (top <> sep <> (showT i)) s) $ Prelude.zip [1..] schemas
        (refs, m) = 
            Prelude.foldr (\(DefinitionSchema ref cm) (refs', c') -> ((RefSchema ref): refs', cm `HM.union` c')) 
                ([], HM.empty) defSchemas
flatten prefix (TupleSchema schemas) = DefinitionSchema top $ HM.insert top (TupleSchema refs) m
    where 
        top = prefix <> sep <> "Tuple"
        defSchemas = fmap (\(i, s) -> flatten (top <> sep <> (showT i)) s) $ Prelude.zip [1..] $ Vec.toList schemas
        (refs, m) = 
            Prelude.foldr (\(DefinitionSchema ref cm) (refs', c') -> (Vec.cons (RefSchema ref) refs', cm `HM.union` c')) 
                (Vec.empty, HM.empty) defSchemas
flatten prefix (ObjectSchema object req) = DefinitionSchema top $ HM.insert top (ObjectSchema objects' req) m 
    where 
        top = prefix <> sep <> "Object"
        defSchemas = fmap (\(k, s) -> (k, flatten (top <> sep <> k) s)) $ HM.toList object 
        (refs, m) = 
            Prelude.foldr (\(k, DefinitionSchema ref cm) (refs', c') -> (HM.insert k ref refs', cm `HM.union` c')) 
                (HM.empty, HM.empty) defSchemas
        objects' = fmap RefSchema refs
        