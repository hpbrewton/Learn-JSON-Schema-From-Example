module ForwardRefs (
   forwardRefs,
   simplifyReferences
) where 

import Schema
import Util
import Data.Aeson
import Data.Scientific
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Test.QuickCheck as Gen
import qualified Data.Vector as Vec
import Generalizer

-- | find the final ref in a chain
finalRef :: HM.HashMap T.Text Schema -> Schema -> Schema
finalRef m maybeFinal@(SchemaWithOracle schema oracle) = (SchemaWithOracle (finalRef m schema) oracle)
finalRef m maybeFinal@(RefSchema ref) = case m HM.! ref of 
    next@(SchemaWithOracle (RefSchema ref) oracle) -> finalRef m next
    next@(RefSchema ref) -> finalRef m next
    _ -> maybeFinal
finalRef m (TupleSchema schemae) = TupleSchema $ fmap (finalRef m) schemae
finalRef m (ObjectSchema schemae required) = ObjectSchema (fmap (finalRef m) schemae) required
finalRef m ob = ob

forwardRefMaps :: HM.HashMap T.Text Schema -> HM.HashMap T.Text Schema
forwardRefMaps m = HM.map (finalRef m) m 

forwardRefs :: Schema -> Schema
forwardRefs (DefinitionSchema top m) = DefinitionSchema top $ forwardRefMaps m

requiredReferences :: Schema -> HS.HashSet T.Text
requiredReferences (DefinitionSchema top m) = HS.insert top $ HM.foldr (HS.union) HS.empty $ fmap requiredReferences m
requiredReferences (SchemaWithOracle schema _) = requiredReferences schema
requiredReferences (TupleSchema schemae) = Vec.foldr (HS.union) HS.empty $ fmap requiredReferences schemae
requiredReferences (ObjectSchema schemae _) = HM.foldr (HS.union) HS.empty $ fmap requiredReferences schemae
requiredReferences (RefSchema ref) = HS.singleton ref
requiredReferences _ = HS.empty

simplifyReferences :: Schema -> Schema 
simplifyReferences schema@(DefinitionSchema top m) = DefinitionSchema top $ HM.filterWithKey (\k _ -> HS.member k reqrefs) m
    where 
        reqrefs = requiredReferences schema