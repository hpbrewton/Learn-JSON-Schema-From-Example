module SimplifyNumbers (
    simplifyNumbers
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

simplifyNumbers :: Schema -> Schema
simplifyNumbers (DefinitionSchema top schemas) = DefinitionSchema top $ HM.union updatedNumSchemae schemas
    where 
        isNumberSchema (NumberSchema _ _) = True 
        isNumberSchema _ = False 
        
        (numSchemae, otherSchemae) = gPartition isNumberSchema $ HM.map (\(SchemaWithOracle schema _) -> schema) schemas

        justNumSchemaData = HM.map (\(NumberSchema a b) -> (a, b)) numSchemae
        refs = HM.foldrWithKey (\k v m -> HM.insert v k m) HM.empty justNumSchemaData

        getOracle = (\(SchemaWithOracle _ oracle) -> oracle) . (schemas HM.!)
        
        replaceWithSchema label (NumberSchema a b) = if (refs HM.! (a, b)) == label
            then (SchemaWithOracle (NumberSchema a b) (getOracle label))
            else (RefSchema (refs HM.! (a, b)))

        updatedNumSchemae = HM.mapWithKey (replaceWithSchema) numSchemae


