module ErrorTeacher (
    -- main
)
where

import Teacher 
import Data.Aeson
import Control.Exception
import Data.Ratio

-- doesItError :: a -> IO Bool
-- doesItError a = do 
--     v <- (try (Control.Exception.evaluate $ a)) -- :: IO (Either Control.Exception.SomeException a)
--     case v of 
--         Left err -> return (const True (err :: Control.Exception.SomeException))
--         Right val -> return False

-- data Calc a b = Calc (a->b)

-- instance Teacher (Calc a b) where 
--     member (doesItError)
