module InLanguage(
    main
)
where

import Control.Exception
import Data.Ratio

doesItError :: a -> IO Bool
doesItError a = do 
    v <- (try (Control.Exception.evaluate $ a)) -- :: IO (Either Control.Exception.SomeException a)
    case v of 
        Left err -> return (const True (err :: Control.Exception.SomeException))
        Right val -> return False



main = do 
    v <- doesItError (1 % 0)
    putStrLn $ show v