{-# LANGUAGE OverloadedStrings #-}


import Debug.Trace
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import JSon 
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Test.QuickCheck.Gen as Gen
import FromSchema as F

-- | "{\"anyOf\":[{\"type\":\"number\"}, null]}"

main = do 
    input <- getContents
    case F.mLatticeGenerator input  of 
        Nothing -> putStrLn $ "failed to create generator"
        Just g -> run 3000 $ \_ f -> do 
            jobj <- Gen.generate g
            f $ responseLBS status200 [(hContentType, "application/json")] $ A.encode jobj