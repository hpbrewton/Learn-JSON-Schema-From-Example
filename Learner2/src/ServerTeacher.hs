{-# LANGUAGE OverloadedStrings #-}


module ServerTeacher (
    ServerTeacher (..)
) where 

import Teacher
import Data.Aeson 
import Control.Lens
import Network.Wreq
import Text.Printf

data ServerTeacher = ServerTeacher {
    url :: String 
}

instance Teacher ServerTeacher where 
    start teacher = do 
        response <- get ((url teacher) <> "start")
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse start"
            Just v -> return v 
    oracle teacher = return $ \value -> do 
        let opts = defaults & header "Content-Type" .~ ["text/plain"]
        response <- (postWith opts ((url teacher) <> "member") value) 
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse response of oracle"
            Just (Bool b) -> do 
                return b 
            _ -> error "was expecting a boolean form the oracle"
    example teacher = do 
        response <- get ((url teacher) <> "example")
        case decode $ (response ^. responseBody) of 
            Nothing -> error "could not parse example"
            Just v -> return v
