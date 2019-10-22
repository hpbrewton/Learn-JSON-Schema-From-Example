import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Test.QuickCheck.Gen as Gen
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Internal as BSI
import Lattice 
import qualified Data.Aeson as A
import Network.Wreq as Wreq
import Control.Lens
import JSon

extract3TupleIO :: (a, b, IO c) -> IO (a, b, c)
extract3TupleIO (a, b, cw) = do 
    c <- cw 
    return (a, b, c)

oracleN :: JSON -> IO Bool
oracleN json = do 
    resp <- Wreq.post "http://127.0.0.1:8080/verify" (A.toJSON json) 
    return $ (=="\"true\"") $ show $ (resp ^. Wreq.responseBody)

oracleExamples :: IO [JSON]
oracleExamples = do 
    responses <- sequence $ replicate 100 $ Wreq.get "http://127.0.0.1:8080/generate"
    let successes = map (\(Just s) -> s) $ filter Maybe.isJust $ map A.decode $ map (\v -> v ^. Wreq.responseBody) responses
    return $ map fromJSON $ successes

grown :: JSON -> IO JSONLattice
grown json = grow json oracleN

convert :: String -> BS.ByteString 
convert = BS.pack . map BSI.c2w

main :: IO ()
main = do 
    contents <- readFile ".."
    let content = A.decode $ convert contents
    let example = fromJSON $ ((fromJSON $ Maybe.fromJust content) :: A.Value)
    l <- grown example

    let (_, indexedObjs) = indexJSON 0 M.empty example
    let (_, indexedOracles) = indexOracles 0 M.empty oracleN example
    let (n, indexedLatitce) = indexJSONLattice 0 M.empty l
    m <- sequence $ M.intersectionWith ($) indexedOracles indexedObjs
    let ir = equivalenceRelation nuleq indexedLatitce
    let r = refinedEquivalenceRelation indexedOracles indexedObjs ir
    pairs <- sequence $ [extract3TupleIO (i, j, r i j) | i <- [0..(n-1)], j <- [0 .. (n-1)]]
    let er = map (\(a, b, _) -> (a, b)) $ filter (\(_, _, b) -> b) pairs
    let ec = equivalanceClasses $ er
    let (top, compressed) = compress 0 ec M.empty l
    examples <- Gen.sample' $ ((latticeExample (lazyObject compressed (compressed M.! (top-1)))) :: Gen.Gen JSON)
    results <- fmap (filter id) $ sequence $ map oracleN examples
    let pos = ((fromIntegral $ length results) :: Float)
    let exs = ((fromIntegral $ length examples) :: Float)

    oExamples <- oracleExamples
    let lo = (lazyObject compressed (compressed M.! (top-1)))
    let pos' = ((fromIntegral $ length $ filter (not . accepted lo) oExamples) :: Float)
    let exs' = ((fromIntegral $ length oExamples) :: Float)

    putStrLn $ (show (pos/exs)) ++ "\t" ++ (show (pos'/exs'))
