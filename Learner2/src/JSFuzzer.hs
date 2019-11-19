module JSFuzzer (
    main
) where 

import System.Process
import GHC.IO.Handle

rubyPath = "/usr/local/opt/ruby/bin/ruby"
rubyFile = "/Users/hb/Desktop/ruby/test.rb"
wd = "/Users/hb/Desktop"

main :: IO ()
main = do
    result <- readProcess rubyPath [rubyFile, "/Users/hb/latlib/Learner2/data/1/schema.json", "5"] ""
    putStrLn result


