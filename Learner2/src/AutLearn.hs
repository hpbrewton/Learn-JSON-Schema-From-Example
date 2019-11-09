module AutLearn (
    main
) where 

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Automaton a = Automaton Int (M.Map (Int, a) (Int)) (S.Set Int)

member :: (Ord a, Eq a) => Automaton a -> [a] -> Bool 
member (Automaton n m s) [] = n `S.member` s
member (Automaton n m s) (x:xs) = case M.lookup (n, x) m of 
    Just n' -> member (Automaton n' m s) xs 
    Nothing -> False 

-- ab*
abstar :: Automaton Char
abstar = Automaton 1 (M.fromList [((1, 'a'), 2), ((2, 'b'), 2)]) (S.fromList [2])

main :: IO ()
main = do 
    putStrLn $ show $ member abstar "abbbb"

-- data Tree a = Inner a (Tree a) (Tree a) 
--     | Leaf a

-- sift :: ([a] -> IO Bool) -> [a] -> Tree [a] -> IO [a]
-- sift oracle s (Leaf a) = a 
-- sfit oracle s (Inner d l r) = do 
--     r <- oracle (s <> d)
--     if r 
--         then sift oracle s l 
--         else sift oracle s r 


