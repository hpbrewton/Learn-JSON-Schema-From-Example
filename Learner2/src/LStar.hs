lstar oracle alphabet = lstarLoop (S.singleton []) (S.singleton []) (M.empty)
    where
        isClosed :: S.Set a -> S.Set a -> M.Map a -> Bool
        isConsistent :: S.Set a -> S.Set a -> M.Map a -> Bool
        conjecture :: 
        lstarLoop :: S.Set a -> S.Set a -> M.Map 
