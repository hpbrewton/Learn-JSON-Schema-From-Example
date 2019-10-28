comonoid :: (Foldable m, Monoid (m (a, a)), Functor m) => m a -> m (a, a) 
comonoid xs = foldr (mappend) mempty $ fmap (\a -> fmap (\b -> (a, b)) xs) xs

order :: (Foldable m, Monoid (m (a, a)), Functor m) => m a -> (a -> a -> IO Bool) -> IO (a, (m a))
order xs gte = 
    where 
        pairs = comonoid xs gte 
        