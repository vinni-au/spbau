-- while m запускает m пока она не вернет False
while :: Monad m => m Bool -> m ()
while = undefined

-- forM_ i j f запускает последовательно f i, f (i + 1), f (i + 2), ... f (j)
forM_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
forM_ = undefined

-- Тоже что и forM_, но возвращает список результатов
forM :: Monad m => Int -> Int -> (Int -> m a) -> m [a]
forM = undefined

-- repeatUntip_ p m запускает m пока она возвращает значение, удовлетворяющее предикату
repeatUntil_ :: Monad m => (a -> Bool) -> m a -> m ()
repeatUntil_ = undefined

-- Тоже что и repeatUntil_, но возвращает список результатов
repeatUntil :: Monad m => (a -> Bool) -> m a -> m [a]
repeatUntil = undefined
