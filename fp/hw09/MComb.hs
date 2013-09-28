-- while m запускает m пока она не вернет False
while :: Monad m => m Bool -> m ()
while b = do
  res <- b
  if res 
    then while b
    else return ()

-- forM_ i j f запускает последовательно f i, f (i + 1), f (i + 2), ... f (j)
forM_ :: Monad m => Int -> Int -> (Int -> m a) -> m ()
forM_ i j f = do
  if (i > j)
    then return ()
    else do
      f i
      forM_ (i+1) j f

-- Тоже что и forM_, но возвращает список результатов
forM :: Monad m => Int -> Int -> (Int -> m a) -> m [a]
forM i j f = 
  if i > j 
    then return []
    else f i >>= \res -> ((forM (i+1) j f) >>= \x -> return (res:x))

-- repeatUntip_ p m запускает m пока она возвращает значение, удовлетворяющее предикату
repeatUntil_ :: Monad m => (a -> Bool) -> m a -> m ()
repeatUntil_ p m = do
  res <- m
  if p res
    then repeatUntil_ p m
    else return ()
      

-- Тоже что и repeatUntil_, но возвращает список результатов
repeatUntil :: Monad m => (a -> Bool) -> m a -> m [a]
repeatUntil p m = m >>=
  \res -> if p res 
    then return [res]
    else ((repeatUntil p m) >>= \res1 -> return (res:res1))
