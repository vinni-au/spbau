import System.Environment
import System.Random
import Control.Monad
import Control.Concurrent.STM
import Control.Concurrent

{-
Решите задачу о философах.
Количество философов передается в параметрах командной строки.
Жизненый цикл философа:
a) Философ сообщает (вывод сообщения на экран) о том, что он готов обедать.
b) Ждет пока не освободятся обе вилки.
c) Берет вилки, сообщает об этом, начинает обедать, что занимает рандомное время (от 1 до 3 секунд).
d) Кладет вилки, сообщает об этом, начинает думать, что занимает рандомное время (от 1 до 3 секунд).
e) Возвращается к шагу (a).

Для реализации используйте библиотеку STM.
Вам также понадобятся функции stickIO, threadDelay и randomRIO.
-}

--randomDelay :: IO ()
randomDelay = do
  delay <- randomRIO (1000000, 3000000)
  threadDelay delay

createStick :: a -> IO (TVar Bool)
createStick _ = newTVarIO True

takeStick :: TVar Bool -> STM ()
takeStick stick = do
  isfree <- readTVar stick
  check isfree
  writeTVar stick False

releaseStick :: TVar Bool -> STM ()
releaseStick stick = do
  writeTVar stick True

philosopher :: Int -> Int -> [TVar Bool] -> IO ()
philosopher i n sticks = do
  print $ (show i) ++ ": ready to eat."
  atomically $ do
    takeStick $ sticks !! i
    takeStick $ sticks !! (mod (i+1) n)
  
  print $ (show i) ++ ": eating."
  randomDelay
  
  atomically $ do
    releaseStick $ sticks !! i
    releaseStick $ sticks !! (mod (i+1) n)
  
  print $ (show i) ++ ": thinking."
  randomDelay


main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "Usage: ./Philosopher <number of philosophers>"
    else do
      let n = read (args !! 0) :: Int
      
      sticks <- mapM createStick [0..n-1]

      forM_ [0..n-1] 
        (\i -> forkIO (forever $ philosopher i n sticks))

      forever (threadDelay 10050000)
