import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Environment
import System.Exit

-- Клиент простой: считывает ввод с stdin, отправляет серверу; то, что получает от сервера, выводит на stdout

readStdinTo :: TChan String -> IO ()
readStdinTo chan = forever $ do
  line <- getLine
  atomically $ writeTChan chan line

readToStdoutFrom :: Handle -> TChan String -> IO ()
readToStdoutFrom hSocket chan = forever $ do
  line <- hGetLine hSocket
  atomically $ writeTChan chan line

mainLoop :: Handle -> TChan String -> TChan String -> IO ()
mainLoop hSocket outChannel inChannel = do
  ioline <- atomically $ readFromSomewhere outChannel inChannel
  case ioline of
    Left line -> do
      putStrLn line
      mainLoop hSocket outChannel inChannel
    Right line -> do
      hPutStrLn hSocket line
      when (line /= "exit") $ 
        mainLoop hSocket outChannel inChannel

readFromSomewhere :: TChan String -> TChan String -> STM (Either String String)
readFromSomewhere ch1 ch2 = orElse 
  (do
    a <- readTChan ch1
    return (Left a) )
  (do
    b <- readTChan ch2 
    return (Right b) )
    
main :: IO ()
main = do
  args <- getArgs
  if length args /= 2 
    then putStrLn "Usage ./client <host> <port>"
    else do
       let port = fromIntegral (read (args !! 1) :: Int)

       hSocket <- connectTo (args !! 0) $ PortNumber port
       hSetBuffering hSocket LineBuffering
       
       inChannel <- newTChanIO
       outChannel <- newTChanIO
       
       forkIO $ readStdinTo inChannel
       forkIO $ readToStdoutFrom hSocket outChannel

       mainLoop hSocket outChannel inChannel

       hClose hSocket

