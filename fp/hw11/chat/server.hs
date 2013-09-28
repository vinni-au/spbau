import Prelude hiding (catch)
import Network
import System.IO
import System.IO.Error hiding (catch)
-- import System.Posix.Signals
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.IORef
import System.Environment
import Control.Exception (finally, catch, Exception (..), IOException)

-- Сервер поддерживает список клиентов.
-- Если один из клиентов присылает сообщение, сервер рассылает его всем остальным (но не отправителю, разумеется).
-- Не забудьте правильно обработать ситуацию, когда клиент закрывает соединение.

processSocket :: Socket -> TVar [Handle] -> IO ()
processSocket socket clientsList = forever $ do
  (hSocket, hostName, portNumber) <- accept socket
  hSetBuffering hSocket LineBuffering

  atomically $ do
    hSockets <- readTVar clientsList
    writeTVar clientsList (hSocket:hSockets)

  hPutStrLn hSocket "Connected to server. Type \"exit\" to disconnect"

  forkIO $ processMessage hSocket clientsList

removeClient hSocket clientsList = do
  hSockets <- atomically $ readTVar clientsList
  let hSockets' = filter (\h -> h /= hSocket) hSockets
  atomically $ writeTVar clientsList hSockets'
  hClose hSocket  
  
processMessage :: Handle -> TVar [Handle] -> IO ()
processMessage hSocket clientsList = do
  iseof <- hIsEOF hSocket
  if iseof 
     then removeClient hSocket clientsList
     else do 
        msg <- hGetLine hSocket
        if (msg == "exit")
          then do
            removeClient hSocket clientsList
          else do
            sendBroadcast msg hSocket clientsList
            processMessage hSocket clientsList

sendBroadcast :: String -> Handle -> TVar [Handle] -> IO ()
sendBroadcast message hSocket clientsList = do
  hSockets <- atomically $ readTVar clientsList
  mapM_ (\h -> when (h /= hSocket) (hPutStrLn h message)) hSockets

main :: IO ()
main = withSocketsDo $ do
  args <- getArgs
  if length args /= 1
    then putStrLn "Usage: ./server <port>"
    else do
      let port = fromIntegral (read (args !! 0) :: Int)

      clientsList <- newTVarIO []
      socket <- listenOn $ PortNumber port
      processSocket socket clientsList
      sClose socket

