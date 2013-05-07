import Network
import System.IO
-- import System.Posix.Signals
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.IORef

-- Сервер поддерживает список клиентов.
-- Если один из клиентов присылает сообщение, сервер рассылает его всем остальным (но не отправителю, разумеется).
-- Не забудьте правильно обработать ситуацию, когда клиент закрывает соединение.
