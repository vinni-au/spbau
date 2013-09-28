module Eval
    ( Eval
    , Error
    , Store
    , update
    , getVar
    , runEval
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Monad Eval where
    return x = Eval $ \store -> (Just x, [], store)
    Eval m >>= k = Eval $ \store -> do
      let (a, err, s) = m store
      case a of
        Just x -> do
          let (b, err1, s1) = runEval (k x) s
          (b, union err err1, s1)
        Nothing -> (Nothing, err, s)
      

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval $ \store -> (Nothing, ["Error from nowhere"], store)
    mplus (Eval l) (Eval r) = Eval $ \store -> do
      let (a, err, s) = l store
      case a of 
        Just x -> (a, err, s)
        Nothing -> do
          let (b, err1, s1) = r store
          (b, union err err1, s1)

update :: String -> Value -> Eval ()
update k v = Eval $ \store -> do
  let s = M.insert k v store
  (Just (), [], s)

getVar :: String -> Eval Value
getVar v = Eval $ \store -> do
  let a = M.lookup v store
  case a of
    Just x -> (a, [], store)
    Nothing -> (Nothing, ["Undefined variable " ++ v], store)


