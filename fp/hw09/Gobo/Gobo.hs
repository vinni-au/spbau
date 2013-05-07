module Gobo
    ( Coord(..)
    , Board
    , Direction(..)
    , Prog
    , go
    , inspect
    , runGobo
    ) where

import qualified Data.Map as M

data Coord = Coord { getX :: Integer, getY :: Integer } deriving (Eq, Ord, Show)
type Board = M.Map Coord Char
data Direction = L | R | U | D
newtype Prog a = Prog { execProg :: Coord -> Board -> (Coord, a) }

instance Monad Prog where
    return = undefined
    (>>=) = undefined

go :: Direction -> Prog ()
go = undefined

inspect :: Prog (Maybe Char)
inspect = undefined

runGobo :: Prog a -> Board -> a
runGobo = undefined
