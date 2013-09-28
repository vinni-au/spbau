module Parser
    ( pure
    , (<$>)
    , (<$)
    , (<*>)
    , (<*)
    , (*>)
    , empty
    , (<|>)
    , Parser
    , satisfy
    , eof
    , runParser
    , execParser
    , compose
    ) where

import Control.Applicative
import Data.Maybe
import Data.List

newtype Parser l a = Parser { execParser :: [l] -> Maybe (a, [l]) }

instance Functor (Parser l) where
    fmap f (Parser x) = Parser (\xs -> fmap g (x xs))
      where g (a, s) = (f a, s)

instance Applicative (Parser l) where
    pure x = Parser (\l -> Just (x, l)) 
    (<*>) (Parser f) (Parser a) = Parser $ \xs -> 
      case f xs of
        Nothing -> Nothing
        Just (g, ys) -> case a ys of
          Nothing -> Nothing
          Just (y, z) -> Just (g y, z)

instance Alternative (Parser l) where
    empty = Parser (\l -> Nothing) 
    (<|>) (Parser fx) (Parser fy) = Parser $ \xs -> case fx xs of
        Nothing -> fy xs 
        r -> r

runParser :: Parser l a -> [l] -> Maybe a
runParser (Parser f) xs = 
  case f xs of
    Nothing -> Nothing
    Just (x, _) -> Just x

satisfy :: (l -> Bool) -> Parser l l
satisfy f = Parser $ \xs -> 
  if g xs then Just (head xs, tail xs) else Nothing
    where
      g [] = False
      g (y: ys) = f y

eof :: Parser l ()
eof = Parser $ \xs -> if null xs 
                        then Just((), xs) 
                        else Nothing

compose :: Parser b c -> Parser a b -> Parser a c
compose (Parser a) (Parser b) = Parser $ \x -> 
  case b x of
    Nothing -> Nothing
    Just (c, d) -> case a [c] of
      Nothing -> Nothing
      Just (y, z) -> Just (y, d)


