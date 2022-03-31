{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (span, max)
import qualified Data.List as List
import Control.Monad

{-@ type S n m = n : Int -> m : Int @-}
type S = Int -> Int

{-@ reflect max @-}
max :: Int -> Int -> Int
max x y
  | x >= y = x
  | otherwise = y

{-@ orElse :: S n m1 -> S n m2 -> S n (max m1 m2) @-}
orElse :: S -> S -> S
orElse f g = \x -> f x `max` g x

{-@ empty :: S n n @-}
empty :: S
empty = id

{-@ andThen :: S n m -> S m k -> S n k @-}
andThen :: S -> S -> S
andThen g f = f . g

{-@ many :: S n m -> { _:S n' m' | m' <= m } @-}
many :: S -> S
many x = some x `orElse` empty

{-@ some :: _ @-}
some :: S -> S
some x = x `andThen` many x

-- data Parser a = MkP { runParser :: String -> [(a, String)] }
-- 
-- instance Functor Parser where
--   fmap f (MkP p) = MkP \s -> do
--     (x, s') <- p s
--     pure (f x, s')
-- 
-- instance Applicative Parser where
--   pure x = MkP \s -> [(x,s)]
--   MkP p <*> MkP q = MkP \s -> do
--     (f,s') <- p s
--     (x,s'') <- q s'
--     pure (f x, s'') 
-- 
-- instance Alternative Parser where
--   empty = MkP \_ -> []
--   MkP p <|> MkP q = MkP \s -> p s <|> q s
-- 
-- span :: (Char -> Bool) -> Parser String
-- span f = MkP \s -> [List.span f s]
-- 
-- satisfy :: (Char -> Bool) -> Parser Char
-- satisfy f = MkP \s -> 
--   case s of
--     x:s' | f x -> [(x, s')]
--     _ -> []
-- 
-- char :: Char -> Parser Char
-- char c = satisfy (== c)
-- 
-- parens :: Parser a -> Parser a
-- parens p = char '(' *> p <* char ')'
-- 
-- fully :: Parser a -> Parser a
-- fully p = MkP \s -> filter (\(x,s') -> null s') (runParser p s)
-- 
-- some :: Parser a -> Parser [a]
-- some p = (:) <$> p <*> many p
-- 
-- many :: Parser a -> Parser [a]
-- many p = some p <|> pure []

main :: IO ()
main = putStrLn "Hello, Haskell!"
