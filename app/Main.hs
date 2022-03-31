{-# LANGUAGE BlockArguments #-}
module Main where

import Prelude hiding (span)
import qualified Data.List as List
import Control.Monad

import Control.Applicative hiding (many, some)

{-@ data S n = S (xs : Int -> { ys : Int | len xs - len ys <= n }) @-}
data S = S (Int -> Int)

instance Semigroup S where
  S f <> S g = S (\x -> max (f x) (g x))

instance Monoid S where
  mempty = S id

andThen :: S -> S -> S
andThen (S g) (S f) = S (f . g)

-- many :: S -> S
-- many x = some x <> mempty
-- 
-- some :: S -> S
-- some x = x `andThen` many x

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
