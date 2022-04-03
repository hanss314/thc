module Parser where


import Data.Functor
import Data.Foldable
import Control.Applicative
import Control.Monad

newtype Parser s a = Parser {runParser :: [s] -> Maybe ([s], a)}

instance Functor (Parser s) where
    fmap f p = Parser $ \s -> do
        (cs, c) <- runParser p s
        return (cs, f c)

instance Applicative (Parser s) where
    pure a = Parser $ \s -> Just (s, a)
    (<*>) p1 p2 = Parser $ \s -> do
        (s' , f) <- runParser p1 s
        (s'', x) <- runParser p2 s'
        return (s'', f x)

instance Alternative (Parser s) where
    empty = Parser $ const Nothing
    (<|>) p1 p2 = Parser $ \s -> (runParser p1 s) <|> (runParser p2 s)

instance Monad (Parser s) where
    (>>=) p f = Parser $ \s -> do
        (s', a) <- runParser p s
        runParser (f a) s'

getOne :: Parser s s
getOne = Parser $ \s -> case s of 
    (c:cs) -> Just (cs, c)
    [] -> Nothing

matchOne :: (Eq s) => s -> Parser s s
matchOne c = Parser $ \s -> case s of
    (x:xs) -> if x == c then Just (xs, x) else Nothing
    [] -> Nothing

matchList :: (Eq s) => [s] -> Parser s [s]
matchList = foldr (liftA2 (:) . matchOne) (pure [])

suppress :: Parser s a -> Parser s ()
suppress = fmap (const ())

matchConst :: (Eq s) => a -> [s] -> Parser s a
matchConst a = fmap (const a) . matchList

satisfying :: (a -> Bool) -> Parser s a -> Parser s a
satisfying f p = do
    c <- p
    guard $ f c
    return c

satisfyingOne :: (s -> Bool) -> Parser s s
satisfyingOne = flip satisfying getOne

