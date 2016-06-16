{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module          : Text.ByoParser.Lexer
Description     : Primitive lexer type
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable

A Lexer is a transformer from stream to stream. This implementation represents
streams with the 'Colist' type.

-}
module Text.ByoParser.Lexer where

import Control.Applicative  ( Alternative(..) )
import Control.Monad
import Control.Monad.Fail
import Data.Char            ( isSpace )

import Prelude hiding ( lex )

{-| Representation of an atomic transformation of a stream of tokens
of type @i@ to a stream of tokens of type @o@.
-}
newtype Lexer i o = Lexer {
    runLexer :: [i] -> ListA o [i]
}

data ListA x s = Nil | Cons x s

{-| Apply a 'Lexer' on a list.
-}
lex :: Lexer i o -> [i] -> [o]
lex (Lexer l) =
    unfold l

unfold :: ([i] -> ListA o [i]) -> [i] -> [o]
unfold f xs = case f xs of
    Nil       -> []
    Cons y xs -> y : (unfold f xs)

{-| Build a Lexer accepting the given token.
-}
token :: Eq i => i -> Lexer i i
token c = satisfy (c ==)

{-| Build a Lexer returning the next token.
-}
anyToken :: Lexer i i
anyToken = satisfy (const True)

{-| Build a Lexer accepting the next token only if it satisfies the given
predicate.
-}
satisfy :: (i -> Bool) -> Lexer i i
satisfy test =
    Lexer $ \case
        []                   -> Nil
        c : cs | test c      -> Cons c cs
               | otherwise   -> Nil

{-| Build a Lexer that succeeds only of the input stream is exhausted.
-}
endOfInput :: Lexer i ()
endOfInput = Lexer $ \case
        [] -> Cons () []
        _  -> Nil

{-| Build a Lexer that recognizes the given string of tokens.
-}
string :: Eq i => [i] -> Lexer i [i]
string [] =
    return []
string (c:cs) = do
    c <- token c
    cs <- string cs
    return (c : cs)


sepBy :: Lexer i a -> Lexer i sep -> Lexer i [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Lexer i a -> Lexer i sep -> Lexer i [a]
sepBy1 p sep = (:) <$> p <*> many (sep >> p)

skipSpaces :: Lexer Char ()
skipSpaces = do
    many (satisfy isSpace)
    return ()


-----
-- Instances
-----

instance Functor (Lexer i) where
    fmap f = Lexer . (post .) . runLexer
        where
            post Nil         = Nil
            post (Cons x xs) = Cons (f x) xs

instance Applicative (Lexer i) where
    pure  = return
    (<*>) = ap

instance Alternative (Lexer i) where
    empty   = Lexer (const Nil)

    m <|> n = Lexer $ \nu ->
        case runLexer m nu of
            Nil       -> runLexer n nu
            Cons x xs -> Cons x xs

instance Monad (Lexer i) where
    return x = Lexer (Cons x)

    m >>= k = Lexer $ \xs ->
        case runLexer m xs of
            Nil       -> Nil
            Cons x xs -> runLexer (k x) xs

instance MonadPlus (Lexer i) where

instance MonadFail (Lexer i) where
    fail _ = mzero
