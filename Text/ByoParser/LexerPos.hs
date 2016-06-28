{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Text.ByoParser.LexerPos (
    Lexer,
    lex,
    char, anyChar, satisfy, string, letter, digit, newline,
    sepBy, sepBy1, skipSpaces, endOfInput,
    SrcLoc(..),
    location
) where

import Control.Applicative  ( Alternative(..) )
import Control.Monad
import Control.Monad.Fail
import Data.Char            ( isLetter, isDigit, isSpace )

import Prelude hiding ( lex )


{-| Representation of an atomic transformation of a stream of tokens
of type @i@ to a stream of tokens of type @o@.
-}
newtype Lexer i o = Lexer {
    runLexer :: forall r. String
             -> (Int -> Int -> ListA o [i] -> r)
             -> (Int -> Int -> [i] -> r)
}

data ListA x s = Nil | Cons x s


lex :: String -> Lexer i o -> [i] -> [o]
lex fname (Lexer l) = l fname go 1 1
    where
        go lin col Nil          = []
        go lin col (Cons x ys)  = x : (l fname go lin col ys)

char :: Char -> Lexer Char Char
char c = satisfy (c ==)

anyChar :: Lexer Char Char
anyChar = satisfy (const True)

satisfy :: (Char -> Bool) -> Lexer Char Char
satisfy test = Lexer $ \_ cont lin col s -> case s of
        []              -> cont lin col Nil
        c : cs
            | test c    -> if c == '\n'
                           then cont (lin + 1) 1 (Cons c cs)
                           else cont lin (col + 1) (Cons c cs)
            | otherwise -> cont lin col Nil

letter :: Lexer Char Char
letter = satisfy isLetter

digit :: Lexer Char Char
digit = satisfy isDigit

newline :: Lexer Char ()
newline = do
    char '\n'
    return ()
    -- TODO : recognize '\n\r' line terminations

string :: [Char] -> Lexer Char [Char]
string [] =
    return []
string (c:cs) = do
    c <- char c
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

{-| Build a Lexer that succeeds only of the input stream is exhausted.
-}
endOfInput :: Lexer i ()
endOfInput = Lexer go
    where
        go _ cont lin col [] = cont lin col (Cons () [])
        go _ cont lin col _  = cont lin col Nil


data SrcLoc = SrcLoc {
    srcFile     :: !String,
    srcLine     :: !Int,
    srcCol      :: !Int
} deriving (Eq,Show)

location :: Lexer i SrcLoc
location = Lexer $ \fname cont lin col s ->
    cont lin col (Cons (SrcLoc fname lin col) s)


-----
-- Instances
-----

instance Functor (Lexer i) where
    fmap f (Lexer lex) = Lexer $ \fname cont ->
        lex fname $ \l c -> \case
            Nil       -> cont l c Nil
            Cons x cs -> cont l c (Cons (f x) cs)

instance Applicative (Lexer i) where
    pure  = return
    (<*>) = ap

instance Alternative (Lexer i) where
    empty = Lexer $ \_ cont l c _ -> cont l c Nil

    Lexer m <|> Lexer n = Lexer $ \fname cont l1 c1 s ->
        m fname (\l2 c2 -> \case
                Nil         -> n fname cont l1 c1 s
                Cons x cs   -> cont l2 c2 (Cons x cs)
            ) l1 c1 s

instance Monad (Lexer i) where
    return x = Lexer $ \_ cont l c s -> cont l c (Cons x s)

    Lexer m >>= k = Lexer $ \fname cont l c s ->
        m fname (\l c -> \case
                Nil         -> cont l c Nil
                Cons x cs   -> runLexer (k x) fname cont l c cs
            ) l c s

instance MonadPlus (Lexer i) where

instance MonadFail (Lexer i) where
    fail _ = mzero
