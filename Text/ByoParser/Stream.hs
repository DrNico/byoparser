{-# LANGUAGE
        TypeFamilies
  #-}

{-|
Module          : Text.ByoParser.Stream
Description     : Primitive parsers that consume an input stream
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable
-}
module Text.ByoParser.Stream (
    ByoStream(Token, anyToken, string, scan),
    -- * Parsing tokens
    token,
    notToken,
    satisfy,
    satisfyWith,
    skip,
    endOfLine,
    endOfInput,
    -- * Lookahead
    peekChar,
    peekChar',
    -- * Parsing strings of tokens
    skipWhile,
    take,
    takeWhile,
    takeTill
) where


import Text.ByoParser.Prim      ( ParserPrim(..), ErrorPrim(..) )

import Prelude (
    ($), (.), fst, snd,
    Eq(..), Bool(..), not, Maybe(..),
    Int, Num(..), (>), Enum(..),
    fmap,
    otherwise, error )

{-|
Class of input streams compatible with 'ParserPrim'.
-}
class ByoStream i where
    type Token i :: *

    anyToken :: ParserPrim i e s r (Token i)
    string   :: i -> ParserPrim i e s r i
    scan     :: t -> (t -> Token i -> Maybe t) -> ParserPrim i e s r (t,i)


{-|
Match the given token.
-}
token :: (ByoStream i, tok ~ Token i, Eq tok)
      => tok -> ParserPrim i e s r tok
token t = satisfy (== t)
{-# INLINABLE token #-}

{-|
Match any but the given token and produce it.
-}
notToken :: (ByoStream i, tok ~ Token i, Eq tok)
      => tok -> ParserPrim i e s r tok
notToken t = satisfy (/= t)
{-# INLINABLE notToken #-}

{-|
Match and produce the next token only if it satisfies the given predicate.
-}
satisfy :: ByoStream i
        => (Token i -> Bool) -> ParserPrim i e s r (Token i)
satisfy test = Prim $ \noC okS noS okC ->
    \i -> runPrim anyToken
            (error "unpossible! 'anyToken' failed while consuming input")
            (error "unpossible! 'anyToken' succeeded without consuming input")
            noS
            (\t i' -> if test t
                      then okC t i'
                      else noS ErrPrimNoMatch i
            )
            i
{-# INLINABLE satisfy #-}

{-|
Convert the next token with the given function and test against the given
predicate, produce the value on success.
-}
satisfyWith :: ByoStream i
            => (Token i -> a) -> (a -> Bool) -> ParserPrim i e s r a
satisfyWith conv test = Prim $ \noC okS noS okC ->
    \i -> runPrim anyToken
            (error "unpossible! 'anyToken' failed while consuming input")
            (error "unpossible! 'anyToken' succeeded without consuming input")
            noS
            (\t i' -> let x = conv t in
                if test x
                then okC x i'
                else noS ErrPrimNoMatch i
            )
            i
{-# INLINABLE satisfyWith #-}

{-|
Consume the next token of the input stream only if it satisfies the
given predicate.
-}
skip :: ByoStream i
        => (Token i -> Bool) -> ParserPrim i e s r ()
skip test = Prim $ \noC okS noS okC ->
    \i -> runPrim anyToken
            (error "unpossible! 'anyToken' failed while consuming input")
            (error "unpossible! 'anyToken' succeeded without consuming input")
            noS
            (\t i' -> if test t
                      then okC () i'
                      else noS ErrPrimNoMatch i
            )
            i
{-# INLINABLE skip #-}

{-|
Consumes any of the three common line terminators:

  * @\r@ carriage return
  * @\n@ line-feed
  * @\r\n@ carriage return and line-feed

-}
endOfLine :: (ByoStream i, t ~ Token i, Enum t)
          => ParserPrim i e s r ()
endOfLine = Prim $ \noC okS noS okC ->
    \i -> runPrim anyToken
        (error "unpossible! 'anyToken' failed while consuming input")
        (error "unpossible! 'anyToken' succeeded without consuming input")
        noS
        (\t i' ->
            if fromEnum t == 0x0D -- carriage return
            then runPrim anyToken
                (error "unpossible! 'anyToken' failed while consuming input")
                (error "unpossible! 'anyToken' succeeded without consuming input")
                (\_ _ -> okC () i')
                (\t i'' ->
                    if fromEnum t == 0x0A -- line feed
                    then okC () i''
                    else okC () i'
                )
                i'
            else if fromEnum t == 0x0A -- line feed
                 then okC () i'
                 else noS ErrPrimNoMatch i
        )
        i
{-# INLINABLE endOfLine #-}

{-|
Succeeds if there are no more tokens left in the stream, otherwise fails.
-}
endOfInput :: ByoStream i => ParserPrim i e s r ()
endOfInput = Prim $ \noC okS noS okC ->
    \i -> runPrim anyToken
        (error "unpossible! 'anyToken' failed while consuming input")
        (error "unpossible! 'anyToken' succeeded without consuming input")
        (\_ _ -> okS () i)
        (\_ _ -> noS ErrPrimNoMatch i)
        i

-----
-- Lookahead
-----

{-|
Produce the next token of the stream without consuming it, or 'Nothing'
if there are no token left. This parser never fails.
-}
peekChar :: ByoStream i => ParserPrim i e s r (Maybe (Token i))
peekChar = fmap fst $ scan Nothing f
    where
        f Nothing c  = Just (Just c)
        f (Just _) _ = Nothing
{-# INLINABLE peekChar #-}

{-|
Produce the next token of the stream without consuming it, or fail if
there are no token left.
-}
peekChar' :: ByoStream i => ParserPrim i e s r (Token i)
peekChar' = Prim $ \noC okS noS okC ->
    \i -> runPrim peekChar
            (error "unpossible! 'peekChar' failed")
            (\mt -> case mt of
                Just t  -> okS t
                Nothing -> noS ErrPrimEOF
            )
            (error "unpossible! 'peekChar' failed")
            (error "unpossible! 'peekChar' consumed input")
            i
{-# INLINABLE peekChar' #-}

-----
-- Capturing stream portions
-----

{-|
Consume the next /n/ tokens and produce them as a stream.

If /n/ is negative or zero, produces the empty string. If less than /n/
tokens remain in the stream, produces a string shortened accordingly.
-}
take :: ByoStream i => Int -> ParserPrim i e s r i
take n = fmap snd $ scan n f
    where
        f n _ | n > 0     = Just (n - 1)
              | otherwise = Nothing
{-# INLINE take #-}

{-|
Consume the next tokens satisfying the predicate. This parser always succeeds.
-}
skipWhile :: ByoStream i
          => (Token i -> Bool) -> ParserPrim i e s r ()
skipWhile test = fmap (\_ -> ()) $ takeWhile test
{-# INLINE skipWhile #-}

{-|
Consume the next tokens satisfying the predicate and produce them as a stream.
This parser always succeeds.
-}
takeWhile :: ByoStream i
          => (Token i -> Bool) -> ParserPrim i e s r i
takeWhile test = fmap snd $ scan () f
    where
        f () x = if test x then Just () else Nothing
{-# INLINE takeWhile #-}

{-|
Consume the next tokens until one satisfies the predicate, and produce them as
a stream. The token satisfying the predicate is neither produced nor consumed.
This parser always succeeds.
-}
takeTill :: ByoStream i
         => (Token i -> Bool) -> ParserPrim i e s r i
takeTill test = takeWhile (not . test)
{-# INLINE takeTill #-}
