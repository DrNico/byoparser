
{-|
Module          : Text.ByoParser.Prim
Description     : Primitive types over which all parsers are based
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable

The primitive parser type 'ParserPrim' underlies all parsers and stream
recognizers. The recommended usage is to define a type alias over the primitive
parser type and write the whole parser with that type. See the 'ParserPrim' entry
for the meaning of all type arguments.

Examples:

> type MySimpleParser a = forall r. ParserPrim String () () r a
>
> type MyDreamParser a = forall r.
>   ParserPrim UTF8String MyErrors (SrcLocT MyState) (ResultT MyMonad r) a

For more information, read the documentation!
-}
module Text.ByoParser.Prim (
    ParserPrim(..),
    parsePrim, parseMaybe, parseEither,
    ResultPrim(..),
    ErrorPrim(..),
    Alternative(..)
) where

import Control.Applicative  ( Applicative(..), Alternative(..) )
import Control.Monad        ( Monad(..), ap )
import Data.Functor         ( Functor(..) )

import Prelude ( ($), String, (++), Maybe(..), Either(..), Show(..) )

{-|
The primitive parser type on which all other parsers are built,
taking five type arguments:

    [input stream type @i@] 'String', 'Text', 'BS.ByteString'
          or 'UTF8String'

    [error type @e@] any error type of your choice, use '()' for
          minimal error reporting or 'ParseError' for more features

    [parser state @s@] any state of your choice, use '()' for
          no state, byoparser's 'SourcePos' for source locations
          and 'Layout' for layout detection support

    [parse result type @r@] leave as variable @r@ for any result type,
          or use 'ParseResult' for partial results or monadic result handling

    [output type @o@] the type of the token produced by a particular parser.

-}
newtype ParserPrim i e s r o = Prim {
    runPrim     :: (ErrorPrim e -> i -> s -> ResultPrim e r) -- consumed fail continuation
                -> (o -> i -> s-> ResultPrim e r)            -- skip success continuation
                -> (ErrorPrim e -> i -> s -> ResultPrim e r) -- skip fail continuation
                -> (o -> i -> s -> ResultPrim e r)           -- consumed success continuation
                -> i -> s -> ResultPrim e r                  -- resulting parser
}

{-|
Run the parser on the given input stream and initial state, returning
'Just' a result or 'Nothing' on failure.

This is the simplest invocation of a primitive parser.
-}
parseMaybe :: ParserPrim i e s r r -> i -> s -> Maybe r
parseMaybe p i s = case parsePrim p i s of
    ResPrimFail _ -> Nothing
    ResPrimDone r -> Just r

{-|
Run the parser on the given input stream and initial state, returning
'Right' a result or 'Left' an error string.
-}
parseEither :: Show e
            => ParserPrim i e s r r -> i -> s -> Either String r
parseEither p i s = case parsePrim p i s of
    ResPrimFail e -> Left $ show e
    ResPrimDone r -> Right r

{-|
Run the parser on the given input stream and initial state, returning
a primitive result 'ResultPrim'.
-}
parsePrim :: ParserPrim i e s r r -> i -> s -> ResultPrim e r
parsePrim p =
    runPrim p no ok no ok
    where
        no e _ _ = ResPrimFail e
        ok o _ _ = ResPrimDone o

{-|
The primitive result type of a 'ParserPrim' parser applied to an input stream.

Library users are encouraged to use result types from "Text.ByoParser.Result"
which support a variety of high-level parse result drivers.
-}
data ResultPrim e r
    = ResPrimDone r             -- ^ Success with result @r@
    | ResPrimFail (ErrorPrim e) -- ^ Failure with a primitive error

{-|
The primitive error type of a 'ParserPrim' parser.

Library users are encouraged to use error types from "Text.ByoParser.Error"
which support a variety of error handling strategies.
-}
data ErrorPrim e
    = ErrPrimNoMatch
    | ErrPrimEOF
    | ErrPrimLoop
    | ErrUser e

instance Functor (ParserPrim i e s r) where
    fmap f p = Prim $ \noC okS noS okC ->
        runPrim p
            noC
            (\o -> okS (f o))
            noS
            (\o -> okC (f o))
    {-# INLINE fmap #-}

instance Monad (ParserPrim i e s r) where
    return x = Prim $ \noC okS noS okC ->
        okS x
    {-# INLINE return #-}

    m >>= f  = Prim $ \noC okS noS okC ->
        runPrim m
            noC
            (\o -> runPrim (f o) noC okS noS okC)
            noS
            (\o -> runPrim (f o) noC okC noC okC)
    {-# INLINE (>>=) #-}

    m >> f   = Prim $ \noC okS noS okC ->
        runPrim m
            noC
            (\o -> runPrim f noC okS noS okC)
            noS
            (\o -> runPrim f noC okC noC okC)
    {-# INLINE (>>) #-}

instance Applicative (ParserPrim i e s r) where
    pure     = return
    {-# INLINE pure #-}

    (<*>)    = ap
    {-# INLINE (<*>) #-}

{-
Helper function to use as an error continuation.
This function is invoked when the parser detects a loop. Such a loop
occurs when a looping combinator, such as 'many', runs an inner parser
that does not consume any input.
-}
panicLoop :: o -> i -> s -> ResultPrim e r
panicLoop _ _ _ = ResPrimFail ErrPrimLoop

instance Alternative (ParserPrim i e s r) where
    empty    = Prim $ \noC okS noS okC -> noS ErrPrimNoMatch
    {-# INLINE empty #-}

    f <|> g  = Prim $ \noC okS noS okC ->
        runPrim f
            noC
            okS
            (\_ -> runPrim g noC okS noS okC)
            okC
    {-# INLINE (<|>) #-}

    many p   = many_p
        where
        many_p = Prim $ \noC okS noS okC ->
            runPrim p
                noC
                panicLoop
                (\_ -> okS [])
                (\o -> runPrim (many_p)
                        noC
                        panicLoop
                        (\_ -> okC [o])
                        (\os -> okC (o:os))
                )
    {-# INLINE many #-}

    some p   = Prim $ \noC okS noS okC ->
        runPrim p
            noC
            panicLoop
            noS
            (\o -> runPrim (many p)
                    noC
                    panicLoop
                    (\_ -> okC [o])
                    (\os -> okC (o:os))
            )
    {-# INLINE some #-}

instance Show e => Show (ErrorPrim e) where
    show ErrPrimNoMatch     = "ErrPrimNoMatch"
    show ErrPrimEOF         = "ErrPrimEOF"
    show ErrPrimLoop        = "ErrPrimLoop"
    show (ErrUser e)        = "ErrUser " ++ show e
