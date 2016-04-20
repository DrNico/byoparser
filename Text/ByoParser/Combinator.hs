
{-|
Module          : Text.ByoParser.Combinator
Description     : Parser combinators
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable

Parser combinators take parser arguments to generate modified parsers.

All combinators except 'try' are /not/ back-tracking. If such a parser
fails /after consuming input/, then the whole parse fails unless it
is enclosed within a 'try' parser.
-}
module Text.ByoParser.Combinator (
    try,
    count,
    option,
    optionMaybe,
    optional,
    skipSome,
    lookAhead,
    sepBy1
) where

import Control.Applicative  ( many )
import Control.Monad        ( (>>), return )
import Data.Maybe

import Text.ByoParser.Prim  ( ParserPrim(..), ErrorPrim(..) )

import Prelude ( ($), Int, Num(..), (>), undefined, otherwise )

{-|
Wrap a non-backtracking parser to make it backtracking. If the supplied
parser fails after consuming input, the input stream is reset, returning
to a parse state as if failure occured without consuming any input. This
parser succeeds if and only if the wraped parser succeeds.
-}
try :: ParserPrim i e s r o -> ParserPrim i e s r o
try p = Prim $ \noC okS noS okC ->
    \i s -> runPrim p
        (\e _ _ -> noS e i s)
        okS
        noS
        okC
        i s
{-# INLINE try #-}

{-|
Runs the supplied parser exactly /n/ times. If /n/ is negative or zero,
succeeds and produces the empty list.

This parser does not backtrack and will stop parsing if the supplied parser
consumes output before reaching /n/ iterations. Use within 'try' to recover.
-}
count :: Int -> ParserPrim i e s r o -> ParserPrim i e s r [o]
count n p = count_p n
    where
    count_p n
        | n > 0 = Prim $ \noC okS noS okC ->
            runPrim p
                noC
                (\_ -> noS ErrPrimLoop)
                noS
                (\o -> runPrim (count_p (n - 1))
                        noC
                        (\_ -> noS ErrPrimLoop)
                        noC
                        (\os -> okC (o:os))
                )
        | otherwise =
            return []
{-# INLINE count #-}

{-|
Run the supplied parser. If it fails without consuming any input,
succeed and produce the supplied value instead.
-}
option :: o -> ParserPrim i e s r o -> ParserPrim i e s r o
option x p = Prim $ \noC okS noS okC ->
    runPrim p
        noC
        okS
        (\_ -> okS x)
        okC
{-# INLINE option #-}


{-|
Run the supplied parser. If it succeeds, produce 'Just' the output.
If it fails without consuming any input, produce 'Nothing'.
-}
optionMaybe :: ParserPrim i e s r o -> ParserPrim i e s r (Maybe o)
optionMaybe p = Prim $ \noC okS noS okC ->
    runPrim p
        noC
        (\o -> okS $ Just o)
        (\_ -> okS Nothing)
        (\o -> okC $ Just o)
{-# INLINE optionMaybe #-}

{-|
Run the supplied parser. If it succeeds, ignore its output and produce
the unit. If it fails without consuming any input, ignore the failure
and return the unit.
-}
optional :: ParserPrim i e s r o -> ParserPrim i e s r ()
optional p = Prim $ \noC okS noS okC ->
    runPrim p
        noC
        (\_ -> okS ())
        (\_ -> okS ())
        (\_ -> okC ())
{-# INLINE optional #-}

{-|
Run the supplied parser at least once, then as many times as it
succeeds, ignoring the output and producing the unit.
-}
skipSome :: ParserPrim i e s r o -> ParserPrim i e s r ()
skipSome p = Prim $ \noC okS noS okC ->
    runPrim p
        noC
        (\_ -> noS ErrPrimLoop)
        noS
        (\o -> runPrim (many p)
                noC
                (\_ -> noS ErrPrimNoMatch)
                (\_ -> okC ())
                (\_ -> okC ())
        )
{-# INLINE skipSome #-}

{-|
Runs the supplied parser without consuming any input. This parser
succeeds if and only if the supplied parser succeeds.

This is a backtracking parser. If the supplied parser consumes input, the
stream is reset to the starting state.

/Warning/ : Changes to the parse state @s@ performed by the supplied parser
are /not/ reset, both on succes and failure.
-}
lookAhead :: ParserPrim i e s r o -> ParserPrim i e s r o
lookAhead p = Prim $ \noC okS noS okC ->
    \i -> runPrim p
        (\e _ -> noS e i)
        okS
        noS
        (\o _ -> okS o i)
        i
{-# INLINE lookAhead #-}

{-|
Match the first parser one or more times, while matching the second
parser in between and ignoring its output.
-}
sepBy1 :: ParserPrim i e s r o -> ParserPrim i e s r w
       -> ParserPrim i e s r [o]
sepBy1 p s = do
    r <- p
    rs <- many (s >> p)
    return (r : rs)
