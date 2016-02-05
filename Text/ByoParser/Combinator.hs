
{-|
Module          : Text.ByoParser.Combinator
Description     : Parser combinators
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : provisional

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
    lookAhead
) where

import Control.Applicative  ( many )
import Control.Monad        ( return )
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
                (\_ -> noS undefined)  -- TODO flag accepts empty input
                noS
                (\o -> runPrim (count_p (n - 1))
                        noC
                        (\_ -> noS undefined) -- TODO idem
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
        (\_ -> noS undefined) -- TODO flag empty match
        noS
        (\o -> runPrim (many p)
                noC
                (\_ -> noS undefined)
                (\_ -> okC ())
                (\_ -> okC ())
        )
{-# INLINE skipSome #-}

{-|
Runs the supplied parser without consuming any input. This parser
succedes if and only if the supplied parser succeeds.
-}
lookAhead :: ParserPrim i e s r o -> ParserPrim i e s r o
lookAhead p = Prim $ \noC okS noS okC ->
    \s i -> runPrim p
        noC
        okS
        noS
        (\o _ _ -> okS o s i)
        s i
{-# INLINE lookAhead #-}
