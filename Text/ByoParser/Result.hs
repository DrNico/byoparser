{-# LANGUAGE
        TypeFamilies
  #-}

{-|
Module          : Text.ByoParser.Result
Description     : Class of parsing results and drivers
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable
-}
module Text.ByoParser.Result (
    -- * Partial results
    PartialResult(..),
    parsePartial,
    parsePartialM,
    feed,
    break
    -- Lazy results streams
) where

import Control.Monad            ( Monad(..) )
-- import Control.Monad.Identity   ( Identity )

import Text.ByoParser.Prim      ( ParserPrim(..), ResultPrim(..), ErrorPrim )
import Text.ByoParser.Stream    ( ByoStream(..), endOfInput )

import Prelude ( ($), undefined, error )

{-|
Data type capable of expressing a partial parse result within a 'Monad'.
This data type may be used as the result @r@ type of 'ParserPrim'.
-}
data PartialResult i e r
    = PDone r
    | PFail (ErrorPrim e)
    | PMore (i -> PartialResult i e r)

{-|
Execute a 'ParserPrim' parser which produces a 'PartialResult' on the given
input stream and initial parser state.
-}
parsePartial :: ParserPrim i e s (PartialResult i e r) r
             -> i -> s -> PartialResult i e r
parsePartial p i s =
    case runPrim p no ok no ok i s of
        ResPrimFail e -> PFail e
        ResPrimDone r -> r
    where
        no e _ _ = ResPrimFail e
        ok r _ _ = ResPrimDone (PDone r)

{-|
Feed more input to a 'PartialResult'.
-}
feed :: PartialResult i e r -> i
     -> PartialResult i e r
feed (PDone r) _ = PDone r
feed (PFail e) _ = PFail e
feed (PMore f) i = f i

{-|
Parse the given stream with the given initial parsing state, and extract
a 'PartialResult' within the associated 'Monad'.
-}
parsePartialM :: Monad m
              => ParserPrim i e s (PartialResult i e r) r
              -> i -> s -> m (PartialResult i e r)
parsePartialM = undefined

{-|
If the input stream is empty, escape to a 'PartialResult', allowing
the caller of the parser to supply more input. Otherwise, this parser does nothing.
-}
break :: ByoStream i => ParserPrim i e s (PartialResult i e r) ()
break = Prim $ \noC okS noS okC ->
    runPrim endOfInput
        (error "unpossible! 'endOfInput' consumed input")
        (\_ i s -> ResPrimDone $ PMore
            (\i -> case okS () i s of
                ResPrimFail e -> PFail e
                ResPrimDone r -> r
            )
        )
        (\_ -> okS ())
        (error "unpossible! 'endOfInput' consumed input")
{-# INLINE break #-}

{-
Insert a monadic action into a parser. Whenever the resulting parser is
run, the monadic action is executed.
-}
-- TODO requires the Monad to live in ParserPrim
-- runM :: Monad m => m o -> ParserPrim i e s (PartialResult i e m r) o
-- runM m = Prim $ \noC okS noS okC ->
--     \i s -> m >>= \o -> okS o i s
