
{-|
Module          : Text.ByoParser.Error
Description     : Error types and handling strategies for parsers
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable
-}
module Text.ByoParser.Error (
    (<?>)
) where

import Text.ByoParser.Prim      ( ParserPrim(..), ErrorPrim(..) )

{-|
Attach to a parser a message indicating what was expected in case of
error.
-}
(<?>) :: ParserPrim i e s r a -> e -> ParserPrim i e s r a
p <?> msg = Prim $ \noC okS noS okC ->
    runPrim p
        noC
        okS
        (\_ -> noS (ErrUser msg))
        okC
