# byoparser: Build your own Parser

The solution to your parsing needs: Build your own Parser! This Haskell library
provides several variants for each component of a typical parser: input stream
type, stream matchers, combinators, parser state, error reporting and recovery,
and results generation. Combine the library components to get the parser _you_
need.

  * Input stream types: String, Text, and three variants for ByteString,
    Word8, ASCII Char or UTF8-encoded Char.
  * Parser state: optional tracking of source locations, optional
    tracking of layout, or simply provide your own parser state
  * Parser combinators: non-backtracking by default, with back-tracking
    capabilities
  * Error reporting: plug-in your own error type, recovery mechanism for
    parse errors
  * Results generation: optional partial results, monadic actions on results,
    lazy token stream generation
