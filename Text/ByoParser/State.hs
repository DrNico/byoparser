
{-|
Module          : Text.ByoParser.State
Description     : Parser states: user, location, layout
Copyright       : (c) 2016 Nicolas Godbout
License         : MIT
Maintainer      : nicolas.godbout@gmail.com
Stability       : unstable
-}
module Text.ByoParser.State (
    PosState(..),
    SourcePos(..),
    incrLine,
    incrCol
    -- incrChar,
    -- incrChars
) where

import Data.Bool
import Data.Monoid

import Prelude ( Int, Char, Eq(..), Num(..), otherwise )

-- | Data type representing the position, i.e. line and column number,
--   in a source file.
data SourcePos = SourcePos {
line        :: !Int,
column      :: !Int
}

-- | Increment the line number by one and reset the column to zero.
incrLine :: SourcePos -> SourcePos
incrLine (SourcePos l c) = SourcePos (l + 1) 0

-- | Increment the column number by one.
incrCol :: SourcePos -> SourcePos
incrCol (SourcePos l c) = SourcePos l (c + 1)


-----
-- State storing the SourcePos data
-----

{-|
Data type to use as a parser State for tracking source location.
-}
data PosState u = PosState {
pos         :: !Int,
loc         :: !SourcePos,
userState   :: !u
}

-----
-- Instances
-----

instance Eq SourcePos where
    SourcePos l1 c1 == SourcePos l2 c2
        = l1 == l2 && c1 == c2

instance Monoid SourcePos where
    mempty      = SourcePos 0 0

    mappend (SourcePos l1 c1) (SourcePos l2 c2)
        | l2 == 0   = SourcePos l1 (c1 + c2)
        | otherwise = SourcePos (l1 + l2) c2
