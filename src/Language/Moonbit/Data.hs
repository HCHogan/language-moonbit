module Language.Moonbit.Data where

import GHC.Generics (Generic)

data Position = Position
  { abs :: {-# UNPACK #-} !Int, -- Absolute position in the source code
    row :: {-# UNPACK #-} !Int,
    col :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq, Generic)

data Span = Span
  { lo :: !Position, -- Start position of the span
    hi :: !Position -- End position of the span
  }
  deriving (Show, Eq, Generic)

data Located a = Located
  { range :: !Span,
    unLoc :: !a
  }
  deriving (Show, Eq, Generic)
