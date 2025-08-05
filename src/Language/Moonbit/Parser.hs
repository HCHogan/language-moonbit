{-# LANGUAGE OverloadedStrings #-}
module Language.Moonbit.Parser where

import Language.Moonbit.Lexer

-- >>> scanMany "=>"
-- Right [Located {range = Span {lo = Position {abs = 0, row = 1, col = 1}, hi = Position {abs = 2, row = 1, col = 3}}, unLoc = TkFatArrow},Located {range = Span {lo = Position {abs = 2, row = 1, col = 3}, hi = Position {abs = 2, row = 1, col = 3}}, unLoc = TkEof}]

