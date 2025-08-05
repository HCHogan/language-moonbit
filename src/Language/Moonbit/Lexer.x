{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Moonbit.Lexer 
  ( -- * Invoking Alex
    Alex
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan
  , scanMany
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Moonbit.Lexer.Token
import Language.Moonbit.Data

}

%wrapper "monadUserState-bytestring"

$ascii             = [\x00-\x7F]
$digit             = [0-9]
$qua_digit         = [0-3]
$oct_digit         = [0-7]
$hex_digit         = [0-9A-Fa-f]
$lower             = [a-z]
$upper             = [A-Z]
$valid_unicode_char = [^\uD800-\uDFFF]

@newline           = (\r\n|\n|\r|\u2028|\u2029)
@hexadecimal       = 0[xX]$hex_digit($hex_digit|_)*
@octal             = 0[oO]$oct_digit($oct_digit|_)*
@binary            = 0[bB][01]([01]|_)*
@decimal           = $digit($digit|_)*
@integer_literal   = (@decimal|@hexadecimal|@octal|@binary)(UL|U|L|N)?

@double_dec        = @decimal\.($digit|_)*([eE][\+\-]?@decimal)?
@double_hex        = @hexadecimal\.($hex_digit|_)*([pP][\+\-]?@decimal)?
@double_literal    = (@double_dec|@double_hex)

@float_dec         = @decimal\.($digit|_)*([eE][\+\-]?@decimal)?F
@float_hex         = @hexadecimal\.($hex_digit|_)*([pP][\+\-]?@decimal)?F
@float_literal     = (@float_dec|@float_hex)

@identifier        = ($lower|$upper|_)(($lower|$upper|$digit|_)*)

$unicode_id_char = [\
    \x0030-\x0039\
    \x0041-\x005A\
    _\
    \x0061-\x007A\   
    \x00A1-\x00AC\   
    \x00AE-\x02AF\   
    \x1100-\x11FF\   
    \x1E00-\x1EFF\   
    \x2070-\x209F\   
    \x2150-\x218F\   
    \x2E80-\x2EFF\   
    \x2FF0-\x2FFF\   
    \x3001-\x30FF\   
    \x31C0-\x9FFF\   
    \xAC00-\xD7FF\   
    \xF900-\xFAFF\   
    \xFE00-\xFE0F\   
    \xFE30-\xFE4F\   
    \x1F000-\x1FBFF\ 
    \x20000-\x2A6DF\ 
    \x2A700-\x2EBEF\ 
    \x2F800-\x2FA1F\ 
    \x30000-\x323AF\ 
    \xE0100-\xE01EF  
]

$whitespace = [\
    \x0009\  
    \x000B\  
    \x000C\  
    \x0020\  
    \x00A0\  
    \xFEFF\  
    \x1680\  
    \x2000-\x200A\ 
    \x202F\  
    \x205F\  
    \x3000   
]

tokens :-

<0> $whitespace+ ;

{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }

mkPosition :: AlexPosn -> Position
mkPosition (AlexPosition abs row col) = Position {..}

mkSpan :: AlexInput -> Int -> Span
mkSpan (start, _, str, _) len =
  let lo = mkPosition start
      hi = mkPosition (BS.foldl' alexMove start $ BS.take len str)
  in Span{..}
}
