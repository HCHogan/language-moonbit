{
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Language.Moonbit.Lexer 
  ( -- * Invoking Alex
    Alex
  , AlexPosn (..)
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
<0> "=>" { tok TkFatArrow }
<0> "->" { tok TkThinArrow }


{
data AlexUserState = AlexUserState
  { nestLevel :: Int
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  }

mkPosition :: AlexPosn -> Position
mkPosition (AlexPn abs row col) = Position {..}

mkSpan :: AlexInput -> Int -> Span
mkSpan (start, _, str, _) len =
  let lo = mkPosition start
      hi = mkPosition (BS.foldl' alexMove start $ BS.take (fromIntegral len) str)
  in Span{..}

alexEOF :: Alex Token
alexEOF = do
  (alexposn, _, _, _) <- alexGetInput
  let pos = mkPosition alexposn
  pure (Located (Span pos pos) TkEof)

tok :: Tk -> AlexAction Token
tok ctor inp len = do
  let span = mkSpan inp (fromIntegral len)
  pure $ Located span ctor

mkAction :: (ByteString -> a) -> (a -> Tk) -> AlexAction Token
mkAction f ctor inp@(_,_,str,_) len =
  let bs = BS.take (fromIntegral len) str
  in pure $ Located (mkSpan inp (fromIntegral len)) (ctor (f bs))

tokUIdent :: AlexAction Token
tokUIdent = mkAction id TkUIdent

tokLIdent :: AlexAction Token
tokLIdent = mkAction id TkLIdent

tokChar :: AlexAction Token
tokChar = mkAction id TkChar

tokInt :: AlexAction Token
tokInt = mkAction id TkInt

tokByte :: AlexAction Token
tokByte = mkAction id TkByte

tokBytes :: AlexAction Token
tokBytes = mkAction id TkBytes

tokFloat :: AlexAction Token
tokFloat = mkAction id TkFloat

tokDouble :: AlexAction Token
tokDouble = mkAction id TkDouble

tokString :: AlexAction Token
tokString = mkAction id TkString

-- tokMultilineString :: AlexAction Token
-- tokMultilineString = mkAction id TkMultilineString

tokMultilineInterp :: AlexAction Token
tokMultilineInterp = mkAction id TkMultilineInterp

tokInterp :: AlexAction Token
tokInterp = mkAction id TkInterp

scanMany :: ByteString -> Either String [Token]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if unLoc output == TkEof
        then pure [output]
        else (output :) <$> go


}
