module Language.Moonbit.Lexer.Token where

import Data.ByteString.Lazy.Char8 (ByteString)
import GHC.Generics (Generic)
import Language.Moonbit.Data

-- ---------------------------------------------------------------------
-- MoonBit tokens
--
-- The following definitions mirror the token definitions found in
-- @src/lexing/tokens/tokens.mbt@.

type CharLiteral = ByteString

type StringLiteral = ByteString

-- It is better to seperate the interpolation into interpElems
type InterpLiteral = ByteString

-- | Tokens recognised by the MoonBit lexer.  Where the original
-- token carried a parameter (for example numeric or identifier
-- tokens) the corresponding constructor here carries the same
-- parameter type.
data Tk
  = CHAR CharLiteral
  | INT ByteString
  | BYTE CharLiteral
  | BYTES StringLiteral
  | FLOAT ByteString
  | DOUBLE ByteString
  | STRING StringLiteral
  | MULTILINE_STRING ByteString
  | MULTILINE_INTERP InterpLiteral
  | INTERP InterpLiteral
  | ATTRIBUTE (ByteString, Maybe String, String)
  | LIDENT ByteString
  | UIDENT ByteString
  | POST_LABEL ByteString
  | COMMENT_TOK Comment
  | NEWLINE
  | INFIX1 ByteString
  | INFIX2 ByteString
  | INFIX3 ByteString
  | INFIX4 ByteString
  | AUGMENTED_ASSIGNMENT ByteString
  | EOF
  | FALSE
  | TRUE
  | PUB
  | PRIV
  | READONLY
  | IMPORT
  | EXTERN
  | BREAK
  | CONTINUE
  | STRUCT
  | ENUM
  | TRAIT
  | DERIVE
  | IMPL
  | WITH
  | RAISE
  | THROW
  | TRY
  | CATCH
  | ASYNC
  | TYPEALIAS
  | TRAITALIAS
  | FNALIAS
  | EQUAL
  | LPAREN
  | RPAREN
  | COMMA
  | MINUS
  | QUESTION
  | EXCLAMATION
  | DOT_LIDENT ByteString
  | DOT_UIDENT ByteString
  | DOT_INT Int
  | DOT_LPAREN
  | COLONCOLON
  | COLON
  | SEMI Bool
  | LBRACKET
  | PLUS
  | RBRACKET
  | UNDERSCORE
  | BAR
  | LBRACE
  | RBRACE
  | AMPERAMPER
  | AMPER
  | CARET
  | BARBAR
  | PACKAGE_NAME ByteString
  | AS
  | PIPE
  | ELSE
  | FN
  | IF
  | LET
  | CONST
  | MATCH
  | USING
  | MUTABLE
  | TYPE
  | FAT_ARROW
  | THIN_ARROW
  | WHILE
  | RETURN
  | DOTDOT
  | RANGE_INCLUSIVE
  | RANGE_EXCLUSIVE
  | ELLIPSIS
  | TEST
  | LOOP
  | GUARD
  | DEFER
  | FOR
  | IN
  | IS
  | SUBERROR
  | AND
  | LETREC
  | ENUMVIEW
  | NORAISE
  deriving (Show, Eq, Generic)

-- | Comments that can appear in the source code.  MoonBit tracks
-- whether a comment is inline trailing or located on its own
-- line along with whether a docstring has consumed it.  In
-- addition to the content of the comment, the 'kind' and
-- 'consumedByDocstring' fields capture this metadata.
data Comment = Comment
  { content :: ByteString,
    kind :: CommentKind
    -- consumedByDocstring :: Bool
  }
  deriving (Show, Eq, Generic)

-- | The two flavours of comments in MoonBit.  An inline trailing
-- comment has no additional metadata.  An ownline comment records
-- whether there is a blank line before or after it.  These flags
-- correspond to the fields 'leading_blank_line' and
-- 'trailing_blank_line' in the original definition.
data CommentKind
  = InlineTrailing
  | Doc
  | Ownline
  -- { leadingBlankLine :: Bool,
  --   trailingBlankLine :: Bool
  -- }
  deriving (Show, Eq, Generic)

type Token = Located Tk
