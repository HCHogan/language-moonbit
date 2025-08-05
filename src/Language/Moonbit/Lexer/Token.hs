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
  = TkChar CharLiteral
  | TkInt ByteString
  | TkByte CharLiteral
  | TkBytes StringLiteral
  | TkFloat ByteString
  | TkDouble ByteString
  | TkString StringLiteral
  | TkMultilineString ByteString
  | TkMultilineInterp InterpLiteral
  | TkInterp InterpLiteral
  | TkAttribute (ByteString, Maybe String, String)
  | TkLIdent ByteString
  | TkUIdent ByteString
  | TkPostLabel ByteString
  | TkCommentTok Comment
  | TkNewline
  | TkInfix1 ByteString
  | TkInfix2 ByteString
  | TkInfix3 ByteString
  | TkInfix4 ByteString
  | TkAugmentedAssignment ByteString
  | TkEof
  | TkFalse
  | TkTrue
  | TkPub
  | TkPriv
  | TkReadonly
  | TkImport
  | TkExtern
  | TkBreak
  | TkContinue
  | TkStruct
  | TkEnum
  | TkTrait
  | TkDerive
  | TkImpl
  | TkWith
  | TkRaise
  | TkThrow
  | TkTry
  | TkCatch
  | TkAsync
  | TkTypealias
  | TkTraitalias
  | TkFnalias
  | TkEqual
  | TkLParen
  | TkRParen
  | TkComma
  | TkMinus
  | TkQuestion
  | TkExclamation
  | TkDotLIdent ByteString
  | TkDotUIdent ByteString
  | TkDotInt Int
  | TkDotLParen
  | TkColonColon
  | TkColon
  | TkSemi Bool
  | TkLBracket
  | TkPlus
  | TkRBracket
  | TkUnderscore
  | TkBar
  | TkLBrace
  | TkRBrace
  | TkAmperAmper
  | TkAmper
  | TkCaret
  | TkBarBar
  | TkPackageName ByteString
  | TkAs
  | TkPipe
  | TkElse
  | TkFn
  | TkIf
  | TkLet
  | TkConst
  | TkMatch
  | TkUsing
  | TkMutable
  | TkType
  | TkFatArrow
  | TkThinArrow
  | TkWhile
  | TkReturn
  | TkDotDot
  | TkRangeInclusive
  | TkRangeExclusive
  | TkEllipsis
  | TkTest
  | TkLoop
  | TkGuard
  | TkDefer
  | TkFor
  | TkIn
  | TkIs
  | TkSuberror
  | TkAnd
  | TkLetrec
  | TkEnumview
  | TkNoraise
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
