{-# LANGUAGE LambdaCase #-}

module Language.Moonbit.Lexer (
  reserved,
  reservedOp,
  identifier,
  integer,
  parens,
  semiSep,
  semi,
  contents,
  ReservedWord (..),
  ReservedOp (..),
  Op,
  Operators,
  stringLit,
  braces,
  brackets,
  whiteSpace,
  commaSep,
  symbol,
  slash,
  commaSep1,
) where

import Data.Functor.Identity
import Data.Text.Lazy qualified as L
import Text.Parsec
import Text.Parsec.Expr qualified as Ex
import Text.Parsec.Text.Lazy
import Text.Parsec.Token qualified as Tok

type Op a = Ex.Operator L.Text () Identity a
type Operators a = Ex.OperatorTable L.Text () Identity a

-- | All of the reserved words in the language
data ReservedWord
  = RWType
  | RWSuberror
  | RWTypealias
  | RWStruct
  | RWEnum
  | RWLet
  | RWConst
  | RWFn
  | RWTest
  | RWTrait
  | RWTraitalias
  | RWImpl
  | RWExtern
  | RWAsync
  | RWMatch
  | RWIf
  | RWElse
  | RWFor
  | RWIn
  | RWWhile
  | RWBreak
  | RWContinue
  | RWReturn
  | RWRaise
  | RWNoRaise
  | RWRaisePoly
  | RWTrue
  | RWFalse
  | RWAs
  | RWIs
  | RWTry
  | RWCatch
  | RWImport
  | RWPackage
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | All of the reserved operator symbols in the language
data ReservedOp
  = OpPlus -- "+"
  | OpMinus -- "-"
  | OpTimes -- "*"
  | OpDivide -- "/"
  | OpMod -- "%"
  | OpEqEq -- "=="
  | OpNotEq -- "!="
  | OpGte -- ">="
  | OpLte -- "<="
  | OpGt -- ">"
  | OpLt -- "<"
  | OpAndAnd -- "&&"
  | OpOrOr -- "||"
  | OpPipeGt -- "|>"
  | OpShiftL -- "<<"
  | OpShiftR -- ">>"
  | OpDotDotEq -- "..="
  | OpDotDotLt -- "..<"
  | OpEq -- "="
  | OpArrow -- "->"
  | OpColonColon -- "::"
  | OpQuestion -- "?"
  | OpExclamation -- "!"
  | OpDot -- "."
  | OpDotDot -- ".."
  | OpColon -- ":"
  | OpTilde -- "~"
  deriving (Eq, Ord, Show, Enum, Bounded)

reservedWordToString :: ReservedWord -> String
reservedWordToString = \case
  RWType -> "type"
  RWSuberror -> "suberror"
  RWTypealias -> "typealias"
  RWStruct -> "struct"
  RWEnum -> "enum"
  RWLet -> "let"
  RWConst -> "const"
  RWFn -> "fn"
  RWTest -> "test"
  RWTrait -> "trait"
  RWTraitalias -> "traitalias"
  RWImpl -> "impl"
  RWExtern -> "extern"
  RWAsync -> "async"
  RWMatch -> "match"
  RWIf -> "if"
  RWElse -> "else"
  RWFor -> "for"
  RWIn -> "in"
  RWWhile -> "while"
  RWBreak -> "break"
  RWContinue -> "continue"
  RWReturn -> "return"
  RWRaise -> "raise"
  RWNoRaise -> "noraise"
  RWRaisePoly -> "raise?"
  RWTrue -> "true"
  RWFalse -> "false"
  RWAs -> "as"
  RWIs -> "is"
  RWTry -> "try"
  RWCatch -> "catch"
  RWImport -> "import"
  RWPackage -> "package"

reservedOpToString :: ReservedOp -> String
reservedOpToString = \case
  OpPlus -> "+"
  OpMinus -> "-"
  OpTimes -> "*"
  OpDivide -> "/"
  OpMod -> "%"
  OpEqEq -> "=="
  OpNotEq -> "!="
  OpGte -> ">="
  OpLte -> "<="
  OpGt -> ">"
  OpLt -> "<"
  OpAndAnd -> "&&"
  OpOrOr -> "||"
  OpPipeGt -> "|>"
  OpShiftL -> "<<"
  OpShiftR -> ">>"
  OpDotDotEq -> "..="
  OpDotDotLt -> "..<"
  OpEq -> "="
  OpArrow -> "->"
  OpColonColon -> "::"
  OpQuestion -> "?"
  OpExclamation -> "!"
  OpDot -> "."
  OpDotDot -> ".."
  OpColon -> ":"
  OpTilde -> "~"

reservedWords :: [String]
reservedWords = reservedWordToString <$> [minBound .. maxBound]

reservedOps :: [String]
reservedOps = reservedOpToString <$> [minBound .. maxBound]

lexer :: Tok.GenTokenParser L.Text () Identity
lexer =
  Tok.makeTokenParser $
    Tok.LanguageDef
      { Tok.commentStart = ""
      , Tok.commentEnd = ""
      , Tok.commentLine = "//"
      , Tok.nestedComments = False
      , Tok.identStart = letter <|> char '_'
      , Tok.identLetter = alphaNum <|> oneOf "_'<>"
      , Tok.opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
      , Tok.reservedNames = reservedWords
      , Tok.reservedOpNames = reservedOps
      , Tok.caseSensitive = True
      }

reserved :: ReservedWord -> Parser ()
reserved w = Tok.reserved lexer (reservedWordToString w)

reservedOp :: ReservedOp -> Parser ()
reservedOp o = Tok.reservedOp lexer (reservedOpToString o)

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semi :: Parser String
semi = Tok.semi lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

integer :: Parser Integer
integer = Tok.natural lexer

stringLit :: Parser String
stringLit = Tok.stringLiteral lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer

slash :: Parser String
slash = symbol "/"

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof -- we don't need Tok.whiteSpace here because the parser already consumes trailing whitespace
  return r
