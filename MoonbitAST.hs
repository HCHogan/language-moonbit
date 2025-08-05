{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      :  MoonbitAST
-- Description :  Haskell representation of the MoonBit (mbt) and MBTI
--                tokens and abstract syntax trees (AST).  The data
--                structures defined here are a straight–forward
--                translation of the definitions found in the original
--                MoonBit parser sources.  All constructors present
--                in the source language are preserved to make it
--                convenient to write lexers and parsers using tools
--                such as Alex and Happy.  In addition to the
--                language specific types, common infrastructure such
--                as positions, locations, comments and attributes are
--                provided.
--
-- The overall goal of this file is to provide a clear and
-- maintainable representation of the syntax tree that closely
-- follows the original project while adopting modern Haskell
-- conventions.  Optional fields in the MoonBit definitions become
-- 'Maybe' values, lists become ordinary lists, and various small
-- enumerations become algebraic data types.  Record syntax is used
-- throughout to enhance readability and facilitate future
-- maintenance.

module MoonbitAST where

import           GHC.Generics     (Generic)

-- | Character and string literal types from the original MoonBit
-- implementation.  In the source these aliases are ultimately
-- represented as UTF‑8 encoded strings.  They are defined here as
-- simple type synonyms to avoid unnecessary wrapping.
type CharLiteral   = String
type StringLiteral = String
type ByteLiteral   = String
type BytesLiteral  = String

-- | An interpolation literal consists of a sequence of either
-- literal segments or nested expressions.  For MoonBit we model
-- interpolation elements as a list of 'InterpElem's, which are
-- defined further down in the file.  The MBTI variant refers to
-- the same concept but names the type differently; both map to a
-- list in Haskell.
type InterpLiteral = [InterpElem]

-- | Source positions track a single point in a source file.  The
-- fields correspond directly to those in the original code:
--
-- * 'fname' – the file name
-- * 'lnum'  – the one‑based line number
-- * 'bol'   – the byte offset of the beginning of the current line
-- * 'cnum'  – the byte offset from the beginning of the file
--
-- Together these values allow both columns and character spans to
-- be computed.  Equality and ordering are derived to support
-- comparisons in the parser.
data Position = Position
  { fname :: String
  , lnum  :: Int
  , bol   :: Int
  , cnum  :: Int
  } deriving (Show, Eq, Ord, Generic)

-- | A 'Location' records the inclusive start and exclusive end
-- positions of a syntactic element.  The two fields mirror the
-- MoonBit definition exactly.  Instances for 'Eq' and 'Show' are
-- provided automatically.
data Location = Location
  { start :: Position
  , end   :: Position
  } deriving (Show, Eq, Ord, Generic)

-- | Comments that can appear in the source code.  MoonBit tracks
-- whether a comment is inline trailing or located on its own
-- line along with whether a docstring has consumed it.  In
-- addition to the content of the comment, the 'kind' and
-- 'consumedByDocstring' fields capture this metadata.
data Comment = Comment
  { content              :: String
  , kind                 :: CommentKind
  , consumedByDocstring  :: Bool
  } deriving (Show, Eq, Generic)

-- | The two flavours of comments in MoonBit.  An inline trailing
-- comment has no additional metadata.  An ownline comment records
-- whether there is a blank line before or after it.  These flags
-- correspond to the fields 'leading_blank_line' and
-- 'trailing_blank_line' in the original definition.
data CommentKind
  = InlineTrailing
  | Ownline
      { leadingBlankLine  :: Bool
      , trailingBlankLine :: Bool
      }
  deriving (Show, Eq, Generic)

-- | Attributes are user supplied annotations that can appear on
-- various declarations.  They consist of a raw string, a parsed
-- representation and a location.  The structure here mirrors the
-- original MoonBit definition.  The 'parsed' field is optional
-- since ill‑formed attributes may be left as raw strings.
data Attribute = Attribute
  { loc    :: Location
  , raw    :: String
  , parsed :: Maybe AttributeExpr
  } deriving (Show, Eq, Generic)

-- | An attribute identifier may be qualified or unqualified.  The
-- optional qualification corresponds to the 'qual' field in the
-- source definition.
data AttributeId = AttributeId
  { qual :: Maybe String
  , name :: String
  } deriving (Show, Eq, Generic)

-- | The attribute expression language allows identifiers, string
-- literals and applications with a list of properties.  In
-- Haskell we model the expression as an algebraic data type with
-- constructors for each variant.
data AttributeExpr
  = AttrIdent AttributeId
  | AttrStr String
  | AttrApply AttributeId [AttributeProp]
  deriving (Show, Eq, Generic)

-- | Properties appearing within an attribute application.  A
-- property can either be labeled (as in @l = expr@) or a bare
-- expression.
data AttributeProp
  = AttrLabeled String AttributeExpr
  | AttrExpr AttributeExpr
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------
-- MoonBit tokens
--
-- The following definitions mirror the token definitions found in
-- @src/lexing/tokens/tokens.mbt@.  Each constructor corresponds to
-- a concrete token in the lexical grammar.  Comments in the source
-- code have been omitted here for brevity, but all original
-- constructors are preserved.  A separate 'TokenKind' type
-- categorises tokens into broader classes; it is useful when
-- implementing parsers with Happy.

-- | Tokens recognised by the MoonBit lexer.  Where the original
-- token carried a parameter (for example numeric or identifier
-- tokens) the corresponding constructor here carries the same
-- parameter type.
data Token
  = CHAR           CharLiteral
  | INT            String
  | BYTE           CharLiteral
  | BYTES          StringLiteral
  | FLOAT          String
  | DOUBLE         String
  | STRING         StringLiteral
  | MULTILINE_STRING   String
  | MULTILINE_INTERP   InterpLiteral
  | INTERP         InterpLiteral
  | ATTRIBUTE      (String, Maybe String, String)
  | LIDENT         String
  | UIDENT         String
  | POST_LABEL     String
  | COMMENT_TOK    Comment
  | NEWLINE
  | INFIX1         String
  | INFIX2         String
  | INFIX3         String
  | INFIX4         String
  | AUGMENTED_ASSIGNMENT String
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
  | DOT_LIDENT     String
  | DOT_UIDENT     String
  | DOT_INT        Int
  | DOT_LPAREN
  | COLONCOLON
  | COLON
  | SEMI           Bool
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
  | PACKAGE_NAME   String
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

-- | Coarse categories of tokens.  These values correspond to the
-- original 'TokenKind' enumeration.  Note that many tokens in
-- MoonBit fall into the same category; for example all forms of
-- identifier are represented by 'TK_LIDENT' or 'TK_UIDENT'.
data TokenKind
  = TK_CHAR
  | TK_INT
  | TK_BYTE
  | TK_BYTES
  | TK_FLOAT
  | TK_DOUBLE
  | TK_STRING
  | TK_MULTILINE_STRING
  | TK_MULTILINE_INTERP
  | TK_INTERP
  | TK_ATTRIBUTE
  | TK_LIDENT
  | TK_UIDENT
  | TK_POST_LABEL
  | TK_COMMENT
  | TK_NEWLINE
  | TK_INFIX1
  | TK_INFIX2
  | TK_INFIX3
  | TK_INFIX4
  | TK_AUGMENTED_ASSIGNMENT
  | TK_EOF
  | TK_FALSE
  | TK_TRUE
  | TK_PUB
  | TK_PRIV
  | TK_READONLY
  | TK_IMPORT
  | TK_EXTERN
  | TK_BREAK
  | TK_CONTINUE
  | TK_STRUCT
  | TK_ENUM
  | TK_TRAIT
  | TK_DERIVE
  | TK_IMPL
  | TK_WITH
  | TK_RAISE
  | TK_THROW
  | TK_TRY
  | TK_CATCH
  | TK_ASYNC
  | TK_TYPEALIAS
  | TK_TRAITALIAS
  | TK_FNALIAS
  | TK_EQUAL
  | TK_LPAREN
  | TK_RPAREN
  | TK_COMMA
  | TK_MINUS
  | TK_QUESTION
  | TK_EXCLAMATION
  | TK_DOT_LIDENT
  | TK_DOT_UIDENT
  | TK_DOT_INT
  | TK_DOT_LPAREN
  | TK_COLONCOLON
  | TK_COLON
  | TK_SEMI
  | TK_LBRACKET
  | TK_PLUS
  | TK_RBRACKET
  | TK_UNDERSCORE
  | TK_BAR
  | TK_LBRACE
  | TK_RBRACE
  | TK_AMPERAMPER
  | TK_AMPER
  | TK_CARET
  | TK_BARBAR
  | TK_PACKAGE_NAME
  | TK_AS
  | TK_PIPE
  | TK_ELSE
  | TK_FN
  | TK_IF
  | TK_LET
  | TK_CONST
  | TK_MATCH
  | TK_USING
  | TK_MUTABLE
  | TK_TYPE
  | TK_FAT_ARROW
  | TK_THIN_ARROW
  | TK_WHILE
  | TK_RETURN
  | TK_DOTDOT
  | TK_RANGE_INCLUSIVE
  | TK_RANGE_EXCLUSIVE
  | TK_ELLIPSIS
  | TK_TEST
  | TK_LOOP
  | TK_GUARD
  | TK_DEFER
  | TK_FOR
  | TK_IN
  | TK_IS
  | TK_SUBERROR
  | TK_AND
  | TK_LETREC
  | TK_ENUMVIEW
  | TK_NORAISE
  deriving (Show, Eq, Ord, Generic)

-- | Determine the broad category of a concrete token.  This
-- function follows the mapping found in @Token::kind@ in the
-- original source.  Only the pattern matches are shown; the
-- ordering is irrelevant because each constructor is matched
-- uniquely.
tokenKind :: Token -> TokenKind
tokenKind tok = case tok of
  CHAR _                 -> TK_CHAR
  INT  _                 -> TK_INT
  BYTE _                -> TK_BYTE
  BYTES _               -> TK_BYTES
  FLOAT _               -> TK_FLOAT
  DOUBLE _              -> TK_DOUBLE
  STRING _              -> TK_STRING
  MULTILINE_STRING _    -> TK_MULTILINE_STRING
  MULTILINE_INTERP _    -> TK_MULTILINE_INTERP
  INTERP _              -> TK_INTERP
  ATTRIBUTE _           -> TK_ATTRIBUTE
  LIDENT _              -> TK_LIDENT
  UIDENT _              -> TK_UIDENT
  POST_LABEL _          -> TK_POST_LABEL
  COMMENT_TOK _         -> TK_COMMENT
  NEWLINE               -> TK_NEWLINE
  INFIX1 _              -> TK_INFIX1
  INFIX2 _              -> TK_INFIX2
  INFIX3 _              -> TK_INFIX3
  INFIX4 _              -> TK_INFIX4
  AUGMENTED_ASSIGNMENT _ -> TK_AUGMENTED_ASSIGNMENT
  EOF                   -> TK_EOF
  FALSE                 -> TK_FALSE
  TRUE                  -> TK_TRUE
  PUB                   -> TK_PUB
  PRIV                  -> TK_PRIV
  READONLY              -> TK_READONLY
  IMPORT                -> TK_IMPORT
  EXTERN                -> TK_EXTERN
  BREAK                 -> TK_BREAK
  CONTINUE              -> TK_CONTINUE
  STRUCT                -> TK_STRUCT
  ENUM                  -> TK_ENUM
  TRAIT                 -> TK_TRAIT
  DERIVE                -> TK_DERIVE
  IMPL                  -> TK_IMPL
  WITH                  -> TK_WITH
  RAISE                 -> TK_RAISE
  THROW                 -> TK_THROW
  TRY                   -> TK_TRY
  CATCH                 -> TK_CATCH
  ASYNC                 -> TK_ASYNC
  TYPEALIAS             -> TK_TYPEALIAS
  TRAITALIAS            -> TK_TRAITALIAS
  FNALIAS               -> TK_FNALIAS
  EQUAL                 -> TK_EQUAL
  LPAREN                -> TK_LPAREN
  RPAREN                -> TK_RPAREN
  COMMA                 -> TK_COMMA
  MINUS                 -> TK_MINUS
  QUESTION              -> TK_QUESTION
  EXCLAMATION           -> TK_EXCLAMATION
  DOT_LIDENT _          -> TK_DOT_LIDENT
  DOT_UIDENT _          -> TK_DOT_UIDENT
  DOT_INT _             -> TK_DOT_INT
  DOT_LPAREN            -> TK_DOT_LPAREN
  COLONCOLON            -> TK_COLONCOLON
  COLON                 -> TK_COLON
  SEMI _                -> TK_SEMI
  LBRACKET              -> TK_LBRACKET
  PLUS                  -> TK_PLUS
  RBRACKET              -> TK_RBRACKET
  UNDERSCORE            -> TK_UNDERSCORE
  BAR                   -> TK_BAR
  LBRACE                -> TK_LBRACE
  RBRACE                -> TK_RBRACE
  AMPERAMPER            -> TK_AMPERAMPER
  AMPER                 -> TK_AMPER
  CARET                 -> TK_CARET
  BARBAR                -> TK_BARBAR
  PACKAGE_NAME _        -> TK_PACKAGE_NAME
  AS                    -> TK_AS
  PIPE                  -> TK_PIPE
  ELSE                  -> TK_ELSE
  FN                    -> TK_FN
  IF                    -> TK_IF
  LET                   -> TK_LET
  CONST                 -> TK_CONST
  MATCH                 -> TK_MATCH
  USING                 -> TK_USING
  MUTABLE               -> TK_MUTABLE
  TYPE                  -> TK_TYPE
  FAT_ARROW             -> TK_FAT_ARROW
  THIN_ARROW            -> TK_THIN_ARROW
  WHILE                 -> TK_WHILE
  RETURN                -> TK_RETURN
  DOTDOT                -> TK_DOTDOT
  RANGE_INCLUSIVE       -> TK_RANGE_INCLUSIVE
  RANGE_EXCLUSIVE       -> TK_RANGE_EXCLUSIVE
  ELLIPSIS              -> TK_ELLIPSIS
  TEST                  -> TK_TEST
  LOOP                  -> TK_LOOP
  GUARD                 -> TK_GUARD
  DEFER                 -> TK_DEFER
  FOR                   -> TK_FOR
  IN                    -> TK_IN
  IS                    -> TK_IS
  SUBERROR              -> TK_SUBERROR
  AND                   -> TK_AND
  LETREC                -> TK_LETREC
  ENUMVIEW              -> TK_ENUMVIEW
  NORAISE               -> TK_NORAISE

-- ---------------------------------------------------------------------
-- MBTI tokens
--
-- The MBTI lexer uses the same token set as MoonBit but with
-- slightly different payload types.  In particular, character
-- tokens and comments refer back into the 'syntax_types' module in
-- the original implementation.  Here we provide a separate token
-- type for MBTI so that parsers for both languages can coexist.

-- | Tokens for the MBTI signature language.  Most constructors
-- mirror those of 'Token' but the payload types are simplified to
-- plain Haskell types.  Comments reference the 'Comment' type
-- defined earlier.
data TokenMbti
  = MCHAR          String
  | MINT           String
  | MBYTE          String
  | MBYTES         String
  | MFLOAT         String
  | MDOUBLE        String
  | MSTRING        String
  | MMULTILINE_STRING String
  | MMULTILINE_INTERP [InterpElem]
  | MINTERP        [InterpElem]
  | MATTRIBUTE     (String, Maybe String, String)
  | MLIDENT        String
  | MUIDENT        String
  | MPOST_LABEL    String
  | MCOMMENT       Comment
  | MNEWLINE
  | MINFIX1        String
  | MINFIX2        String
  | MINFIX3        String
  | MINFIX4        String
  | MAUGMENTED_ASSIGNMENT String
  | MEOF
  | MFALSE
  | MTRUE
  | MPUB
  | MPRIV
  | MREADONLY
  | MIMPORT
  | MEXTERN
  | MBREAK
  | MCONTINUE
  | MSTRUCT
  | MENUM
  | MTRAIT
  | MDERIVE
  | MIMPL
  | MWITH
  | MRAISE
  | MTHROW
  | MTRY
  | MCATCH
  | MASYNC
  | MTYPEALIAS
  | MTRAITALIAS
  | MFNALIAS
  | MEQUAL
  | MLPAREN
  | MRPAREN
  | MCOMMA
  | MMINUS
  | MQUESTION
  | MEXCLAMATION
  | MDOT_LIDENT    String
  | MDOT_UIDENT    String
  | MDOT_INT       Int
  | MDOT_LPAREN
  | MCOLONCOLON
  | MCOLON
  | MSEMI          Bool
  | MLBRACKET
  | MPLUS
  | MRBRACKET
  | MUNDERSCORE
  | MBAR
  | MLBRACE
  | MRBRACE
  | MAMPERAMPER
  | MAMPER
  | MCARET
  | MBARBAR
  | MPACKAGE_NAME  String
  | MAS
  | MPIPE
  | MELSE
  | MFN
  | MIF
  | MLET
  | MCONST
  | MMATCH
  | MUSING
  | MMUTABLE
  | MTYPE
  | MFAT_ARROW
  | MTHIN_ARROW
  | MWHILE
  | MRETURN
  | MDOTDOT
  | MRANGE_INCLUSIVE
  | MRANGE_EXCLUSIVE
  | MELLIPSIS
  | MTEST
  | MLOOP
  | MGUARD
  | MDEFER
  | MFOR
  | MIN
  | MIS
  | MSUBERROR
  | MAND
  | MLETREC
  | MENUMVIEW
  | MNORAISE
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------
-- Core AST definitions
--
-- The following section defines the core abstract syntax tree for
-- MoonBit.  The translation closely follows the structure of
-- @src/parsing/syntax/ast.mbt@.  Because of the size and complexity
-- of the original AST, only the data declarations are given here.
-- Functions and helper methods from the original implementation are
-- omitted.  When optional fields appear in the MoonBit source they
-- become 'Maybe' values in Haskell; array types become lists.  The
-- names of fields and constructors mirror the original as closely
-- as possible while adhering to Haskell naming conventions.  When
-- a constructor name would conflict with a built‑in type (for
-- example "Bool") we prefix it with the type name to avoid
-- confusion.

-- | Visibility qualifiers for declarations.  A definition without
-- an explicit visibility annotation uses 'Default'.  Public and
-- private definitions carry an optional attribute string and a
-- location respectively.
data Visibility
  = Default
  | Pub
      { attr :: Maybe String
      , loc  :: Location
      }
  | Priv
      { loc  :: Location
      }
  deriving (Show, Eq, Generic)

-- | Literals of various primitive types.  The constructors are
-- prefixed with "Const" to avoid clashing with built‑in Haskell
-- type names.  Each variant corresponds to the same variant in
-- the original MoonBit 'Constant' enumeration.
data Constant
  = ConstBool     Bool
  | ConstByte     ByteLiteral
  | ConstBytes    BytesLiteral
  | ConstChar     CharLiteral
  | ConstInt      String
  | ConstInt64    String
  | ConstUInt     String
  | ConstUInt64   String
  | ConstFloat    String
  | ConstDouble   String
  | ConstString   StringLiteral
  | ConstBigInt   String
  deriving (Show, Eq, Generic)

-- | A simple label consists of a name and a location.  Many
-- constructs (record fields, loop labels, etc.) reuse this
-- structure.
data Label = Label
  { name :: String
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | Constructor names carry a string and a location.  They are
-- separate from 'Label' because they refer specifically to
-- type/constructor identifiers.
data ConstrName = ConstrName
  { name :: String
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | Long identifiers may either be simple identifiers or
-- qualified names of the form @pkg.id@.  This type is reused
-- throughout the syntax tree.
data LongIdent
  = Ident
      { name :: String
      }
  | Dot
      { pkg :: String
      , id  :: String
      }
  deriving (Show, Eq, Generic)

-- | Type names include an optional object flag that marks whether
-- the type refers to an object.  They wrap a 'LongIdent' along
-- with a location.
data TypeName = TypeName
  { name     :: LongIdent
  , isObject :: Bool
  , loc      :: Location
  } deriving (Show, Eq, Generic)

-- | Qualified constructor identifiers combine a 'LongIdent' with
-- a location.
data ConstrId = ConstrId
  { id  :: LongIdent
  , loc :: Location
  } deriving (Show, Eq, Generic)

-- | MoonBit types.  Many of the variants carry nested types or
-- additional metadata such as locations and error types.  All of
-- the constructors from the original 'Type' enumeration are
-- represented here.
data Type
  = TyAny
      { loc :: Location
      }
  | TyArrow
      { args     :: [Type]
      , res      :: Type
      , err      :: ErrorType
      , isAsync  :: Bool
      , loc      :: Location
      }
  | TyTuple
      { tys :: [Type]
      , loc :: Location
      }
  | TyName
      { constrId :: ConstrId
      , tys      :: [Type]
      , loc      :: Location
      }
  | TyOption
      { ty          :: Type
      , loc         :: Location
      , questionLoc :: Location
      }
  | TyObject
      { constrId :: ConstrId
      }
  deriving (Show, Eq, Generic)

-- | Error types annotate functions with potential error returns.
-- MoonBit distinguishes between explicit error types, default
-- error types, the absence of an error type, a "noraise" flag and
-- a "maybe error" constructor.  This enumeration follows the
-- original closely.
data ErrorType
  = ErrorType
      { ty :: Type
      }
  | DefaultErrorType
      { loc :: Location
      }
  | NoErrorType
  | Noraise
      { loc :: Location
      }
  | MaybeError
      { ty :: Type
      }
  deriving (Show, Eq, Generic)

-- | Constructor parameters consist of a type, a mutability flag
-- and an optional label.  They are used in variant and record
-- declarations.
data ConstrParam = ConstrParam
  { ty   :: Type
  , mut  :: Bool
  , label :: Maybe Label
  } deriving (Show, Eq, Generic)

-- | Data constructors and exception constructors share a common
-- declaration structure.  Each constructor has a name, optional
-- argument list, optional tag, location and an associated
-- documentation string.  The 'DocString' type is defined later in
-- this file.
data ConstrDecl = ConstrDecl
  { name :: ConstrName
  , args :: Maybe [ConstrParam]
  , tag  :: Maybe (String, Location)
  , loc  :: Location
  , doc  :: DocString
  } deriving (Show, Eq, Generic)

-- | Exception declarations may have no payload, a single payload or
-- a choice of constructors for different payloads.  This type
-- captures that distinction.
data ExceptionDecl
  = NoPayload
  | SinglePayload Type
  | EnumPayload [ConstrDecl]
  deriving (Show, Eq, Generic)

-- | Names of fields inside record types.  They consist of a
-- string and a location.
data FieldName = FieldName
  { label :: String
  , loc   :: Location
  } deriving (Show, Eq, Generic)

-- | Field declarations for record types record the name, the type
-- of the field, whether it is mutable, its visibility, the
-- location and an associated documentation string.
data FieldDecl = FieldDecl
  { name :: FieldName
  , ty   :: Type
  , mut  :: Bool
  , vis  :: Visibility
  , loc  :: Location
  , doc  :: DocString
  } deriving (Show, Eq, Generic)

-- | Description of the body of a type declaration.  Types may be
-- abstract, extern, newtypes, exceptions, variants, records or
-- aliases.  Each variant here corresponds directly to one in
-- @TypeDesc@ from the MoonBit source.
data TypeDesc
  = TypeAbstract
  | TypeExtern
  | TypeNewtype    Type
  | TypeError      ExceptionDecl
  | TypeVariant    [ConstrDecl]
  | TypeRecord     [FieldDecl]
  | TypeAlias      Type
  deriving (Show, Eq, Generic)

-- | Holes are used internally by the parser to represent
-- incomplete syntax.  A hole may be synthesised by the parser,
-- explicitly incomplete or marked as TODO by the programmer.
data Hole
  = Synthesized
  | Incomplete
  | Todo
  deriving (Show, Eq, Generic)

-- | The kind of argument used in function calls and patterns.
-- Arguments may be positional or labelled in several different
-- ways.  When a label is combined with a question mark in the
-- source, a location for the question mark is recorded.
data ArgumentKind
  = PositionalArg
  | LabelledArg     Label
  | LabelledPun     Label
  | LabelledOption  { label :: Label, questionLoc :: Location }
  | LabelledOptionPun { label :: Label, questionLoc :: Location }
  deriving (Show, Eq, Generic)

-- | The kind of function being defined or expressed.  MoonBit
-- distinguishes between lambda expressions, matrix (multi‑argument
-- pattern matching) functions and arrow functions.
data FnKind = LambdaKind | MatrixKind | ArrowKind
  deriving (Show, Eq, Generic)

-- | Simple grouping constructs used to disambiguate expressions.
-- These reflect parentheses and braces in the source language.
data Group = BraceGroup | ParenGroup
  deriving (Show, Eq, Generic)

-- | When constructing records and tuples a trailing comma or
-- semicolon may be present.  The parser records whether such a
-- trailing mark was seen or not.  The 'None' value corresponds
-- to the absence of any trailing separator.
data TrailingMark = TrailingComma | TrailingSemi | TrailingNone
  deriving (Show, Eq, Generic)

-- | Attributes that modify function application.  In MoonBit the
-- exclamation mark (@!@) and question mark (@?@) can be used to
-- override default calling semantics.  When no modifier is used,
-- 'NoAttr' is selected.
data ApplyAttr = NoAttr | ExclamationAttr | QuestionAttr
  deriving (Show, Eq, Generic)

-- | Type variable constraints used in generic declarations.  A
-- constraint names a trait that the type variable must implement
-- and records the location of the constraint.
data TypeVarConstraint = TypeVarConstraint
  { trait :: LongIdent
  , loc   :: Location
  } deriving (Show, Eq, Generic)

-- | A bound type variable with zero or more trait constraints.
data TypeVarBinder = TypeVarBinder
  { name        :: String
  , constraints :: [TypeVarConstraint]
  , loc         :: Location
  } deriving (Show, Eq, Generic)

-- | A type declaration binder may have an optional name.  In the
-- source this corresponds to an underscore placeholder for
-- anonymous binders.
data TypeDeclBinder = TypeDeclBinder
  { name :: Maybe String
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | A binder represents a variable bound by a pattern or
-- declaration.  It carries the name and the location of the
-- binding occurrence.
data Binder = Binder
  { name :: String
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | A variable refers to a possibly qualified identifier along
-- with its source location.  It is separate from 'Binder' to
-- distinguish uses from bindings.
data Var = Var
  { name :: LongIdent
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | Additional information attached to constructors.  It may
-- include a type name for newtype constructors, a package name for
-- external constructors or nothing at all.
data ConstructorExtraInfo
  = ExtraTypeName TypeName
  | ExtraPackage String
  | NoExtraInfo
  deriving (Show, Eq, Generic)

-- | A fully qualified data constructor.  Each constructor has a
-- name, optional extra information and a location.
data Constructor = Constructor
  { name      :: ConstrName
  , extraInfo :: ConstructorExtraInfo
  , loc       :: Location
  } deriving (Show, Eq, Generic)

-- | Accessors are used when extracting fields from records or
-- tuples.  They may be labelled (for record fields), indexed (for
-- tuples) or refer to the sole field of a newtype.
data Accessor
  = AccessorLabel Label
  | AccessorIndex { tupleIndex :: Int, loc :: Location }
  | AccessorNewtype { loc :: Location }
  deriving (Show, Eq, Generic)

-- | The target of an alias.  An alias binds a new name to either a
-- binder or an existing label.  A missing target label indicates
-- that the alias refers to the binder itself.
data AliasTarget = AliasTarget
  { binder :: Binder
  , target :: Maybe Label
  } deriving (Show, Eq, Generic)

-- | Arguments to function calls record the expression being passed
-- and the kind of argument (positional, labelled, etc.).
data Argument = Argument
  { value :: Expr
  , kind  :: ArgumentKind
  } deriving (Show, Eq, Generic)

-- | Parameters used in function and method declarations.  A
-- parameter may be a discarded positional value (an underscore), a
-- named positional parameter, a labelled parameter, or optional
-- parameters with default values.  The 'QuestionOptional'
-- variant corresponds to parameters declared with a leading
-- question mark.
data Parameter
  = DiscardPositional
      { ty  :: Maybe Type
      , loc :: Location
      }
  | PositionalParam
      { binder :: Binder
      , ty     :: Maybe Type
      }
  | LabelledParam
      { binder :: Binder
      , ty     :: Maybe Type
      }
  | OptionalParam
      { binder :: Binder
      , defaultVal :: Expr
      , ty         :: Maybe Type
      }
  | QuestionOptional
      { binder :: Binder
      , ty     :: Maybe Type
      }
  deriving (Show, Eq, Generic)

-- | A type alias for lists of parameters.  This mirrors the
-- @typealias@ from the source and is used to improve readability.
type Parameters = [Parameter]

-- | A single case in a match expression.  It consists of a
-- pattern, an optional guard expression and a body expression.
data Case = Case
  { pattern :: Pattern
  , guard_  :: Maybe Expr
  , body    :: Expr
  } deriving (Show, Eq, Generic)

-- | Cases used in multi‑argument match expressions.  Each case
-- pattern is a list of patterns corresponding to the arguments of
-- the matched function.  As with 'Case', each multi‑argument case
-- may have an optional guard and a body.
data MultiArgCase = MultiArgCase
  { patterns :: [Pattern]
  , guard_   :: Maybe Expr
  , body     :: Expr
  } deriving (Show, Eq, Generic)

-- | Elements of an array or record expression that may include
-- spreads.  A regular element contains a simple expression,
-- whereas a spread element records the expression being spread and
-- the location of the spread operator.
data SpreadableElem
  = RegularElem Expr
  | SpreadElem
      { expr :: Expr
      , loc  :: Location
      }
  deriving (Show, Eq, Generic)

-- | An element of a map expression.  In MoonBit map literals use
-- constants for keys, so the key type is 'Constant'.  Both the
-- position of the key and the overall element are recorded.
data MapExprElem = MapExprElem
  { key    :: Constant
  , expr   :: Expr
  , keyLoc :: Location
  , loc    :: Location
  } deriving (Show, Eq, Generic)

-- | A static assertion that requires a type to satisfy a trait at
-- compile time.  The 'msg' field records the user supplied
-- error message.
data StaticAssertion = StaticAssertion
  { ty    :: Type
  , trait :: LongIdent
  , loc   :: Location
  , msg   :: String
  } deriving (Show, Eq, Generic)

-- | Functions (lambdas and matches) form part of expressions.  A
-- function records its parameters, optional return and error
-- annotations, whether it is asynchronous, and the underlying
-- function kind (lambda or match).  Multi‑argument functions are
-- represented by the 'Match' constructor, which stores a list of
-- 'MultiArgCase's.
data Func
  = LambdaFunc
      { parameters :: Parameters
      , paramsLoc  :: Location
      , body       :: Expr
      , returnType :: Maybe Type
      , errorType  :: ErrorType
      , kind       :: FnKind
      , hasError   :: Maybe Location
      , isAsync    :: Bool
      , loc        :: Location
      }
  | MatchFunc
      { cases     :: [MultiArgCase]
      , hasError  :: Maybe Location
      , isAsync   :: Bool
      , fnLoc     :: Location
      , loc       :: Location
      }
  deriving (Show, Eq, Generic)

-- | Field definitions used when constructing record literals and
-- updates.  A field definition includes the label, the expression
-- assigned to the field, whether the assignment is a pun (i.e.
-- the label and variable name coincide) and its location.
data FieldDef = FieldDef
  { label  :: Label
  , expr   :: Expr
  , isPun  :: Bool
  , loc    :: Location
  } deriving (Show, Eq, Generic)

-- | Interpolation elements for string literals.  A literal piece
-- carries its textual representation and location; an embedded
-- expression contains a nested 'Expr' and its location; and the
-- 'Source' variant represents source interpolation used in MBTI.
data InterpElem
  = InterpLit
      { repr :: StringLiteral
      , loc  :: Location
      }
  | InterpExpr
      { expr :: Expr
      , loc  :: Location
      }
  | InterpSource
      { source :: InterpSource
      }
  deriving (Show, Eq, Generic)

-- | A helper type used for interpolation sources.  In the
-- original code this structure stores the raw source and its
-- location.  Here it is replicated directly.
data InterpSource = InterpSource
  { source :: String
  , loc    :: Location
  } deriving (Show, Eq, Generic)

-- | Elements of multiline string literals.  A multiline literal
-- either contains a simple string line or a nested interpolation.
data MultilineStringElem
  = MStringLine String
  | MStringInterp [InterpElem]
  deriving (Show, Eq, Generic)

-- | Expressions form the heart of the MoonBit language.  The
-- number of constructors reflects the rich syntactic forms of
-- MoonBit.  Each variant records the necessary substructure and
-- locations.  Pattern synonyms or lenses could be employed on
-- top of this definition to ease traversal and matching.
data Expr
  = ApplyExpr
      { func :: Expr
      , args :: [Argument]
      , attr :: ApplyAttr
      , loc  :: Location
      }
  | InfixExpr
      { op  :: Var
      , lhs :: Expr
      , rhs :: Expr
      , loc :: Location
      }
  | UnaryExpr
      { op  :: Var
      , expr :: Expr
      , loc  :: Location
      }
  | ArrayExpr
      { exprs :: [Expr]
      , loc   :: Location
      }
  | ArraySpreadExpr
      { elems :: [SpreadableElem]
      , loc   :: Location
      }
  | ArrayGetExpr
      { array :: Expr
      , index :: Expr
      , loc   :: Location
      }
  | ArrayGetSliceExpr
      { array     :: Expr
      , startIndex :: Maybe Expr
      , endIndex   :: Maybe Expr
      , indexLoc  :: Location
      , loc       :: Location
      }
  | ArraySetExpr
      { array :: Expr
      , index :: Expr
      , value :: Expr
      , loc   :: Location
      }
  | ArrayAugmentedSetExpr
      { op    :: Var
      , array :: Expr
      , index :: Expr
      , value :: Expr
      , loc   :: Location
      }
  | ConstantExpr
      { constant :: Constant
      , loc      :: Location
      }
  | MultilineStringExpr
      { elems :: [MultilineStringElem]
      , loc   :: Location
      }
  | InterpExprLit
      { elems :: [InterpElem]
      , loc   :: Location
      }
  | ConstraintExpr
      { expr :: Expr
      , ty   :: Type
      , loc  :: Location
      }
  | ConstrExpr
      { constr :: Constructor
      , loc    :: Location
      }
  | WhileExpr
      { loopCond  :: Expr
      , loopBody  :: Expr
      , whileElse :: Maybe Expr
      , label     :: Maybe Label
      , loc       :: Location
      }
  | FunctionExpr
      { func :: Func
      , loc  :: Location
      }
  | IdentExpr
      { id  :: Var
      , loc :: Location
      }
  | IfExpr
      { cond  :: Expr
      , ifso  :: Expr
      , ifnot :: Maybe Expr
      , loc   :: Location
      }
  | GuardExpr
      { cond     :: Expr
      , otherwiseExpr :: Maybe Expr
      , body     :: Expr
      , loc      :: Location
      }
  | IsExpr
      { expr :: Expr
      , pat  :: Pattern
      , loc  :: Location
      }
  | DeferExpr
      { expr :: Expr
      , body :: Expr
      , loc  :: Location
      }
  | LetFnExpr
      { name :: Binder
      , func :: Func
      , body :: Expr
      , loc  :: Location
      }
  | LetRecExpr
      { bindings :: [(Binder, Func)]
      , body     :: Expr
      , loc      :: Location
      }
  | LetAndExpr
      { bindings :: [(Binder, Maybe Type, Func)]
      , body     :: Expr
      , loc      :: Location
      }
  | LetExpr
      { pattern :: Pattern
      , expr    :: Expr
      , body    :: Expr
      , loc     :: Location
      }
  | SequenceExpr
      { exprs    :: [Expr]
      , lastExpr :: Expr
      , loc      :: Location
      }
  | TupleExpr
      { exprs :: [Expr]
      , loc   :: Location
      }
  | RecordExpr
      { typeName :: Maybe TypeName
      , fields   :: [FieldDef]
      , trailing :: TrailingMark
      , loc      :: Location
      }
  | RecordUpdateExpr
      { typeName :: Maybe TypeName
      , record   :: Expr
      , fields   :: [FieldDef]
      , loc      :: Location
      }
  | FieldExpr
      { record   :: Expr
      , accessor :: Accessor
      , loc      :: Location
      }
  | MethodExpr
      { typeName   :: TypeName
      , methodName :: Label
      , loc        :: Location
      }
  | DotApplyExpr
      { self        :: Expr
      , methodName  :: Label
      , args        :: [Argument]
      , returnSelf  :: Bool
      , attr        :: ApplyAttr
      , loc         :: Location
      }
  | AsExpr
      { expr   :: Expr
      , trait  :: TypeName
      , loc    :: Location
      }
  | MutateExpr
      { record      :: Expr
      , accessor    :: Accessor
      , field       :: Expr
      , augmentedBy :: Maybe Var
      , loc         :: Location
      }
  | MatchExpr
      { expr      :: Expr
      , cases     :: [Case]
      , matchLoc  :: Location
      , using     :: Maybe Label
      , loc       :: Location
      }
  | LetMutExpr
      { binder :: Binder
      , ty     :: Maybe Type
      , expr   :: Expr
      , body   :: Expr
      , loc    :: Location
      }
  | PipeExpr
      { lhs :: Expr
      , rhs :: Expr
      , loc :: Location
      }
  | AssignExpr
      { var         :: Var
      , expr        :: Expr
      , augmentedBy :: Maybe Var
      , loc         :: Location
      }
  | HoleExpr
      { loc  :: Location
      , kind :: Hole
      }
  | ReturnExpr
      { returnValue :: Maybe Expr
      , loc         :: Location
      }
  | RaiseExpr
      { errValue :: Expr
      , loc      :: Location
      }
  | UnitExpr
      { loc   :: Location
      , faked :: Bool
      }
  | BreakExpr
      { arg   :: Maybe Expr
      , label :: Maybe Label
      , loc   :: Location
      }
  | ContinueExpr
      { args  :: [Expr]
      , label :: Maybe Label
      , loc   :: Location
      }
  | LoopExpr
      { args     :: [Expr]
      , body     :: [MultiArgCase]
      , label    :: Maybe Label
      , loopLoc  :: Location
      , loc      :: Location
      }
  | ForExpr
      { binders       :: [(Binder, Expr)]
      , condition     :: Maybe Expr
      , continueBlock :: [(Binder, Expr)]
      , body          :: Expr
      , forElse       :: Maybe Expr
      , label         :: Maybe Label
      , loc           :: Location
      }
  | ForEachExpr
      { binders  :: [Maybe Binder]
      , expr     :: Expr
      , body     :: Expr
      , elseBlock :: Maybe Expr
      , label    :: Maybe Label
      , loc      :: Location
      }
  | TryExpr
      { body     :: Expr
      , catch_   :: [Case]
      , catchAll :: Bool
      , tryElse  :: Maybe [Case]
      , hasTry   :: Bool
      , tryLoc   :: Location
      , catchLoc :: Location
      , elseLoc  :: Location
      , loc      :: Location
      }
  | TryOperatorExpr
      { body    :: Expr
      , kind    :: TryOperatorKind
      , tryLoc  :: Location
      , loc     :: Location
      }
  | MapExpr
      { elems :: [MapExprElem]
      , loc   :: Location
      }
  | GroupExpr
      { expr  :: Expr
      , group :: Group
      , loc   :: Location
      }
  | StaticAssertExpr
      { asserts :: [StaticAssertion]
      , body    :: Expr
      }
  deriving (Show, Eq, Generic)

-- | The two operators that can follow the 'try' keyword in
-- MoonBit.  Either a question mark to propagate errors or an
-- exclamation mark to bypass error propagation.
data TryOperatorKind = TryQuestion | TryExclamation
  deriving (Show, Eq, Generic)

-- | Binders used in patterns such as spread patterns.  An
-- underscore indicates a wildcard; no binder indicates that the
-- names from the constructor definition are reused; 'BinderAs'
-- associates an alias with the matched value; and 'Binder' wraps
-- a simple binder.  The duplication of the 'Binder' constructor
-- name here does not conflict with the 'Binder' type because of
-- the distinct namespace of constructors.
data DotDotBinder
  = UnderscoreBinder
  | NoBinder
  | BinderAs Binder
  | BinderOnly Binder
  deriving (Show, Eq, Generic)

-- | Elements of array patterns can either be nested patterns,
-- spreads of strings or bytes, or constant spreads binding a
-- binder and an optional package name.  Each variant carries the
-- appropriate metadata.
data ArrayPattern
  = APPattern    Pattern
  | APStringSpread StringLiteral
  | APBytesSpread  BytesLiteral
  | APConstSpread
      { binder :: Binder
      , pkg    :: Maybe String
      , loc    :: Location
      }
  deriving (Show, Eq, Generic)

-- | Array patterns may be closed (just a list of patterns) or
-- open, specifying prefix and suffix patterns around a central
-- spread binder.  The central binder indicates where the
-- remainder of the array is bound.
data ArrayPatterns
  = ClosedArrayPatterns [ArrayPattern]
  | OpenArrayPatterns [ArrayPattern] [ArrayPattern] DotDotBinder
  deriving (Show, Eq, Generic)

-- | Field patterns used in record pattern matching.  Each field
-- pattern records the label, the nested pattern, whether the
-- pattern is a pun and the location.
data FieldPat = FieldPat
  { label  :: Label
  , pattern :: Pattern
  , isPun  :: Bool
  , loc    :: Location
  } deriving (Show, Eq, Generic)

-- | Constructor pattern arguments combine the nested pattern with
-- the kind of argument used.  They are used when matching on
-- variant constructors.
data ConstrPatArg = ConstrPatArg
  { pat  :: Pattern
  , kind :: ArgumentKind
  } deriving (Show, Eq, Generic)

-- | Elements of map patterns use constants as keys and patterns
-- as values.  They record whether absent keys should match and
-- both the key and overall locations.
data MapPatElem = MapPatElem
  { key         :: Constant
  , pat         :: Pattern
  , matchAbsent :: Bool
  , keyLoc      :: Location
  , loc         :: Location
  } deriving (Show, Eq, Generic)

-- | Patterns used throughout MoonBit.  Patterns mirror the
-- expression language with constructs for aliases, wildcards,
-- arrays, constants, constraints, constructor patterns, disjunctions,
-- tuples, variables, records, maps and ranges.  Each variant
-- carries the appropriate substructure and location information.
data Pattern
  = AliasPat
      { pat   :: Pattern
      , alias :: Binder
      , loc   :: Location
      }
  | AnyPat
      { loc :: Location
      }
  | ArrayPat
      { pats :: ArrayPatterns
      , loc  :: Location
      }
  | ConstPat
      { constant :: Constant
      , loc      :: Location
      }
  | ConstraintPat
      { pat :: Pattern
      , ty  :: Type
      , loc :: Location
      }
  | ConstrPat
      { constr   :: Constructor
      , args     :: Maybe [ConstrPatArg]
      , isOpen   :: Bool
      , loc      :: Location
      }
  | OrPat
      { pat1 :: Pattern
      , pat2 :: Pattern
      , loc  :: Location
      }
  | TuplePat
      { pats :: [Pattern]
      , loc  :: Location
      }
  | VarPat Binder
  | RecordPat
      { fields   :: [FieldPat]
      , isClosed :: Bool
      , loc      :: Location
      }
  | MapPat
      { elems    :: [MapPatElem]
      , isClosed :: Bool
      , loc      :: Location
      }
  | RangePat
      { lhs       :: Pattern
      , rhs       :: Pattern
      , inclusive :: Bool
      , loc       :: Location
      }
  deriving (Show, Eq, Generic)

-- | Test names used in top level test declarations.  In MoonBit
-- this is an optional pair of a string literal and a location.
-- Here we model that directly using 'Maybe'.
type TestName = Maybe (StringLiteral, Location)

-- | Local type declarations used inside functions or letrec
-- bindings.  They record the type constructor, its location,
-- components, deriving directives and more.  See the MoonBit
-- source for details.
data LocalTypeDecl = LocalTypeDecl
  { tycon      :: String
  , tyconLoc   :: Location
  , components :: TypeDesc
  , deriving   :: [DerivingDirective]
  } deriving (Show, Eq, Generic)

-- | Deriving directives used to indicate that a type implements
-- certain traits.  The arguments correspond to those in the
-- original definition: the name of the type, arguments passed to
-- the deriving trait, and a location.
data DerivingDirective = DerivingDirective
  { typeName :: TypeName
  , args     :: [Argument]
  , loc      :: Location
  } deriving (Show, Eq, Generic)

-- | A complete type declaration at the top level.  The record
-- matches the fields defined in the MoonBit source including
-- parameters, attributes, visibility and documentation strings.
data TypeDecl = TypeDecl
  { tycon      :: String
  , tyconLoc   :: Location
  , params     :: [TypeDeclBinder]
  , components :: TypeDesc
  , attrs      :: [Attribute]
  , doc        :: DocString
  , typeVis    :: Visibility
  , deriving   :: [DerivingDirective]
  , loc        :: Location
  } deriving (Show, Eq, Generic)

-- | Function stubs used when declaring external or embedded
-- functions.  A stub either imports a named function from a
-- module or embeds code written in another language.  Embedded
-- code can be either a simple string or a list of strings for
-- multiline code.
data FuncStubs
  = ImportStub
      { moduleName :: StringLiteral
      , funcName   :: StringLiteral
      }
  | EmbeddedStub
      { language :: Maybe StringLiteral
      , code     :: EmbeddedCode
      }
  deriving (Show, Eq, Generic)

-- | Code embedded in foreign languages for function stubs.  It
-- can be a single line or multiple lines.  In the MoonBit
-- implementation these variants wrap string literals and string
-- arrays respectively.
data EmbeddedCode
  = CodeString StringLiteral
  | CodeMultilineString [String]
  deriving (Show, Eq, Generic)

-- | A declaration body either contains an expression together with
-- local type declarations or a list of stubs describing the
-- imported/embedded body of the function.  The former
-- corresponds to actual function definitions; the latter to
-- external declarations.
data DeclBody
  = DeclBody
      { localTypes :: [LocalTypeDecl]
      , expr       :: Expr
      }
  | DeclStubs FuncStubs
  deriving (Show, Eq, Generic)

-- | A full function declaration.  Many of the fields mirror those
-- seen in the 'LambdaFunc' constructor of 'Func', but they are
-- used for top level declarations rather than expressions.  The
-- 'doc' field stores any documentation string associated with
-- the function.
data FunDecl = FunDecl
  { typeName  :: Maybe TypeName
  , name      :: Binder
  , hasError  :: Maybe Location
  , isAsync   :: Bool
  , declParams :: Maybe Parameters
  , paramsLoc :: Location
  , quantifiers :: [TypeVarBinder]
  , returnType :: Maybe Type
  , errorType :: ErrorType
  , vis      :: Visibility
  , attrs    :: [Attribute]
  , doc      :: DocString
  } deriving (Show, Eq, Generic)

-- | Trait method declarations within trait definitions.  They
-- resemble ordinary function declarations but carry an additional
-- flag indicating whether a default implementation is provided.
data TraitMethodDecl = TraitMethodDecl
  { name       :: Binder
  , hasError   :: Maybe Location
  , isAsync    :: Bool
  , quantifiers :: [TypeVarBinder]
  , params     :: Parameters
  , returnType :: Maybe Type
  , errorType  :: ErrorType
  , hasDefault :: Maybe Location
  , loc        :: Location
  } deriving (Show, Eq, Generic)

-- | Trait declarations specify the name of the trait, its super
-- traits, method declarations, visibility, location, attributes and
-- documentation.  The representation here matches the original
-- MoonBit definition exactly.
data TraitDecl = TraitDecl
  { name    :: Binder
  , supers  :: [TypeVarConstraint]
  , methods :: [TraitMethodDecl]
  , vis     :: Visibility
  , loc     :: Location
  , attrs   :: [Attribute]
  , doc     :: DocString
  } deriving (Show, Eq, Generic)

-- | The various forms of top level implementation in MoonBit.  A
-- top level implementation may consist of an expression, a test,
-- a type definition, a function definition, a function alias, a
-- constant/value definition, a trait definition, batch aliases,
-- an implementation of a trait for a type, a view definition, or
-- an implementation relation specifying that a type implements a
-- trait.  Each variant carries all of the relevant substructures
-- and metadata from the original definition.  Documentation
-- strings are marked mutable to allow comments to be attached.
data Impl
  = TopExpr
      { expr      :: Expr
      , isMain    :: Bool
      , localTypes :: [LocalTypeDecl]
      , loc       :: Location
      }
  | TopTest
      { expr      :: Expr
      , name      :: TestName
      , params    :: Maybe Parameters
      , localTypes :: [LocalTypeDecl]
      , loc       :: Location
      , attrs     :: [Attribute]
      , doc       :: DocString
      }
  | TopTypeDef TypeDecl
  | TopFuncDef
      { funDecl  :: FunDecl
      , declBody :: DeclBody
      , loc      :: Location
      }
  | TopFuncAlias
      { pkg        :: Maybe Label
      , typeName   :: Maybe Label
      , targets    :: [AliasTarget]
      , vis        :: Visibility
      , attrs      :: [Attribute]
      , isList     :: Bool
      , doc        :: DocString
      , loc        :: Location
      }
  | TopLetDef
      { binder     :: Binder
      , ty         :: Maybe Type
      , expr       :: Expr
      , vis        :: Visibility
      , isConstant :: Bool
      , loc        :: Location
      , attrs      :: [Attribute]
      , doc        :: DocString
      }
  | TopTrait TraitDecl
  | TopBatchTypeAlias
      { pkg     :: Maybe Label
      , targets :: [AliasTarget]
      , vis     :: Visibility
      , loc     :: Location
      , attrs   :: [Attribute]
      , isList  :: Bool
      , doc     :: DocString
      }
  | TopBatchTraitAlias
      { pkg     :: Maybe Label
      , targets :: [AliasTarget]
      , vis     :: Visibility
      , loc     :: Location
      , attrs   :: [Attribute]
      , isList  :: Bool
      , doc     :: DocString
      }
  | TopImpl
      { selfTy    :: Maybe Type
      , trait     :: TypeName
      , methodName :: Binder
      , hasError  :: Maybe Location
      , quantifiers :: [TypeVarBinder]
      , params    :: Parameters
      , retTy     :: Maybe Type
      , errTy     :: ErrorType
      , body      :: DeclBody
      , vis       :: Visibility
      , loc       :: Location
      , attrs     :: [Attribute]
      , doc       :: DocString
      }
  | TopView
      { quantifiers  :: [TypeVarBinder]
      , sourceTy     :: Type
      , viewTypeName :: String
      , viewTypeLoc  :: Location
      , viewConstrs  :: [ConstrDecl]
      , viewFuncName :: Binder
      , parameters   :: Parameters
      , paramsLoc    :: Location
      , body         :: Expr
      , vis          :: Visibility
      , loc          :: Location
      , attrs        :: [Attribute]
      , doc          :: DocString
      }
  | TopImplRelation
      { selfTy   :: Type
      , trait    :: TypeName
      , quantifiers :: [TypeVarBinder]
      , vis     :: Visibility
      , attrs   :: [Attribute]
      , loc     :: Location
      , doc     :: DocString
      }
  deriving (Show, Eq, Generic)

-- | A type alias for lists of top level implementations.  Used
-- within the parser to represent whole modules.
type Impls = [Impl]

-- ---------------------------------------------------------------------
-- Documentation strings and ancillary types
--
-- The following definitions appear in @syntax_types/ast_types.mbti@
-- and related files.  They are used by both MoonBit and MBTI
-- signatures.  The definitions here are simplified from the
-- original: each documentation string is stored as a list of
-- lines together with its location.

-- | Documentation comments that precede declarations.  A doc
-- string consists of a list of lines and the location covering
-- those lines.  An empty doc string can be created using
-- 'DocString [] dummyLocation'.
data DocString = DocString
  { content :: [String]
  , loc     :: Location
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------
-- MBTI AST definitions
--
-- The MBTI signature language shares many structures with MoonBit
-- (for example types and constants) but has its own specialised
-- structures for describing function signatures, type signatures,
-- alias signatures, traits, implementations and packages.  The
-- following definitions are direct translations of those found in
-- @src/parsing/mbti_ast/mbti_ast.mbt@.

-- | A simple name consisting of an identifier and a location.
data Name = Name
  { name :: String
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | A qualified name pairs a 'LongIdent' with a location.  In the
-- original MBTI AST this is known as 'QualifiedName'.
data QualifiedName = QualifiedName
  { name :: LongIdent
  , loc  :: Location
  } deriving (Show, Eq, Generic)

-- | A type parameter with trait constraints.  Each constraint is
-- itself a qualified name.  This structure corresponds to the
-- @TypeParamWithConstraints@ definition in the original source.
data TypeParamWithConstraints = TypeParamWithConstraints
  { name        :: Name
  , constraints :: [QualifiedName]
  } deriving (Show, Eq, Generic)

-- | Type parameters without constraints may either be named or
-- underscores.  The underscore variant records the location of
-- the underscore.
data TypeParamNoConstraints
  = TypeParamName Name
  | TypeParamUnderscore Location
  deriving (Show, Eq, Generic)

-- | Parameters to functions in the MBTI signature language.  These
-- are simpler than full MoonBit parameters: only positional,
-- labelled, autofill, default option and question option
-- parameters are supported.
data MbtiParameter
  = MbtiPositional Type
  | MbtiLabelled  Label Type
  | MbtiAutofill  Label Type
  | MbtiOptionalDefault Label Type
  | MbtiOptionalOption  Label Type
  deriving (Show, Eq, Generic)

-- | Parameters to trait methods in MBTI.  Only positional and
-- labelled parameters are supported.
data TraitMethodParameter
  = TMPositional Type
  | TMLabelled  Label Type
  deriving (Show, Eq, Generic)

-- | A function signature in MBTI consists of optional attributes
-- (name, qualifier and value), an optional type name, the name of
-- the function, its parameters, the return type along with an
-- error type, and any type parameters with constraints.
data FuncSig = FuncSig
  { attr       :: [(String, Maybe String, String)]
  , typeName   :: Maybe Name
  , name       :: Name
  , params     :: [MbtiParameter]
  , return_    :: (Type, ErrorType)
  , typeParams :: [TypeParamWithConstraints]
  } deriving (Show, Eq, Generic)

-- | A type signature in MBTI records the name, type parameters,
-- type description and visibility of a type.  It mirrors the
-- corresponding structure in the source.
data TypeSig = TypeSig
  { name      :: Name
  , typeParams :: [TypeParamNoConstraints]
  , components :: TypeDesc
  , vis       :: Visibility
  } deriving (Show, Eq, Generic)

-- | Alias signatures may define type aliases, trait aliases or
-- function aliases.  Type and trait aliases carry the name,
-- parameters, aliased type/trait and visibility; function aliases
-- record a location for the alias.  The constructors here map
-- directly onto those in the original 'AliasSig' definition.
data AliasSig
  = TypeAliasSig
      { name      :: Name
      , typeParams :: [TypeParamNoConstraints]
      , type_     :: Type
      , vis       :: Visibility
      }
  | TraitAliasSig
      { name      :: Name
      , traitName :: QualifiedName
      , vis       :: Visibility
      }
  | FnAliasSig
      { name      :: Name
      , typeName  :: QualifiedName
      , loc       :: Location
      }
  deriving (Show, Eq, Generic)

-- | Signatures of trait methods consist of the name, list of
-- parameters, flag indicating presence of a default implementation
-- and the return type paired with an error type.
data TraitMethodSig = TraitMethodSig
  { name      :: Name
  , params    :: [TraitMethodParameter]
  , hasDefault :: Bool
  , return_   :: (Type, ErrorType)
  } deriving (Show, Eq, Generic)

-- | Traits in MBTI record the name of the trait, a list of
-- super traits, method signatures, and visibility.
data TraitSig = TraitSig
  { name       :: Name
  , superTraits :: [QualifiedName]
  , methods    :: [TraitMethodSig]
  , vis        :: Visibility
  } deriving (Show, Eq, Generic)

-- | Implementations either define a trait implementation for a
-- specific type with type parameters, or they declare a default
-- implementation of a trait method.  This mirrors the original
-- 'ImplSig' enumeration.
data ImplSig
  = TraitImpl
      { typeParams :: [TypeParamWithConstraints]
      , type_      :: Type
      , traitName  :: QualifiedName
      }
  | DefaultImpl
      { traitName  :: Name
      , methodName :: Name
      }
  deriving (Show, Eq, Generic)

-- | Constant signatures record the name, type and value of a
-- constant binding.
data ConstSig = ConstSig
  { name  :: Name
  , type_ :: Type
  , value :: Constant
  } deriving (Show, Eq, Generic)

-- | Value signatures record the name and type of a value binding.
data ValueSig = ValueSig
  { name  :: Name
  , type_ :: Type
  } deriving (Show, Eq, Generic)

-- | Import declarations specify a package name and an optional
-- alias.  These appear at the beginning of MBTI signature files.
data PackageImport = PackageImport
  { name  :: String
  , alias :: Maybe String
  } deriving (Show, Eq, Generic)

-- | The top level of an MBTI file is a list of signatures.  Each
-- signature is paired with a location, allowing the parser to
-- track where each declaration originated.  Signatures include
-- functions, types, aliases, traits, implementations, constants
-- and values.  This sum type mirrors the original 'Sig'
-- enumeration.
data Sig
  = SigFunc  FuncSig
  | SigType  TypeSig
  | SigAlias AliasSig
  | SigTrait TraitSig
  | SigImpl  ImplSig
  | SigConst ConstSig
  | SigValue ValueSig
  deriving (Show, Eq, Generic)

-- | A complete MBTI module consists of the package name, a list of
-- imports and a list of signatures paired with their locations.
data Mbti = Mbti
  { packageName :: String
  , imports     :: [PackageImport]
  , sigs        :: [(Sig, Location)]
  } deriving (Show, Eq, Generic)
