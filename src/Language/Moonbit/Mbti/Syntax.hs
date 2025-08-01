{-# LANGUAGE DeriveGeneric #-}

module Language.Moonbit.Mbti.Syntax (
  -- * 基础
  Position (..),
  SrcSpan (..),
  Located (..),
  lmap,

  -- * 名称相关
  Name (..),
  Qualified (..),
  Visibility (..),

  -- * 顶层结构
  Module (..),
  PackageImport (..),

  -- * 各类签名
  Sig (..),
  FuncSig (..),
  ValueSig (..),
  ConstSig (..),
  TypeSig (..),
  TypeDeclComp (..),
  ExceptionDecl (..),
  AliasSig (..),
  ImplSig (..),
  TraitSig (..),
  TraitMethodSig (..),

  -- * 类型 & 参数
  Type (..),
  ErrorType (..),
  ReturnType (..),
  TypeParam (..),
  TypeParamNC (..),
  TypeConstraint (..),
  Parameter (..),
  TraitParam (..),

  -- * 其他
  EnumConstr (..),
  ConstrParam (..),
  RecordField (..),
  Constant (..),
) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- 基础位置类型
--------------------------------------------------------------------------------

data Position = Position
  { posFile :: !Text
  , posLine :: !Int
  , posColumn :: !Int
  }
  deriving (Eq, Ord, Show, Generic)

data SrcSpan = SrcSpan
  { spanStart :: !Position
  -- ^ inclusive
  , spanEnd :: !Position
  -- ^ exclusive
  }
  deriving (Eq, Ord, Show, Generic)

-- | 带位置信息的包装
data Located a = L
  { locSpan :: !SrcSpan
  , locThing :: a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | map 内部值同时保留位置信息
lmap :: (a -> b) -> Located a -> Located b
lmap f (L s x) = L s (f x)

--------------------------------------------------------------------------------
-- 名称与可见性
--------------------------------------------------------------------------------

-- | 简单（未限定）名字，附带源位置
data Name = Name
  { nText :: !Text
  , nLoc :: !SrcSpan
  }
  deriving (Eq, Ord, Show, Generic)

-- | 可能带包名前缀的限定名字
data Qualified
  = -- | 形如  Foo
    QSimple (Located Name)
  | -- | 形如  pkg.Foo
    QDotPkg !(Located Text) !(Located Name)
  deriving (Eq, Show, Generic)

data Visibility
  = VisDefault
  | -- | 记录 "priv" 关键字位置
    VisPriv !(Located ())
  | -- | 记录 "pub" [attr]
    VisPub !(Maybe Text) !(Located ())
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 顶层模块
--------------------------------------------------------------------------------

data Module = Module
  { mPkgName :: !Text
  -- ^ package "foo.bar"
  , mImports :: ![PackageImport]
  , mSigs :: ![Located Sig]
  , mLoc :: !SrcSpan
  -- ^ 整个文件 span
  }
  deriving (Eq, Show, Generic)

data PackageImport = PackageImport
  { impName :: !Text
  , impAlias :: !(Maybe Text)
  , impLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 顶层签名
--------------------------------------------------------------------------------

data Sig
  = SigFunc !(Located FuncSig)
  | SigType !(Located TypeSig)
  | SigAlias !(Located AliasSig)
  | SigTrait !(Located TraitSig)
  | SigImpl !(Located ImplSig)
  | SigConst !(Located ConstSig)
  | SigValue !(Located ValueSig)
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 1. 函数 & 常量 & 值
--------------------------------------------------------------------------------

data FuncSig = FuncSig
  { fsAsync :: !Bool
  , fsTypeName :: !(Maybe (Located Name))
  -- ^ Optional  Foo::
  , fsName :: !(Located Name)
  , fsTyParams :: ![TypeParam]
  -- ^ [T: Trait + ...]
  , fsParams :: ![Parameter]
  , fsReturn :: !ReturnType
  , fsLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

data ValueSig = ValueSig
  { vsName :: !(Located Name)
  , vsType :: !Type
  }
  deriving (Eq, Show, Generic)

data ConstSig = ConstSig
  { csName :: !(Located Name)
  , csType :: !Type
  , csValue :: !Constant
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 2. 类型声明
--------------------------------------------------------------------------------

data TypeSig = TypeSig
  { tsVis :: !Visibility
  , tsName :: !(Located Name)
  , tsTyParams :: ![TypeParamNC]
  -- ^ 没有约束的参数 (T, _, …)
  , tsComponent :: !TypeDeclComp
  , tsLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

data TypeDeclComp
  = TDExtern
  | TDAbstract
  | TDNewtype !Type
  | TDError !ExceptionDecl
  | TDRecord ![RecordField]
  | TDVariant ![EnumConstr]
  deriving (Eq, Show, Generic)

data ExceptionDecl
  = NoPayload
  | SinglePayload !Type
  | EnumPayload ![EnumConstr]
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 3. Alias / Impl / Trait
--------------------------------------------------------------------------------

data AliasSig
  = TypeAlias
      { alVis :: !Visibility
      , alName :: !(Located Name)
      , alTyParams :: ![TypeParamNC]
      , alType :: !Type
      , alLoc :: !SrcSpan
      }
  | TraitAlias
      { alVis :: !Visibility
      , alName :: !(Located Name)
      , alTrait :: !Qualified
      , alLoc :: !SrcSpan
      }
  | FuncAlias
      { faTypeName :: !(Located Name)
      -- ^ Foo::
      , faName :: !(Located Name)
      , faLoc :: !SrcSpan
      }
  deriving (Eq, Show, Generic)

data ImplSig = ImplSig
  { imTyParams :: ![TypeParam]
  -- ^ 可为空
  , imTrait :: !Qualified
  , imFor :: !Type
  , imLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

data TraitSig = TraitSig
  { trVis :: !Visibility
  , trName :: !(Located Name)
  , trSupers :: ![Qualified]
  -- ^ 父 trait
  , trMethods :: ![Located TraitMethodSig]
  , trLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

data TraitMethodSig = TraitMethodSig
  { tmName :: !(Located Name)
  , tmParams :: ![TraitParam]
  , tmReturn :: !ReturnType
  , tmHasDefault :: !Bool
  , tmLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 4. 类型系统
--------------------------------------------------------------------------------

data Type
  = -- | T?
    TyOption !SrcSpan !Type
  | -- | (A, B, …)
    TyTuple !SrcSpan ![Type]
  | -- | Foo[T, U]
    TyName !SrcSpan !Qualified ![Type]
  | -- | &Foo
    TyObject !Qualified
  | -- | _
    TyAny !SrcSpan
  | TyArrow
      { tyLoc :: !SrcSpan
      , tyAsync :: !Bool
      , tyArgs :: ![Type]
      -- ^ 可以为空
      , tyResult :: !Type
      , tyError :: !ErrorType
      }
  deriving (Eq, Show, Generic)

data ErrorType
  = NoError
  | -- | ! / raise
    DefaultError {etOldSyntax :: !Bool}
  | ExplicitError {etType :: !Type, etOldSyntax :: !Bool}
  | MaybeError {etType :: !Type, etOldSyntax :: !Bool}
  deriving (Eq, Show, Generic)

data ReturnType = ReturnType
  { rtResult :: !Type
  , rtError :: !ErrorType
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 5. 类型参数 & 约束
--------------------------------------------------------------------------------

data TypeConstraint = TCQualified !Qualified
  deriving (Eq, Show, Generic)

data TypeParam = TypeParam
  { tpName :: !(Located Name)
  , tpConstraints :: ![TypeConstraint]
  }
  deriving (Eq, Show, Generic)

data TypeParamNC -- 无约束版本（带下划线）
  = TPNamed !(Located Name)
  | TPUnderscore !SrcSpan
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 6. 函数 / 方法参数
--------------------------------------------------------------------------------

data Parameter
  = PPositional !Type
  | PLabelled !(Located Name) !Type
  | POptDefault !(Located Name) !Type -- label = ..
  | PAutofill !(Located Name) !Type -- label = _
  | POptOption !(Located Name) !Type -- label?:
  deriving (Eq, Show, Generic)

-- trait 中只允许 Positional / Labelled
data TraitParam
  = TPPositional !Type
  | TPLabelled !(Located Name) !Type
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 7. Enum / Record
--------------------------------------------------------------------------------

data EnumConstr = EnumConstr
  { ecName :: !(Located Name)
  , ecArgs :: ![ConstrParam]
  , ecTag :: !(Maybe (Int64, SrcSpan))
  , ecLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

data ConstrParam = ConstrParam
  { cpType :: !Type
  , cpMut :: !Bool
  , cpLabel :: !(Maybe (Located Name))
  }
  deriving (Eq, Show, Generic)

data RecordField = RecordField
  { rfName :: !(Located Name)
  , rfType :: !Type
  , rfMut :: !Bool
  , rfVis :: !Visibility
  , rfLoc :: !SrcSpan
  }
  deriving (Eq, Show, Generic)

--------------------------------------------------------------------------------
-- 8. 常量
--------------------------------------------------------------------------------

data Constant
  = CBool !Bool
  | CByte !Char
  | CBytes !ByteString
  | CChar !Char
  | CInt !Integer
  | CDouble !Double
  | CString !Text
  deriving (Eq, Show, Generic)
