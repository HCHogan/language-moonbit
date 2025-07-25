module Language.Moonbit.Mbti.Syntax (
  MbtiFile(..),
  Decl(..),
) where

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq)

data Decl
  = FunDecl
    {
      funName :: String
    }
  -- | FunAliasDecl

  -- | MethodDecl
  -- | ImplForTypeDecl
  -- | DefaultImplDecl

  -- | ConstDecl
  -- | TypeDecl
  -- | TypeAliasDecl

  -- | StructDecl
  -- | EnumDecl
  -- | ErrorTypeDecl
  -- | TraitDecl
  -- | TraitAliasDecl
  deriving (Show, Eq)

data ModulePath = ModulePath { mpUserName :: String, mpModuleName :: String, mpPackagePath :: [String] }
  deriving (Show, Eq)

data Type
  = TVar String           -- ^ 类型变量或无参类型构造器
  | TCon String [Type]    -- ^ 有参的类型构造器
  | TFun Type Type        -- ^ 函数类型：T1 -> T2
  | TTuple [Type]         -- ^ 元组类型：(..., ...)
  | TArray Type           -- ^ 数组/列表类型：[T]
  deriving (Eq, Show)

