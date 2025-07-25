module Language.Moonbit.Mbti.Syntax (
  MbtiFile(..),
  Decl(..),
) where

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq)

data Decl
  = FunDecl
  | FunAliasDecl

  | MethodDecl
  | ImplForTypeDecl
  | DefaultImplDecl

  | ValueDecl
  | ConstDecl
  | TypeDecl
  | TypeAliasDecl

  | StructDecl
  | EnumDecl
  | ErrorTypeDecl
  | TraitDecl
  | TraitAliasDecl
  deriving (Show, Eq)

data ModulePath = ModulePath { mpUserName :: String, mpModuleName :: String, mpPackagePath :: [String] }
  deriving (Show, Eq)


