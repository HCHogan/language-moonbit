module Language.Moonbit.Mbti.Syntax
  ( MbtiFile (..),
    Decl (..),
  )
where

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq)

data Decl
  = FnDecl
      { fnSig :: FnSig,
        fnAttr :: [FnAttr],
        fnKind :: FnKind
      }
  | -- \| ImplForTypeDecl
    -- \| DefaultImplDecl

    -- \| ConstDecl
    TypeDecl
  -- \| TypeAliasDecl

  -- \| StructDecl
  -- \| EnumDecl
  -- \| ErrorTypeDecl
  -- \| TraitDecl
  -- \| TraitAliasDecl
  deriving
    ( -- | FunAliasDecl
      Show,
      Eq
    )

data ModulePath = ModulePath {mpUserName :: Name, mpModuleName :: Name, mpPackagePath :: [Name]}
  deriving (Show, Eq)

data Effect
  = EffAsync
  | EffException EffectException
  deriving (Show, Eq)

data EffectException
  = NoAraise
  | Araise Type
  deriving (Show, Eq)

data Type
  = TVar TVar
  | TCon Name [Type]
  | TFun [Type] Type [Effect]
  | TTuple [Type]
  | TArray Type
  deriving (Eq, Show)

newtype TVar = TV Name
  deriving (Eq, Show)

newtype Constraint
  = CTrait Name
  deriving (Eq, Show)

data FnKind
  = FreeFn
  | Method {}
  deriving (Eq, Show)

newtype Name = Name String
  deriving (Eq, Show)

data FnSig = FnSig
  { funName :: Name,
    funParams :: [(Name, Type)],
    funReturnType :: Type,
    funTyParams :: [(TVar, [Constraint])],
    funEff :: [Effect]
  }
  deriving (Eq, Show)

data FnAttr
  = Deprecated
  deriving (Eq, Show)
