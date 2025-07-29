module Language.Moonbit.Mbti.Syntax (
  MbtiFile (..),
  Decl (..),
  ModulePath (..),
  Effect (..),
  EffectException (..),
  Type (..),
  TCon (..),
  Constraint (..),
  FnKind (..),
  Name,
  FnSig (..),
  FnAttr (..),
  TPath (..),
  FnDecl' (..),
)
where

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq)

data Decl
  = FnDecl FnDecl'
  | -- \| ImplForTypeDecl
    -- \| DefaultImplDecl

    -- \| ConstDecl
    TypeDecl Type
  -- \| TypeAliasDecl

  -- \| StructDecl
  -- \| EnumDecl
  -- \| ErrorTypeDecl
  -- \| TraitDecl
  -- \| TraitAliasDecl
  deriving
    ( -- | FunAliasDecl
      Show
    , Eq
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
  | AraisePoly
  deriving (Show, Eq)

data Type
  = TName (Maybe TPath) TCon
  | TFun [Type] Type [Effect]
  | TTuple [Type]
  deriving (Eq, Show)

data TPath = TPath [Name] Name -- path + module name
  deriving (Eq, Show)

data TCon = TCon Name [Type]
  deriving (Eq, Show)

data Constraint
  = CTrait (Maybe TPath) Name
  deriving (Eq, Show)

data FnKind
  = FreeFn
  | Method Type -- impl For which type
  deriving (Eq, Show)

-- \| TraitMethod

type Name = String

data FnSig = FnSig
  { funName :: Name
  , funParams :: [(Maybe Name, Type)] -- Maybe Name for named parameters
  , funReturnType :: Type
  , funTyParams :: [(TCon, [Constraint])]
  , funEff :: [Effect]
  }
  deriving (Eq, Show)

newtype FnAttr
  = Deprecated (Maybe String) -- #deprecated("reason")
  deriving (Eq, Show)

data FnDecl' = FnDecl'
  { fnSig :: FnSig
  , fnAttr :: [FnAttr]
  , fnKind :: FnKind
  }
  deriving (Eq, Show)
