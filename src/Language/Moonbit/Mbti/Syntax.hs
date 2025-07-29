module Language.Moonbit.Mbti.Syntax
  ( MbtiFile (..),
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
    TTrait (..),
    ImplSig (..),
    FnParam (..),
    Visibility (..),
    ErrorType (..),
  )
where

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq)

data Decl
  = FnDecl FnDecl'
  | ImplForTypeDecl ImplSig
  | ConstDecl Name Type
  | TypeDecl Type
  | TypeAliasDecl Visibility Type Type -- original type, alias name
  | StructDecl Visibility Type [(Name, Type)]
  | EnumDecl Visibility Type [(Name, [FnParam])]
  | ErrorTypeDecl Visibility Name ErrorType
  | TraitDecl Visibility TTrait [Constraint] [FnDecl']
  | TraitAliasDecl Visibility TTrait TTrait
  deriving
    ( -- | FunAliasDecl
      Show,
      Eq
    )

-- impl[K, V] Trait for T[K, V]
data ImplSig = ImplSig [(TCon, [Constraint])] TTrait Type
  deriving (Show, Eq)

data ModulePath = ModulePath {mpUserName :: Name, mpModuleName :: Name, mpPackagePath :: [Name]}
  deriving (Show, Eq)

data ErrorType
  = ETNoPayload
  | ETSinglePayload Type
  | ETEnumPayload [(Name, [FnParam])]
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
  | TDynTrait TTrait
  deriving (Eq, Show)

data TPath = TPath [Name] Name -- path + module name
  deriving (Eq, Show)

data TCon = TCon Name [Type]
  deriving (Eq, Show)

newtype Constraint
  = CTrait TTrait
  deriving (Eq, Show)

data TTrait = TTrait (Maybe TPath) Name
  deriving (Eq, Show)

data FnKind
  = FreeFn
  | Method Type -- impl For which type
  | TraitMethod TTrait Bool -- whether have default impl
  deriving (Eq, Show)

-- \| TraitMethod

type Name = String

data FnSig = FnSig
  { funName :: Name,
    funParams :: [FnParam],
    funReturnType :: Type,
    funTyParams :: [(TCon, [Constraint])],
    funEff :: [Effect]
  }
  deriving (Eq, Show)

data FnParam
  = AnonParam Type -- Anonymous parameter, e.g. `Type`
  | NamedParam Name Type Bool Bool -- Named parameter, e.g. `name~ : Type = ../_`
  deriving (Eq, Show)

newtype FnAttr
  = Deprecated (Maybe String) -- #deprecated("reason")
  deriving (Eq, Show)

data FnDecl' = FnDecl'
  { fnSig :: FnSig,
    fnAttr :: [FnAttr],
    fnKind :: FnKind
  }
  deriving (Eq, Show)

data Visibility
  = VisPub
  | VisPubOpen
  | VisPriv
  | VisPubAll
  deriving (Eq, Show)
