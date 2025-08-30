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
    Attr (..),
    TPath (..),
    FnDecl' (..),
    TTrait (..),
    ImplSig (..),
    FnParam (..),
    Visibility (..),
    ErrorType (..),
    StructD (..),
  )
where

import GHC.Generics (Generic)

-- | This module defines the syntax for the Moonbit type inference system (Mbti).
data MbtiFile = MbtiFile ModulePath [ModulePath] [Decl]
  deriving (Show, Eq, Generic)

data Decl
  = FnDecl FnDecl'
  | ImplForTypeDecl ImplSig
  | ConstDecl Name Type
  | LetDecl Name Type
  | TypeDecl [Attr] Visibility Type (Maybe Type) -- opaque or newtype
  | TypeAliasDecl Visibility Type Type -- original type, alias name
  | StructDecl Visibility Type StructD
  | EnumDecl Visibility Type [(Name, [FnParam])]
  | ErrorTypeDecl Visibility Name ErrorType
  | TraitDecl Visibility TTrait [Constraint] [FnDecl']
  | TraitAliasDecl Visibility TTrait TTrait
  | FnAliasDecl Visibility (Maybe TPath) Type Name
  deriving (Show, Eq, Generic)

-- impl[K, V] Trait for T[K, V]
data ImplSig = ImplSig [(TCon, [Constraint])] TTrait Type
  deriving (Show, Eq, Generic)

data ModulePath = ModulePath {mpUserName :: Name, mpModuleName :: Name, mpPackagePath :: [Name]}
  deriving (Show, Eq, Generic)

data StructD
  = NamedStruct [(Name, Type, Bool)] -- Bool indicates whether the field is mutable
  | TupleStruct [(Type, Bool)]
  deriving (Show, Eq, Generic)

data ErrorType
  = ETNoPayload
  | ETSinglePayload Type
  | ETEnumPayload [(Name, [FnParam])]
  deriving (Show, Eq, Generic)

data Effect
  = EffAsync
  | EffException EffectException
  deriving (Show, Eq, Generic)

data EffectException
  = NoAraise
  | Araise Type
  | AraisePoly
  deriving (Show, Eq, Generic)

data Type
  = TName (Maybe TPath) TCon
  | TFun [Type] Type [Effect]
  | TTuple [Type]
  | TDynTrait TTrait
  deriving (Eq, Show, Generic)

data TPath = TPath [Name] Name -- path + module name
  deriving (Eq, Show, Generic)

data TCon = TCon Name [Type]
  deriving (Eq, Show, Generic)

newtype Constraint
  = CTrait TTrait
  deriving (Eq, Show, Generic)

data TTrait = TTrait (Maybe TPath) Name
  deriving (Eq, Show, Generic)

data FnKind
  = FreeFn
  | Method Type -- impl For which type
  | TraitMethod TTrait Bool -- whether have default impl
  deriving (Eq, Show, Generic)

-- \| TraitMethod

type Name = String

data FnSig = FnSig
  { funName :: Name,
    funParams :: [FnParam],
    funReturnType :: Type,
    funTyParams :: [(TCon, [Constraint])],
    funEff :: [Effect]
  }
  deriving (Eq, Show, Generic)

data FnParam
  = AnonParam Bool Type -- Anonymous parameter, e.g. `Type`
  | NamedParam Bool Name Type Bool Bool -- Named parameter, e.g. `name~ : Type = ../_`
  deriving (Eq, Show, Generic) -- NOTE: Bool indicates whether the parameter is mutable

data Attr
  = Deprecated (Maybe String) -- #deprecated("reason")
  | External (Maybe String)
  | Alias String (Maybe (Maybe String)) -- #alias(combine, deprecated="use add instead")
  deriving (Eq, Show, Generic)

data FnDecl' = FnDecl'
  { fnSig :: FnSig,
    fnAttr :: [Attr],
    fnKind :: FnKind
  }
  deriving (Eq, Show, Generic)

data Visibility
  = VisPub
  | VisPubOpen
  | VisPriv
  | VisPubAll
  deriving (Eq, Show, Generic)
