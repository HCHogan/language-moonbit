{-# LANGUAGE OverloadedStrings #-}

module Language.Moonbit.Mbti.Parser where

import Control.Monad
import Data.Functor
import Language.Moonbit.Lexer
import Language.Moonbit.Mbti.Syntax
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

attrCtors :: [(String, Maybe String -> FnAttr)]
attrCtors =
  [ ("deprecated", Deprecated),
    ("external", External)
  ]

-- >>> parse pAttr "" "#deprecated(\"use xxx instead\")"
-- Right (Deprecated (Just "use xxx instead"))

pAttr :: Parser FnAttr
pAttr = do
  _ <- char '#' <?> "‘#’"
  name <- choice (map ((try . string) . fst) attrCtors) <?> "attribute name"
  arg <- optionMaybe $ parens stringLit -- optional argument for the attribute
  case lookup name attrCtors of
    Just ctor -> return $ ctor arg
    Nothing -> parserFail $ "unknown attribute: " ++ name

-- >>> parse pTypeDecl "" "type Node[K, V]"
-- Right (TypeDecl VisPriv (TName Nothing (TCon "Node" [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])])) Nothing)

pTypeDecl :: Parser Decl
pTypeDecl = do
  vis <- pVisibility
  _ <- reserved RWType
  name <- identifier
  tyargs <- option [] (brackets (commaSep identifier))
  inner <- optionMaybe pType
  return $ TypeDecl vis (TName Nothing (TCon name $ (\n -> TName Nothing $ TCon n []) <$> tyargs)) inner

-- >>> parse pConstDecl "" "const PI : Double = 0x3.243F6A8885A308CA8A54"
-- Right (ConstDecl "PI" (TName Nothing (TCon "Double" [])))

pConstDecl :: Parser Decl
pConstDecl = do
  _ <- reserved RWConst
  name <- identifier
  _ <- reservedOp OpColon
  ty <- pType
  optional skipLine
  whiteSpace
  return $ ConstDecl name ty
  where
    skipLine :: Parser () -- HACK: this is a hack to skip the rest of the line after the type declaration
    skipLine = void $ manyTill anyChar (void endOfLine <|> eof)

-- >>> parse pLetDecl "" "let x : Int = 42"
-- Right (LetDecl "x" (TName Nothing (TCon "Int" [])))

pLetDecl :: Parser Decl
pLetDecl = do
  _ <- reserved RWLet
  name <- identifier
  _ <- reservedOp OpColon
  LetDecl name <$> pType

pLit :: Parser ()
pLit =
  choice
    [ void $ try stringLit,
      void $ try charLiteral,
      void $ try integer,
      void $ try natural,
      void $ try float,
      void $ try hexadecimal,
      void $ try octal
    ]

-- >>> parse pImplForTypeDecl "" "impl[K: Compare + Eq, V : Show] Div for Node[K, V]"
-- Right (ImplForTypeDecl (ImplSig [(TCon "K" [],[CTrait (TTrait Nothing "Compare"),CTrait (TTrait Nothing "Eq")]),(TCon "V" [],[CTrait (TTrait Nothing "Show")])] (TTrait Nothing "Div") (TName Nothing (TCon "Node" [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])]))))

pImplForTypeDecl :: Parser Decl
pImplForTypeDecl = do
  _ <- reserved RWImpl
  typs <- option [] pTyParams
  trait <- pTTrait
  _ <- reserved RWFor
  ImplForTypeDecl . ImplSig typs trait <$> pType

pTFun :: Parser Type
pTFun = do
  isAsync <- option False (reserved RWAsync $> True)
  args <- parens (commaSep pType)
  reservedOp OpArrow
  retTy <- pType
  effEx <- option NoAraise pEffectException
  let effects = [EffAsync | isAsync] ++ [EffException effEx]
  return (TFun args retTy effects)

pEffectException :: Parser EffectException
pEffectException =
  try (reserved RWRaisePoly $> AraisePoly)
    <|> try (reserved RWNoRaise $> NoAraise)
    <|> do
      reserved RWRaise
      Araise <$> option (TName Nothing $ TCon "Error" []) pType
    <?> "exception effect (raise?, noaraise or raise T)"

pTTuple :: Parser Type
pTTuple = TTuple <$> parens (commaSep pType)

pTPath :: Parser TPath
pTPath = do
  _ <- symbol "@"
  segs <- pModuleSeg `sepBy1` slash
  return $ TPath (init segs) (last segs) -- SAFETY: sepBy1 ensures at least one segment

pTCon :: Parser TCon
pTCon = do
  name <- identifier
  tyargs <- option [] (brackets (commaSep pType))
  return $ TCon name tyargs

-- TCon without the ?
pTAtom :: Parser Type
pTAtom = do
  mpath <- optionMaybe (pTPath <* reservedOp OpDot)
  TName mpath <$> pTCon

pTDynTrait :: Parser Type
pTDynTrait = symbol "&" *> (TDynTrait <$> pTTrait)

-- >>> parse pType "" "T??"
-- Right (TName Nothing (TCon "Option" [TName Nothing (TCon "Option" [TName Nothing (TCon "T" [])])]))

pTNonFun :: Parser Type
pTNonFun = do
  base <- try pTTuple <|> pTAtom
  qs <- many (symbol "?")
  let wrap t = TName Nothing (TCon "Option" [t])
      ty = foldl (\t _ -> wrap t) base qs
  return ty

-- >>> parse pTFun "" "async (K, V) -> V raise E"
-- Right (TFun [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])] (TName Nothing (TCon "V" [])) [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))])

-- >>> parse pTFun "" "(T[(&Show) -> Int, String], Bool) -> String? raise"
-- Right (TFun [TName Nothing (TCon "T" [TFun [TDynTrait (TTrait Nothing "Show")] (TName Nothing (TCon "Int" [])) [EffException NoAraise],TName Nothing (TCon "String" [])]),TName Nothing (TCon "Bool" [])] (TName Nothing (TCon "Option" [TName Nothing (TCon "String" [])])) [EffException (Araise (TName Nothing (TCon "Error" [])))])

-- >>> parse pTFun "" "async (@btree/btree.T) -> U noaraise"
-- Right (TFun [TName (Just (TPath ["btree"] "btree")) (TCon "T" [])] (TName Nothing (TCon "U" [])) [EffAsync,EffException NoAraise])

-- >>> parse pType "" "(A?,B) -> C raise?"
-- Right (TFun [TName Nothing (TCon "Option" [TName Nothing (TCon "A" [])]),TName Nothing (TCon "B" [])] (TName Nothing (TCon "C" [])) [EffException AraisePoly])

pType :: Parser Type
pType =
  choice
    [ try pTFun,
      try pTDynTrait,
      pTNonFun
    ]
    <?> "type"

-- skip a default value (`= ..`)
pPDefault :: Parser ()
pPDefault = reservedOp OpEq *> reservedOp OpDotDot

-- skip a autofill value (`= _`)
pPAutoFill :: Parser ()
pPAutoFill = void $ reservedOp OpEq *> reservedOp OpUnderscore

pTTrait :: Parser TTrait
pTTrait = do
  mpath <- optionMaybe (pTPath <* reservedOp OpDot)
  TTrait mpath <$> identifier

-- parse one constraint, e.g. `Compare`
pConstraint :: Parser Constraint
pConstraint = CTrait <$> pTTrait

-- Type parameters: [T1, T2: C1, T3: C2 + C3]
pTyParams :: Parser [(TCon, [Constraint])]
pTyParams = brackets (commaSep1 pTyParam)
  where
    pTyParam = do
      name <- identifier
      let tc = TCon name []
      cs <- option [] (reservedOp OpColon *> pConstraint `sepBy1` reservedOp OpPlus)
      return (tc, cs)

-- >>> parse pTyParams "" "[T1, T2: C1, T3: @p1/p2.C2 + C3]"
-- Right [(TCon "T1" [],[]),(TCon "T2" [],[CTrait (TTrait Nothing "C1")]),(TCon "T3" [],[CTrait (TTrait (Just (TPath ["p1"] "p2")) "C2"),CTrait (TTrait Nothing "C3")])]

-- parse a single function parameter, named or not
pParam :: Parser FnParam
pParam = try pNamed <|> pAnon
  where
    pNamed :: Parser FnParam
    pNamed = do
      isMut <- option False (reserved RWMut $> True)
      nm <- identifier
      isOptional <- option False (reservedOp OpQuestion $> True)
      unless isOptional $ reservedOp OpTilde
      reservedOp OpColon
      ty <- pType
      au <- option False (try (pPAutoFill $> True))
      de <- option False (try (pPDefault $> True))
      return $ NamedParam isMut nm (if isOptional then TName Nothing $ TCon "Option" [ty] else ty) de au

    pAnon :: Parser FnParam
    pAnon = do
      isMut <- option False (reserved RWMut $> True)
      AnonParam isMut <$> pType

-- parse the FnKind + plain name
pKindName :: Parser (FnKind, Name)
pKindName = try method <|> free
  where
    method = do
      recv <- pTAtom
      reservedOp OpColonColon
      nm <- identifier
      return (Method recv, nm)

    free = do
      nm <- identifier
      return (FreeFn, nm)

-- the top-level parser
pFnDecl :: Parser FnDecl'
pFnDecl = do
  whiteSpace
  attrs <- many (pAttr <* whiteSpace) -- 1) attributes
  isAsync <- option False (reserved RWAsync $> True) -- 2) async keyword
  reserved RWFn -- 2) the fn keyword + optional type params
  typs <- option [] pTyParams -- 3) kind (free vs method) and the simple name
  (kind, nm) <- pKindName -- 4) parameter list
  params <- parens (commaSep pParam) -- 5) return arrow + return type
  reservedOp OpArrow
  retTy <- pType -- 6) exception effects
  effEx <- option NoAraise pEffectException
  let effects = [EffAsync | isAsync] ++ [EffException effEx]
  let sig = FnSig {funName = nm, funParams = params, funReturnType = retTy, funTyParams = typs, funEff = effects}
  return $ FnDecl' sig attrs kind

pTraitMethod :: TTrait -> Parser FnDecl'
pTraitMethod trait = do
  isAsync <- option False (reserved RWAsync $> True) -- 1) async keyword
  nm <- identifier
  params <- parens (commaSep pParam)
  reservedOp OpArrow
  retTy <- pType
  effEx <- option NoAraise pEffectException
  -- NOTE: default implementation of method use `= _`, in contrast to `= ..` which is a default value for a parameter
  de <- option False (pPAutoFill $> True)
  let effects = [EffAsync | isAsync] ++ [EffException effEx]
  let sig = FnSig {funName = nm, funParams = params, funReturnType = retTy, funTyParams = [], funEff = effects}
  return $ FnDecl' sig [] (TraitMethod trait de)

-- >>> parse pTraitDecl "" "pub(open) trait Compare : Eq { compare(Self, Self) -> Int = _ }"
-- Right (TraitDecl VisPubOpen (TTrait Nothing "Compare") [CTrait (TTrait Nothing "Eq")] [FnDecl' {fnSig = FnSig {funName = "compare", funParams = [AnonParam False (TName Nothing (TCon "Self" [])),AnonParam False (TName Nothing (TCon "Self" []))], funReturnType = TName Nothing (TCon "Int" []), funTyParams = [], funEff = [EffException NoAraise]}, fnAttr = [], fnKind = TraitMethod (TTrait Nothing "Compare") True}])

pTraitDecl :: Parser Decl
pTraitDecl = do
  vis <- pVisibility
  _ <- reserved RWTrait
  trait <- pTTrait
  cs <- option [] $ reservedOp OpColon *> commaSep pConstraint
  methods <- braces $ many (pTraitMethod trait)
  return $ TraitDecl vis trait cs methods

-- >>> parse pEnumDecl "" "pub(all) enum MyEnum[T] { A(T) B(T, fuck~ : T) }"
-- Right (EnumDecl VisPubAll (TName Nothing (TCon "MyEnum" [TName Nothing (TCon "T" [])])) [("A",[AnonParam False (TName Nothing (TCon "T" []))]),("B",[AnonParam False (TName Nothing (TCon "T" [])),NamedParam False "fuck" (TName Nothing (TCon "T" [])) False False])])

pEnumDecl :: Parser Decl
pEnumDecl = do
  vis <- pVisibility
  _ <- reserved RWEnum
  enumTy <- pType
  xs <- braces $ many enumVariant
  return $ EnumDecl vis enumTy xs

enumVariant :: Parser (Name, [FnParam])
enumVariant = do
  nm <- identifier
  elems <- option [] $ parens (commaSep pParam)
  return (nm, elems)

-- >>> parse pErrorTypeDecl "" "pub suberror Eff { Get((Int) -> Unit) Set(Int, (Unit) -> Unit) }"
-- Right (ErrorTypeDecl VisPub "Eff" (ETEnumPayload [("Get",[AnonParam False (TFun [TName Nothing (TCon "Int" [])] (TName Nothing (TCon "Unit" [])) [EffException NoAraise])]),("Set",[AnonParam False (TName Nothing (TCon "Int" [])),AnonParam False (TFun [TName Nothing (TCon "Unit" [])] (TName Nothing (TCon "Unit" [])) [EffException NoAraise])])]))

pErrorTypeDecl :: Parser Decl
pErrorTypeDecl = do
  vis <- pVisibility
  _ <- reserved RWSuberror
  -- NOTE: we use Name here because error type declarations are not allowed to have type parameters
  name <- identifier
  et <- option ETNoPayload $ choice [try singlePayload, try enumPayload]
  return $ ErrorTypeDecl vis name et
  where
    singlePayload = ETSinglePayload <$> pType

    enumPayload = ETEnumPayload <$> braces (many enumVariant)

-- >>> parse pTypeAliasDecl "" "pub typealias @moonbitlang/core/bigint.BigInt as BigInt"
-- Right (TypeAliasDecl VisPub (TName (Just (TPath ["moonbitlang","core"] "bigint")) (TCon "BigInt" [])) (TName Nothing (TCon "BigInt" [])))

pTypeAliasDecl :: Parser Decl
pTypeAliasDecl = do
  vis <- pVisibility
  _ <- reserved RWTypealias
  orig <- pType
  _ <- reserved RWAs
  TypeAliasDecl vis orig <$> pType

-- >>> parse pFnAliasDecl "" "fnalias Int::abs"
-- Right (FnAliasDecl VisPriv Nothing (TName Nothing (TCon "Int" [])) "abs")

pFnAliasDecl :: Parser Decl
pFnAliasDecl = do
  vis <- pVisibility
  _ <- reserved RWFnAlias
  path <- option Nothing (Just <$> pTPath)
  ty <- pType
  _ <- reservedOp OpColonColon
  FnAliasDecl vis path ty <$> identifier

-- >>> parse pTraitAliasDecl "" "pub traitalias @builtin.BitXOr as BitXOr"
-- Right (TraitAliasDecl VisPub (TTrait (Just (TPath [] "builtin")) "BitXOr") (TTrait Nothing "BitXOr"))

pTraitAliasDecl :: Parser Decl
pTraitAliasDecl = do
  vis <- pVisibility
  _ <- reserved RWTraitalias
  orig <- pTTrait
  _ <- reserved RWAs
  TraitAliasDecl vis orig <$> pTTrait

-- >>> parse pStructDecl "" "pub(all) struct SparseArray[X] { elem_info : Bitset \n data : FixedArray[X] }"
-- Right (StructDecl VisPubAll (TName Nothing (TCon "SparseArray" [TName Nothing (TCon "X" [])])) [("elem_info",TName Nothing (TCon "Bitset" []),False),("data",TName Nothing (TCon "FixedArray" [TName Nothing (TCon "X" [])]),False)])

pStructDecl :: Parser Decl
pStructDecl = do
  vis <- pVisibility
  _ <- reserved RWStruct
  name <- pType
  fields <- braces (many fieldDecl)
  return $ StructDecl vis name fields
  where
    fieldDecl = do
      isMut <- option False (reserved RWMut $> True)
      nm <- identifier
      _ <- reservedOp OpColon
      ty <- pType
      return (nm, ty, isMut)

pVisibility :: Parser Visibility
pVisibility = do
  option VisPriv ((try pubOpen <|> try pubAll <|> try pub <|> priv) <?> "visibility specifier")
  where
    pubOpen = do
      _ <- reserved RWPub
      _ <- parens (symbol "open")
      pure VisPubOpen

    pubAll = do
      _ <- reserved RWPub
      _ <- parens (symbol "all")
      pure VisPubAll

    pub = reserved RWPub $> VisPub

    priv = reserved RWPriv $> VisPriv

-- >>> parse pFnDecl "" "#deprecated async fn[A, B] Decimal::parse_decimal(T[A, Int], String) -> Self raise StrConvError"
-- Right (FnDecl' {fnSig = FnSig {funName = "parse_decimal", funParams = [AnonParam False (TName Nothing (TCon "T" [TName Nothing (TCon "A" []),TName Nothing (TCon "Int" [])])),AnonParam False (TName Nothing (TCon "String" []))], funReturnType = TName Nothing (TCon "Self" []), funTyParams = [(TCon "A" [],[]),(TCon "B" [],[])], funEff = [EffAsync,EffException (Araise (TName Nothing (TCon "StrConvError" [])))]}, fnAttr = [Deprecated Nothing], fnKind = Method (TName Nothing (TCon "Decimal" []))})

-- >>> parse pFnDecl "" "fn inspect(&ToJson, content~ : Json, loc~ : SourceLoc = _, args_loc~ : ArgsLoc = _) -> Unit raise InspectError"
-- Right (FnDecl' {fnSig = FnSig {funName = "inspect", funParams = [AnonParam False (TDynTrait (TTrait Nothing "ToJson")),NamedParam False "content" (TName Nothing (TCon "Json" [])) False False,NamedParam False "loc" (TName Nothing (TCon "SourceLoc" [])) False True,NamedParam False "args_loc" (TName Nothing (TCon "ArgsLoc" [])) False True], funReturnType = TName Nothing (TCon "Unit" []), funTyParams = [], funEff = [EffException (Araise (TName Nothing (TCon "InspectError" [])))]}, fnAttr = [], fnKind = FreeFn})

-- >>> parse pFnDecl "" "fn[T : @quickcheck.Arbitary + FromJson] from_json(Json, path~ : JsonPath = ..) -> T raise JsonDecodeError"
-- Right (FnDecl' {fnSig = FnSig {funName = "from_json", funParams = [AnonParam False (TName Nothing (TCon "Json" [])),NamedParam False "path" (TName Nothing (TCon "JsonPath" [])) True False], funReturnType = TName Nothing (TCon "T" []), funTyParams = [(TCon "T" [],[CTrait (TTrait (Just (TPath [] "quickcheck")) "Arbitary"),CTrait (TTrait Nothing "FromJson")])], funEff = [EffException (Araise (TName Nothing (TCon "JsonDecodeError" [])))]}, fnAttr = [], fnKind = FreeFn})

-- >>> parse pFnDecl "" "fn[T] Option::flatten(T??) -> T?"
-- Right (FnDecl' {fnSig = FnSig {funName = "flatten", funParams = [AnonParam False (TName Nothing (TCon "Option" [TName Nothing (TCon "Option" [TName Nothing (TCon "T" [])])]))], funReturnType = TName Nothing (TCon "Option" [TName Nothing (TCon "T" [])]), funTyParams = [(TCon "T" [],[])], funEff = [EffException NoAraise]}, fnAttr = [], fnKind = Method (TName Nothing (TCon "Option" []))})

-- >>> parse pPackageDecl "" "package \"user/repo/path/to/module\""
-- Right (ModulePath {mpUserName = "user", mpModuleName = "repo", mpPackagePath = ["path","to","module"]})

pPackageDecl :: Parser ModulePath
pPackageDecl = reserved RWPackage *> pQuotedModulePath

-- >>> parse pImportDecl "" "import (\"user1/repo1/path/to/module\" \n \"user2/repo2/another/path\")"
-- Right [ModulePath {mpUserName = "user1", mpModuleName = "repo1", mpPackagePath = ["path","to","module"]},ModulePath {mpUserName = "user2", mpModuleName = "repo2", mpPackagePath = ["another","path"]}]

pImportDecl :: Parser [ModulePath]
pImportDecl = reserved RWImport *> parens (many $ spaces *> pQuotedModulePath <* spaces)

-- >>> parse pModulePath "" "user/modname/path/to/module"
-- Right (ModulePath {mpUserName = "user", mpModuleName = "modname", mpPackagePath = ["path","to","module"]})

pModulePath :: Parser ModulePath
pModulePath = do
  segments <- pModuleSeg `sepBy1` slash
  case segments of
    user : modName : rest -> return $ ModulePath user modName rest
    _ -> parserFail "module path must be in the form ‘user/modname(/path)"

pModuleSeg :: Parser String
pModuleSeg = do
  first <- alphaNum <|> char '_'
  rest  <- many (alphaNum <|> char '-' <|> char '_')
  pure $ first : rest

pQuotedModulePath :: Parser ModulePath
pQuotedModulePath = between (char '"') (char '"') pModulePath

pDecl :: Parser Decl
pDecl =
  choice
    [ FnDecl <$> try pFnDecl,
      try pImplForTypeDecl,
      try pConstDecl,
      try pLetDecl,
      try pTypeDecl,
      try pTypeAliasDecl,
      try pStructDecl,
      try pEnumDecl,
      try pErrorTypeDecl,
      try pTraitDecl,
      try pTraitAliasDecl,
      try pFnAliasDecl
    ]

pMbtiFile :: Parser MbtiFile
pMbtiFile = contents $ do
  p <- pPackageDecl
  whiteSpace
  is <- option [] pImportDecl
  decls <- many pDecl
  return $ MbtiFile p is decls
