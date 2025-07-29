{-# LANGUAGE OverloadedStrings #-}

module Language.Moonbit.Mbti.Parser where

import Data.Functor
import Language.Moonbit.Lexer
import Language.Moonbit.Mbti.Syntax
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

attrCtors :: [(String, Maybe String -> FnAttr)]
attrCtors =
  [ ("deprecated", Deprecated)
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
-- Right (TypeDecl (TName Nothing (TCon "Node" [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])])))

pTypeDecl :: Parser Decl
pTypeDecl = do
  _ <- reserved RWType
  name <- identifier
  tyargs <- option [] (brackets (commaSep identifier))
  return $ TypeDecl (TName Nothing (TCon name $ (\n -> TName Nothing $ TCon n []) <$> tyargs))

-- >>> parse pConstDecl "" "const PI : Double = 0x3.243F6A8885A308CA8A54"
-- Right (ConstDecl "PI" (TName Nothing (TCon "Double" [])))

pConstDecl :: Parser Decl
pConstDecl = do
  _ <- reserved RWConst
  name <- identifier
  _ <- reservedOp OpColon
  ty <- pType
  _ <- optional (reservedOp OpEq <* pLit)
  return $ ConstDecl name ty

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
      Araise <$> pType
    <?> "exception effect (raise?, noaraise or raise T)"

pTTuple :: Parser Type
pTTuple = TTuple <$> parens (commaSep pType)

pTPath :: Parser TPath
pTPath = do
  _ <- symbol "@"
  segs <- identifier `sepBy1` slash
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

pTNonFun :: Parser Type
pTNonFun = do
  base <- try pTTuple <|> pTAtom
  isOpt <- option False (symbol "?" $> True)
  if isOpt
    then return $ TName Nothing (TCon "Option" [base])
    else return base

-- >>> parse pTFun "" "async (K, V) -> V raise E"
-- Right (TFun [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])] (TName Nothing (TCon "V" [])) [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))])

-- >>> parse pTFun "" "(T[(&Show) -> Int, String], Bool) -> String?"
-- Right (TFun [TName Nothing (TCon "T" [TFun [TDynTrait (TTrait Nothing "Show")] (TName Nothing (TCon "Int" [])) [EffException NoAraise],TName Nothing (TCon "String" [])]),TName Nothing (TCon "Bool" [])] (TName Nothing (TCon "Option" [TName Nothing (TCon "String" [])])) [EffException NoAraise])

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
      nm <- identifier
      reservedOp OpTilde
      reservedOp OpColon
      ty <- pType
      au <- option False (try (pPAutoFill $> True))
      de <- option False (try (pPDefault $> True))
      return $ NamedParam nm ty de au

    pAnon :: Parser FnParam
    pAnon = AnonParam <$> pType

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

-- >>> parse pFnDecl "" "#deprecated async fn[A, B] Decimal::parse_decimal(T[A, Int], String) -> Self raise StrConvError"
-- Right (FnDecl' {fnSig = FnSig {funName = "parse_decimal", funParams = [AnonParam (TName Nothing (TCon "T" [TName Nothing (TCon "A" []),TName Nothing (TCon "Int" [])])),AnonParam (TName Nothing (TCon "String" []))], funReturnType = TName Nothing (TCon "Self" []), funTyParams = [(TCon "A" [],[]),(TCon "B" [],[])], funEff = [EffAsync,EffException (Araise (TName Nothing (TCon "StrConvError" [])))]}, fnAttr = [Deprecated Nothing], fnKind = Method (TName Nothing (TCon "Decimal" []))})

-- >>> parse pFnDecl "" "fn inspect(&ToJson, content~ : Json, loc~ : SourceLoc = _, args_loc~ : ArgsLoc = _) -> Unit raise InspectError"
-- Right (FnDecl' {fnSig = FnSig {funName = "inspect", funParams = [AnonParam (TDynTrait (TTrait Nothing "ToJson")),NamedParam "content" (TName Nothing (TCon "Json" [])) False False,NamedParam "loc" (TName Nothing (TCon "SourceLoc" [])) False True,NamedParam "args_loc" (TName Nothing (TCon "ArgsLoc" [])) False True], funReturnType = TName Nothing (TCon "Unit" []), funTyParams = [], funEff = [EffException (Araise (TName Nothing (TCon "InspectError" [])))]}, fnAttr = [], fnKind = FreeFn})

-- >>> parse pFnDecl "" "fn[T : @quickcheck.Arbitary + FromJson] from_json(Json, path~ : JsonPath = ..) -> T raise JsonDecodeError"
-- Right (FnDecl' {fnSig = FnSig {funName = "from_json", funParams = [AnonParam (TName Nothing (TCon "Json" [])),NamedParam "path" (TName Nothing (TCon "JsonPath" [])) True False], funReturnType = TName Nothing (TCon "T" []), funTyParams = [(TCon "T" [],[CTrait (TTrait (Just (TPath [] "quickcheck")) "Arbitary"),CTrait (TTrait Nothing "FromJson")])], funEff = [EffException (Araise (TName Nothing (TCon "JsonDecodeError" [])))]}, fnAttr = [], fnKind = FreeFn})

-- >>> parse pPackageDecl "" "package \"user/repo/path/to/module\""
-- Right (ModulePath {mpUserName = "user", mpModuleName = "repo", mpPackagePath = ["path","to","module"]})

pPackageDecl :: Parser ModulePath
pPackageDecl = reserved RWPackage *> pQuotedModulePath

-- >>> parse pImportDecl "" "import (\"user1/repo1/path/to/module\" \"user2/repo2/another/path\")"
-- Right [ModulePath {mpUserName = "user1", mpModuleName = "repo1", mpPackagePath = ["path","to","module"]},ModulePath {mpUserName = "user2", mpModuleName = "repo2", mpPackagePath = ["another","path"]}]

pImportDecl :: Parser [ModulePath]
pImportDecl = reserved RWImport *> parens (spaces *> pQuotedModulePath `sepBy` spaces <* spaces)

-- >>> parse pModulePath "" "user/modname/path/to/module"
-- Right (ModulePath {mpUserName = "user", mpModuleName = "modname", mpPackagePath = ["path","to","module"]})

pModulePath :: Parser ModulePath
pModulePath = do
  segments <- identifier `sepBy1` slash
  case segments of
    user : modName : pName1 : rest -> return $ ModulePath user modName (pName1 : rest) -- make sure to have at least 1 segment after the module name
    _ -> parserFail "module path must be in the form ‘user/modname/path"

pQuotedModulePath :: Parser ModulePath
pQuotedModulePath = between (char '"') (char '"') pModulePath
