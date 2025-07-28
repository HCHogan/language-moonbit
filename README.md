# language-moonbit

WIP

Moonbit & Mbti parser for haskell

Some examples:

```
-- >>> parse pTypeDecl "" "type Node[K, V]"
-- Right (TypeDecl (TName Nothing (TCon "Node" [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])])))
-- >>> parse pTypeDecl "" "type Node"
-- Right (TypeDecl (TName Nothing (TCon "Node" [])))

-- >>> parse pTFun "" "async (K, V) -> V raise E"
-- Right (TFun [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])] (TName Nothing (TCon "V" [])) [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))])

-- >>> parse pTFun "" "(Int, Bool) -> String?"
-- Right (TFun [TName Nothing (TCon "Int" []),TName Nothing (TCon "Bool" [])] (TName Nothing (TCon "Option" [TName Nothing (TCon "String" [])])) [EffException NoAraise])

-- >>> parse pTFun "" "async (@btree/btree.T) -> U noaraise"
-- Right (TFun [TName (Just (TPath ["btree"] "btree")) (TCon "T" [])] (TName Nothing (TCon "U" [])) [EffAsync,EffException NoAraise])

-- >>> parse pType "" "(A?,B) -> C raise?"
-- Right (TFun [TName Nothing (TCon "Option" [TName Nothing (TCon "A" [])]),TName Nothing (TCon "B" [])] (TName Nothing (TCon "C" [])) [EffException AraisePoly])

-- >>> parse pFnDecl "" "#deprecated fn Decimal::parse_decimal(String) -> Self raise StrConvError"
-- Right (FnDecl' {fnSig = FnSig {funName = "parse_decimal", funParams = [(Nothing,TName Nothing (TCon "String" []))], funReturnType = TName Nothing (TCon "Self" []), funTyParams = [], funEff = [EffException (Araise (TName Nothing (TCon "StrConvError" [])))]}, fnAttr = [Deprecated Nothing], fnKind = Method (TName Nothing (TCon "Decimal" []))})

-- >>> parse pFnDecl "" "fn parse_int(String, base~ : Int = ..) -> Int raise StrConvError"
-- Right (FnDecl' {fnSig = FnSig {funName = "parse_int", funParams = [(Nothing,TName Nothing (TCon "String" [])),(Just "base",TName Nothing (TCon "Int" []))], funReturnType = TName Nothing (TCon "Int" []), funTyParams = [], funEff = [EffException (Araise (TName Nothing (TCon "StrConvError" [])))]}, fnAttr = [], fnKind = FreeFn})

-- >>> parse pImportDecl "" "import (\"user1/repo1/path/to/module\" \"user2/repo2/another/path\")"
-- Right [ModulePath {mpUserName = "user1", mpModuleName = "repo1", mpPackagePath = ["path","to","module"]},ModulePath {mpUserName = "user2", mpModuleName = "repo2", mpPackagePath = ["another","path"]}]
```
