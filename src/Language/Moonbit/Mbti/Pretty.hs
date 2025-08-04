{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Moonbit.Mbti.Pretty where

import Data.Text (Text)
import Language.Moonbit.Mbti.Syntax
import Prettyprinter
import Prettyprinter.Render.Text

--------------------------------------------------------------------------------
-- Top‑level entry points -------------------------------------------------------
--------------------------------------------------------------------------------

-- | Render a full 'MbtiFile' as lazy 'Text' using the default layout options.
renderMbtiFile :: MbtiFile -> Text
renderMbtiFile =
  renderStrict . layoutSmart defaultLayoutOptions . prettyMbtiFile

-- | Pretty‑print a 'MbtiFile'.
prettyMbtiFile :: MbtiFile -> Doc ann
prettyMbtiFile (MbtiFile modPath imports decls) =
  vsep $ header <> map prettyImport imports <> memptyLine <> map prettyDecl decls
  where
    header =
      [ "// Generated using `moon info`, DON'T EDIT IT",
        "package " <> dquotes (prettyModulePath modPath)
      ]

    prettyImport p = "import " <> dquotes (prettyModulePath p)

    -- insert a blank line only if there is anything following
    memptyLine | null decls = [] | otherwise = [prettyEmpty]

    prettyEmpty = pretty ("" :: String)

--------------------------------------------------------------------------------
-- Module paths ----------------------------------------------------------------
--------------------------------------------------------------------------------

prettyModulePath :: ModulePath -> Doc ann
prettyModulePath (ModulePath u m pkgSegs) = hcat . punctuate "/" $ map prettySeg segments
  where
    segments = [u, m] ++ pkgSegs
    prettySeg = pretty

--------------------------------------------------------------------------------
-- Declarations ----------------------------------------------------------------
--------------------------------------------------------------------------------

renderDecl :: Decl -> Text
renderDecl = renderStrict . layoutSmart defaultLayoutOptions . prettyDecl

prettyDecl :: Decl -> Doc ann
prettyDecl = \case
  FnDecl d -> prettyFnDecl d
  ImplForTypeDecl sig -> prettyImplSig sig
  ConstDecl n ty ->
    "const " <> pretty n <+> ":" <+> prettyType ty
  LetDecl n ty ->
    "let " <> pretty n <+> ":" <+> prettyType ty
  TypeDecl attrs vis ty mb ->
    vsep (map prettyAttr attrs)
      <+> prettyVis vis
      <+> "type"
      <+> prettyType ty
      <> maybe mempty (\t -> space <> "=" <+> prettyType t) mb
  TypeAliasDecl vis orig alias ->
    prettyVis vis <+> "type" <+> prettyType alias <+> "=" <+> prettyType orig
  StructDecl vis t fields ->
    vsep $
      [prettyVis vis <+> "struct" <+> prettyType t <+> "{"]
        <> map prettyField fields
        <> ["}"]
    where
      prettyField (nm, ty, mut) = "  " <> pretty nm <> ":" <+> prettyType ty <> if mut then " mut" else mempty
  EnumDecl vis t ctors ->
    vsep $
      [prettyVis vis <+> "enum" <+> prettyType t <+> "{"]
        <> map prettyCtor ctors
        <> ["}"]
    where
      prettyCtor (nm, params) = "  " <> pretty nm <+> parens (hsep (punctuate comma (map prettyParam params)))
  ErrorTypeDecl vis nm et ->
    prettyVis vis <+> "suberror" <+> pretty nm <+> prettyErrorType et
  TraitDecl vis tr cs fns ->
    vsep
      ( [prettyVis vis <+> "trait" <+> prettyTrait tr <> prettyConstraints cs <+> "{"]
          ++ map (indent 2 . prettyFnDecl) fns
          ++ ["}"]
      )
  TraitAliasDecl vis tr1 tr2 ->
    prettyVis vis <+> "trait" <+> prettyTrait tr1 <+> "=" <+> prettyTrait tr2
  FnAliasDecl vis mbPath ty newNm ->
    prettyVis vis
      <+> "fn"
      <+> maybe mempty (\p -> prettyTPath p <> "::") mbPath
      <> pretty newNm
      <+> "="
      <+> prettyType ty

--------------------------------------------------------------------------------
-- Helper: visibility -----------------------------------------------------------
--------------------------------------------------------------------------------

prettyVis :: Visibility -> Doc ann
prettyVis = \case
  VisPub -> "pub"
  VisPubOpen -> "pub(open)"
  VisPriv -> mempty
  VisPubAll -> "pub(all)"

--------------------------------------------------------------------------------
-- Impl for Type ---------------------------------------------------------------
--------------------------------------------------------------------------------

prettyImplSig :: ImplSig -> Doc ann
prettyImplSig (ImplSig tvs tr tgt) =
  "impl" <> prettyTyParams tvs <+> prettyTrait tr <+> "for" <+> prettyType tgt

--------------------------------------------------------------------------------
-- Function declarations --------------------------------------------------------
--------------------------------------------------------------------------------

prettyFnDecl :: FnDecl' -> Doc ann
prettyFnDecl (FnDecl' sig attrs kind) = vsep $ map prettyAttr attrs ++ [l]
  where
    l =
      "fn"
        <> prettyTyParams (funTyParams sig)
        <+> prettyFnName kind (funName sig)
        <> parens (hsep (punctuate comma (map prettyParam (funParams sig))))
        <+> "->"
        <+> prettyType (funReturnType sig)
        <> prettyEffects (funEff sig)

prettyAttr :: Attr -> Doc ann
prettyAttr = \case
  Deprecated Nothing -> "#deprecated"
  Deprecated (Just msg) -> "#deprecated(" <> pretty msg <> ")"
  External Nothing -> "#external"
  External (Just msg) -> "#external(" <> pretty msg <> ")"

--------------------------------------------------------------------------------
-- Pretty: function name with context ------------------------------------------
--------------------------------------------------------------------------------

prettyFnName :: FnKind -> Name -> Doc ann
prettyFnName kind nm = case kind of
  FreeFn -> pretty nm
  Method t -> prettyTypeBase t <> "::" <> pretty nm
  TraitMethod tr _ -> prettyTrait tr <> "::" <> pretty nm
  where
    -- For a method receiver type we only print the head constructor (matching the
    -- style in the prompt where the generic parameters are omitted, e.g. Array instead of Array[T]).
    prettyTypeBase = \case
      TName _ (TCon n _) -> pretty n
      other -> prettyType other -- best effort

--------------------------------------------------------------------------------
-- Pretty: type parameters / constraints ---------------------------------------
--------------------------------------------------------------------------------

prettyTyParams :: [(TCon, [Constraint])] -> Doc ann
prettyTyParams [] = mempty
prettyTyParams tvs = brackets . hsep . punctuate comma $ map prettyOne tvs
  where
    prettyOne (TCon n _, cs) = pretty n <> prettyConstraints cs

prettyConstraints :: [Constraint] -> Doc ann
prettyConstraints [] = mempty
prettyConstraints cs = " : " <> hsep (punctuate " + " (map prettyConstraint cs))

prettyConstraint :: Constraint -> Doc ann
prettyConstraint (CTrait tr) = prettyTrait tr

--------------------------------------------------------------------------------
-- Pretty: effects --------------------------------------------------------------
--------------------------------------------------------------------------------

prettyEffects :: [Effect] -> Doc ann
prettyEffects [] = mempty
prettyEffects es = space <> hsep (punctuate space (map prettyEff es))
  where
    prettyEff EffAsync = "async"
    prettyEff (EffException ex) = prettyEx ex

    prettyEx = \case
      NoAraise -> "raise"
      Araise ty -> "raise" <+> prettyType ty
      AraisePoly -> "raise?"

--------------------------------------------------------------------------------
-- Pretty: parameters -----------------------------------------------------------
--------------------------------------------------------------------------------

prettyParam :: FnParam -> Doc ann
prettyParam = \case
  AnonParam _ ty -> prettyType ty
  NamedParam _ name ty opt def ->
    pretty name <> prettyOpt opt <> " : " <> prettyType ty <> prettyDef def
    where
      prettyOpt True = "?"
      prettyOpt False = mempty
      prettyDef True = " = _" -- we do not track exact default value; underscore is canonical
      prettyDef False = mempty

--------------------------------------------------------------------------------
-- Pretty: error‑type payload ---------------------------------------------------
--------------------------------------------------------------------------------

prettyErrorType :: ErrorType -> Doc ann
prettyErrorType = \case
  ETNoPayload -> mempty
  ETSinglePayload ty -> space <> prettyType ty
  ETEnumPayload ctors -> space <> braces (hsep (punctuate comma (map prettyCtor ctors)))
    where
      prettyCtor (nm, ps) = pretty nm <> parens (hsep (punctuate comma (map prettyParam ps)))

--------------------------------------------------------------------------------
-- Pretty: types ---------------------------------------------------------------
--------------------------------------------------------------------------------

prettyType :: Type -> Doc ann
prettyType = go False
  where
    go _ (TName Nothing con) = prettyTCon con
    go _ (TName (Just p) con) = prettyTPath p <> "::" <> prettyTCon con
    go _ (TTuple ts) = tupled (map (go False) ts)
    go p (TFun as r effs) =
      parensIf p $
        hsep
          [ parens (hsep (punctuate comma (map (go False) as))),
            "->",
            go False r
          ]
          <> prettyEffects effs
    go _ (TDynTrait tr) = "&" <> prettyTrait tr

    parensIf True = parens
    parensIf False = id

prettyTCon :: TCon -> Doc ann
prettyTCon (TCon n []) = pretty n
prettyTCon (TCon n tys) = pretty n <> brackets (hsep (punctuate comma (map prettyType tys)))

prettyTPath :: TPath -> Doc ann
prettyTPath (TPath mods last') = hcat . punctuate "::" $ map pretty (mods ++ [last'])

prettyTrait :: TTrait -> Doc ann
prettyTrait (TTrait mbPath nm) = maybe mempty (\p -> prettyTPath p <> "::") mbPath <> pretty nm
