# language-moonbit

**IMPORTANT: Currently only `.mbti` parser are implemented.**

<!-- [![Haskell CI](https://github.com/HCHogan/language-moonbit/actions/workflows/haskell.yml/badge.svg)](https://github.com/HCHogan/language-moonbit/actions/workflows/haskell.yml) -->
<!-- [![Hackage](https://img.shields.io/hackage/v/language-moonbit.svg)](https://hackage.haskell.org/package/language-moonbit) -->
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A Haskell library for parsing Moonbit source code (`.mbt`) and Moonbit binary interface (`.mbti`) files.

## Features

-   Parses Moonbit source files.
-   Parses Moonbit interface (`.mbti`) files.
-   Provides a Haskell Abstract Syntax Tree (AST) for the Moonbit language.
-   Built with `parsec`.

## Installation

This project uses [Cabal](https://www.haskell.org/cabal/) to manage dependencies and build the project.

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/HCHogan/language-moonbit.git
    cd language-moonbit
    ```

2.  **Build the project:**
    ```bash
    cabal build
    ```

## Usage

To use this library in your own project, add `language-moonbit` to the `build-depends` section of your `.cabal` file.

Here is a basic example of how to parse an `.mbti` file's content:

```haskell
import Language.Moonbit.Mbti (parseMbti)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Text.Parsec (ParseError)

main :: IO ()
main = do
  let sourceFile = "example.mbti"
  mbtiContent <- TIO.readFile sourceFile
  case parseMbti sourceFile mbtiContent of
    Left err -> putStrLn $ "Failed to parse: " ++ show err
    Right mbtiData -> do
      putStrLn "Successfully parsed MBTI file:"
      print mbtiData
```

## Examples

Here are some examples of what this library can parse, taken from the test suite.

### Type Declarations

```haskell
parse pTypeDecl "" "type Node[K, V]"
-- Right (TypeDecl VisPriv (TName Nothing (TCon "Node" [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])])) Nothing)
```

### Function Types

```haskell
parse pTFun "" "async (K, V) -> V raise E"
-- Right (TFun [TName Nothing (TCon "K" []),TName Nothing (TCon "V" [])] (TName Nothing (TCon "V" [])) [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))])

parse pType "" "(A?,B) -> C raise?"
-- Right (TFun [TName Nothing (TCon "Option" [TName Nothing (TCon "A" [])]),TName Nothing (TCon "B" [])] (TName Nothing (TCon "C" [])) [EffException AraisePoly])
```

### Function Declarations

```haskell
parse pFnDecl "" "#deprecated async fn[A, B] Decimal::parse_decimal(T[A, Int], String) -> Self raise StrConvError"
-- Right (FnDecl' {fnSig = FnSig {funName = "parse_decimal", funParams = [AnonParam False (TName Nothing (TCon "T" [TName Nothing (TCon "A" []),TName Nothing (TCon "Int" [])])),AnonParam False (TName Nothing (TCon "String" []))], funReturnType = TName Nothing (TCon "Self" []), funTyParams = [(TCon "A" [],[]),(TCon "B" [],[])], funEff = [EffAsync,EffException (Araise (TName Nothing (TCon "StrConvError" [])))]}, fnAttr = [Deprecated Nothing], fnKind = Method (TName Nothing (TCon "Decimal" []))})

parse pFnDecl "" "fn[T : @quickcheck.Arbitary + FromJson] from_json(Json, path~ : JsonPath = ..) -> T raise JsonDecodeError"
-- Right (FnDecl' {fnSig = FnSig {funName = "from_json", funParams = [AnonParam False (TName Nothing (TCon "Json" [])),NamedParam False "path" (TName Nothing (TCon "JsonPath" [])) True False], funReturnType = TName Nothing (TName Nothing (TCon "T" [])), funTyParams = [(TCon "T" [],[CTrait (TTrait (Just (TPath [] "quickcheck")) "Arbitary"),CTrait (TTrait Nothing "FromJson")])], funEff = [EffException (Araise (TName Nothing (TCon "JsonDecodeError" [])))]}, fnAttr = [], fnKind = FreeFn})
```

### Package and Import Declarations

```haskell
parse pPackageDecl "" "package \"user/repo/path/to/module\""
-- Right (ModulePath {mpUserName = "user", mpModuleName = "repo", mpPackagePath = ["path","to","module"]})

parse pImportDecl "" "import (\"user1/repo1/path/to/module\" \n \"user2/repo2/another/path\")"
-- Right [ModulePath {mpUserName = "user1", mpModuleName = "repo1", mpPackagePath = ["path","to","module"]},ModulePath {mpUserName = "user2", mpModuleName = "repo2", mpPackagePath = ["another","path"]}]
```

## Building & Testing

The project is configured with GitHub Actions for continuous integration on Linux, macOS, and Windows.

-   **Build the library:**
    ```bash
    cabal build
    ```

-   **Run the test suite:**
    ```bash
    cabal test
    ```

## Contributing

Parsing failure on any legal moonbit sourcefile or `.mbti` file generated by `moon info` is considered a bug. Please do not hesitate to fire an issue.

Contributions are welcome! 

## License

This project is licensed under the **MIT License**. See the [LICENSE](./LICENSE) file for details.
