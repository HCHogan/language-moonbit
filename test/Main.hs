module Main (main) where

import Language.Moonbit.Mbti.Parser (pMbtiFile)
import           Control.Monad          (forM, forM_)
import           Data.Either            (isRight)
import qualified Data.Text.Lazy.IO      as TL
import           System.Directory       (doesDirectoryExist, getHomeDirectory,
                                         listDirectory)
import           System.FilePath        ((</>), takeExtension)
import           Test.Hspec
import           Text.Parsec            (parse)

collectMbti :: FilePath -> IO [FilePath]
collectMbti root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat . forM entries $ \e -> do
        let path = dir </> e
        isSub <- doesDirectoryExist path
        if isSub
          then go path
          else pure [path | takeExtension path == ".mbti"]

main :: IO ()
main = do
  home <- getHomeDirectory
  let coreRoot = home </> ".moon" </> "lib" </> "core"
  -- let coreRoot = home </> ".moongle" </> "packages"
  -- let coreRoot = home </> ".moongle" </> "packages" </> "Kaida-Amethyst" </> "llvm-0.2.1"

  mbtiFiles <- collectMbti coreRoot

  hspec $ describe "Parsing all core .mbti files" $ do
    forM_ mbtiFiles $ \fp ->
      it fp $ do
        txt <- TL.readFile fp
        parse pMbtiFile fp txt `shouldSatisfy` isRight
