module Shake.Extra.GitSpec where

import           Control.Exception
import           Data.Functor
import           Development.Shake
import           Shake.Extra.Git
import           System.Directory  as D
import           System.FilePath
import           System.Posix.Temp
import           System.Process
import           Test.Hspec

spec :: Spec
spec = around withTemporaryDir $ describe "Git Head Oracle" $ do
  let
    system_ = void . system

    gitInit = do
        system_ "git init"
        writeFile "bar" "bar"
        system_ "git add bar"
        system_ "git commit -m commit"

  describe "Check clean working directory" $ do

    it "returns Clean given working directory is clean" $ \ dir -> do
      let rule = do
            want ["foo"]
            "foo" %> \ f -> do
              res <- checkGitWorkDirIsClean
              writeFile' f (show res)

      withCurrentDirectory dir $ do
        gitInit
        shake shakeOptions rule

      readFile (dir </> "foo") `shouldReturn` "True"

    it "returns Dirty given working directory has unchecked file" $ \ dir -> do
      let rule = do
            want ["foo"]
            "foo" %> \ f -> do
              writeFile' f "foo"
              cmd_ "git" [ "add" , f ]
              res <- checkGitWorkDirIsClean
              writeFile' f (show res)

      withCurrentDirectory dir $ do
        gitInit
        shake shakeOptions rule

      readFile (dir </> "foo") `shouldReturn` "False"

withTemporaryDir :: (String -> IO ()) -> IO ()
withTemporaryDir =
  bracket (mkdtemp "contracts-generator") removePathForcibly
