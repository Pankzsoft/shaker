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

    it "returns True given working directory is clean" $ \ dir -> do
      let rule = do
            want ["foo"]
            "foo" %> \ f -> do
              res <- checkGitWorkDirIsClean
              writeFile' f (show res)

      withCurrentDirectory dir $ do
        gitInit
        shake shakeOptions rule

      readFile (dir </> "foo") `shouldReturn` "True"

    it "returns False given working directory has unchecked file" $ \ dir -> do
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

  describe "Git Head rule" $ do

    it "returns Nothing given working directory is dirty" $ \ dir -> do
      let rule = do
            askGit <- gitHeadRule
            want ["foo"]
            "foo" %> \ f -> do
              writeFile' f "foo"
              cmd_ "git" [ "add" , f ]
              hd <- askGit (GitHead ())
              writeFile' f (show hd)

      withCurrentDirectory dir $ do
        gitInit
        shake shakeOptions rule

      readFile (dir </> "foo") `shouldReturn` "Nothing"

    it "returns HEAD commit given working directory is clean" $ \ dir -> do
      let rule = do
            askGit <- gitHeadRule
            want ["foo"]
            "foo" %> \ f -> do
              hd <- askGit (GitHead ())
              writeFile' f (show hd)

      withCurrentDirectory dir $ do
        gitInit
        shake shakeOptions rule

      readFile (dir </> "foo") `shouldNotReturn` "Nothing"


withTemporaryDir :: (String -> IO ()) -> IO ()
withTemporaryDir =
  bracket (mkdtemp "contracts-generator") removePathForcibly
