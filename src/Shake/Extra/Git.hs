{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Shake.Extra.Git where

import           Data.Binary
import           Data.Data
import           Data.Maybe
import           Development.Shake
import           Development.Shake.Classes

-- | Require a change in current git HEAD revision as a rule
newtype GitHead = GitHead ()
  deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

-- | Result of asking for git head can be Nothing if the working dir
-- is unclean, or the HEAD commit
type instance RuleResult GitHead = Maybe String

newtype GhcPkgVersion = GhcPkgVersion String deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
type instance RuleResult GhcPkgVersion = Maybe String

gitHeadRule :: Rules (GitHead -> Action (Maybe String))
gitHeadRule = addOracle $ \GitHead{} -> do
  isClean <- checkGitWorkDirIsClean
  if not isClean
    then pure Nothing
    else computeHead

computeHead :: Action (Maybe String)
computeHead = do
  Stdout out <- cmd "git" [ "rev-parse", "HEAD" ]
  return $ listToMaybe $ lines out

checkGitWorkDirIsClean :: Action Bool
checkGitWorkDirIsClean = do
  Stdout out <- cmd "git" [ "status", "-s", "-uno" ]
  putInfo  out
  pure $ null $ filter (/= "") (lines out)
