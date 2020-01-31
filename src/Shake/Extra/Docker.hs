{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-| A simple implementation of Shake rules to handle docker images.

** Usage

Here is a minimalist Shake file to build a single docker image. Note the
use of a "phony" rule as an extra level of indirection to be able to
`want` the image built. IT's not possible to directly `want` an image in
this list hence the need to use a plain string that's never actually
converted into a file.

@@
main = shakeArgs options $ do

  addBuiltinDockerRule

  want [ "haskell.img" ]

  ".haskell.img" ~> void  (needImage "gcr.io/symbiont-rex/haskell")

  imageRule "gcr.io/symbiont-rex/haskell" $ \ img -> do
    need [ "docker/haskell/Dockerfile" ]
    cmd "docker" [ "build", "-t", toArg img, "docker/haskell" ]
@@

** References:

 * <https://hackage.haskell.org/package/shake-0.17.3/docs/Development-Shake-Rule.html#t:BuiltinIdentity Shake API>

 * <https://tech-blog.capital-match.com/posts/5-upgrading-shake.html Capital Match Tech blog> on upgrading from 0.15.X to 0.16.Y

-}
module Shake.Extra.Docker where

import           Data.Aeson                as Aeson
import           Data.Binary
import qualified Data.ByteString           as BS
import           Data.String
import qualified Data.Text                 as Text
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Rule
import           GHC.Generics
import           System.Exit

-- * Docker Rules
-- see <https://hackage.haskell.org/package/shake-0.18.4/docs/Development-Shake-Rule.html#t:BuiltinIdentity Shake API> documentation for details

-- ** Images
--
-- We define the needed types as a bunch of @newtype@s to make it easy to derive the mandatory
-- typeclasses for Shake. Might be slightly overkill at this stage...

-- | Identifies a docker "repository"
newtype Repo = Repo String
    deriving newtype (Show, Eq, Hashable, Binary, NFData)

-- | Identifies a docker tag
newtype Tag = Tag String
    deriving newtype (Show, Eq, Hashable, Binary, NFData)

-- |The hash value identifying a docker image
-- Note that for simplicity the contained string is prefixed with @sha256:@ as this
-- is what is output by the command
--
-- @@
--  docker images --no-trunc -q repo:tag
-- @@
newtype SHA256 = SHA256 { unSha256 :: String }
    deriving newtype (Show, Eq, Hashable, Binary, NFData, ToJSON, FromJSON)

emptySha :: BS.ByteString
emptySha = encodeUtf8 $ Text.pack "sha256:000000000000000000000000000000000000000000000000000000000000000"

newtype Image = Image (Repo,Tag)
    deriving newtype (Show, Eq, Hashable, Binary, NFData)

instance IsString Image where
  fromString s =  Image (r, t)
    where
      (part1, part2) = break (':' ==) s
      (r, t) = ( Repo part1
               , Tag $ if null part2 then "latest" else tail part2)

toArg :: Image -> String
toArg (Image (Repo r, Tag t)) = r <> ":" <> t

imageSha :: Image -> Action [Word8]
imageSha img = do
  Stdout out <- cmd "docker" [ "images", "--no-trunc", "-q", toArg img ]
  pure $ BS.unpack $ if BS.null out
                     then emptySha
                     else out

type instance RuleResult Image = SHA256

data ImageRule = ImageRule Image (Action ())

imageRule :: String -> (Image -> Action ()) -> Rules ()
imageRule s a = addUserRule $ ImageRule img (a img)
  where
    img = fromString s

needImage :: String -> Action SHA256
needImage = apply1 . fromString @Image

toBytes :: String -> BS.ByteString
toBytes = encodeUtf8 . Text.pack

fromBytes :: BS.ByteString -> String
fromBytes = Text.unpack . decodeUtf8

-- | Run an `Image` rule
runImage :: BuiltinRun Image SHA256
runImage key old mode = do
    current <- imageSha key
    if mode == RunDependenciesSame && fmap BS.unpack old == Just current
      then return (RunResult ChangedNothing (BS.pack current) (SHA256 $ fromBytes $ BS.pack current))
      else do
      (_, act) <- getUserRuleOne key (const Nothing) $ \(ImageRule k act) -> if k == key then Just act else Nothing
      act  -- run the action to update the target 'key'
      current' <- imageSha key  -- get the value to store for 'key' changed
      return $ RunResult ChangedRecomputeDiff (BS.pack current') (SHA256 $ fromBytes $ BS.pack current')


-- ** Volumes
--
-- Types and rules to handle docker volumes within Shake.

-- | A volume identified by its (local) name and a path it's built from.
newtype Volume = Volume (String, FilePath)
    deriving newtype (Show, Eq, Hashable, Binary, NFData)

type instance RuleResult Volume = ()

data VolumeRule = VolumeRule Volume (Action ())

type VolumeHash = Volume -> Action BS.ByteString

needVolume :: String -> FilePath -> Action ()
needVolume s fp = apply1 $ Volume (s,fp)

volumeRule :: String -> FilePath -> (Volume -> Action ()) -> Rules ()
volumeRule s fp a = addUserRule $ VolumeRule vol (a vol)
  where
    vol = Volume (s, fp)

-- | Run an `Volume` rule
runVolume :: VolumeHash -> BuiltinRun Volume ()
runVolume volumeHash key old mode = do
    current <- volumeHash key
    if mode == RunDependenciesSame && old == Just current
      then return (RunResult ChangedNothing current ())
      else do
      (_, act) <- getUserRuleOne key (const Nothing) $ \(VolumeRule k act) -> if k == key then Just act else Nothing
      act
      current' <- volumeHash key
      return $ RunResult ChangedRecomputeDiff current' ()

-- ** Containers
--
-- Types and rules to handle keeping containers running with Shake

-- | A container identified its name *and* the image it's built from
newtype Container = Container String
    deriving newtype (Show, Eq, Hashable, Binary, NFData)

newtype ContainerId = ContainerId String
    deriving newtype (Show, Eq, Hashable, Binary, NFData, FromJSON, ToJSON)

type instance RuleResult Container = ContainerId

data ContainerRule = ContainerRule Container (Action ())

data ContainerInspect = ContainerInspect
    { containerId    :: ContainerId
    , containerImage :: SHA256
    , running        :: Bool
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

needContainer :: String -> Action ContainerId
needContainer containerName = apply1 $ Container containerName

containerRule :: String -> (Container -> Action ()) -> Rules ()
containerRule containerName act = addUserRule $ ContainerRule container (act container)
  where
    container = Container containerName


-- | Run an `Container` rule
runContainer :: BuiltinRun Container ContainerId
runContainer containerName oldSha mode = do
    current <- findRunningContainer containerName
    if mode == RunDependenciesSame
           && (fromBytes <$> oldSha) == (unSha256 . containerImage <$> current)
           && (running <$> current) == Just True
       -- We don't run the rule if 1/ no dependecny has changed , 2/ image is the same 3/ container is running
      then case current of
             Just ContainerInspect{containerId, containerImage} -> return (RunResult ChangedNothing (toBytes $ unSha256 containerImage) containerId)
             Nothing -> error $ "unexpected state of container " <> show containerName
      else do
      (_, act) <- getUserRuleOne containerName (const Nothing) $ \(ContainerRule k act) -> if k == containerName then Just act else Nothing
      act
      Just ContainerInspect{containerImage, containerId} <- findRunningContainer containerName
      return $ RunResult ChangedRecomputeDiff (toBytes $ unSha256 containerImage) containerId


findRunningContainer :: Container -> Action (Maybe ContainerInspect)
findRunningContainer (Container containerName) = do
  (Exit c, Stdout out) <- cmd "docker" [ "inspect", containerName, "--format", "{\"containerId\": \"{{.Id}}\", \"running\": {{.State.Running}}, \"containerImage\": \"{{.Image}}\"}" ]
  case c of
    ExitSuccess   -> pure $ Aeson.decode out
    ExitFailure _ ->  pure Nothing

-- ** Rules registration

-- | Record how to build `Image`s within Shake's dependency tracking system.
-- This should be called first within the @main@ file's body.
addBuiltinImageRule :: Rules ()
addBuiltinImageRule = addBuiltinRule noLint imageIdentity runImage
  where
    imageIdentity _ (SHA256 v) = Just $ toBytes v

addBuiltinVolumeRule :: VolumeHash -> Rules ()
addBuiltinVolumeRule = addBuiltinRule noLint noIdentity . runVolume


-- | Add rule to "build" containers
-- "Building" containers means actually to have a given container for a given image
-- up and running. The container is "rebuilt" if the image changes or  t is not
-- running
addBuiltinContainerRule :: Rules ()
addBuiltinContainerRule = addBuiltinRule noLint noIdentity runContainer
