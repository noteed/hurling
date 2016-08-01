{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hurling.Handlers where

import Control.Monad (mzero)
import Data.Aeson
  ( fromJSON, object, Value(Object), Result(..), FromJSON(..), ToJSON(..), (.:)
  , (.:?), (.=) )
import Data.ByteString (ByteString)
import Data.List (isPrefixOf, nub)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Process (readProcessWithExitCode)

-- | Handler for the HORDE commands.
handler :: Int -> ByteString -> Value -> IO ()
handler _ _ value = do
  case fromJSON value of
    Error err -> putStrLn err
    Success s -> do
      e <- runContainer "TODO" s
      case e of
        Left err -> putStrLn err
        Right i -> putStrLn i
  hFlush stdout

runContainer :: String -> Service -> IO (Either String String)
runContainer hordeName Service{..} = do
  let hordeDir = "/HORDE" </> hordeName
  (code, out, err) <- readProcessWithExitCode "sudo"
    ([ "docker"
    , "run"
    , "-d"
    ]
    ++ (if servicePrivileged then ["--privileged"] else [])
    ++ (if serviceBindHordeDirectory then ["-v", hordeDir ++ ":" ++ hordeDir] else [])
    ++ (concatMap (\(a, b) -> ["-v", hordeDir </> a ++ ":" ++ b]) serviceBindDirectories)
    ++ [ serviceImage ]
    ++ serviceCommand)
    ""
  case code of
    ExitFailure _ -> return $ Left $ "Error starting service: " ++ out ++ err
    ExitSuccess -> return . Right . head $ words out -- TODO Might explode.

-- | A service describes a particular invokation of a Docker image.
data Service = Service
  { serviceImage :: String
  , serviceCommand :: [String]
  , serviceBindHordeDirectory :: Bool
  -- ^ Bind the horde directory to /HORDE within the container ?
  , serviceBindDirectories :: [(String, String)]
  -- ^ Directories to share with the host.
  , servicePrivileged :: Bool
  -- ^ Whether the service must be run with "--privileged".
  }
  deriving Show

instance FromJSON Service where
  parseJSON (Object v) = do
    image <- v .: "image"
    command <- v .: "command"
    return defaultService
      { serviceImage = image
      , serviceCommand = command
      }
  parseJSON _ = mzero

defaultService :: Service
defaultService = Service
  { serviceImage = "base"
  , serviceCommand = []
  , serviceBindHordeDirectory = False
  , serviceBindDirectories = []
  , servicePrivileged = False
  }

-- | Handler for the GitHub Hooks `push` events.
githubHooksHandler :: Int -> ByteString -> Value -> IO ()
githubHooksHandler _ "push" value = do
  case fromJSON value of
    Error err -> putStrLn err
    Success s -> do
      e <- runBuildContainer s
      case e of
        Left err -> putStrLn err
        Right i -> putStrLn i
  hFlush stdout

githubHooksHandler _ _ _ = do
  putStrLn "GitHub Hooks handler: not a `push` event."
  hFlush stdout

runBuildContainer :: Push -> IO (Either String String)
runBuildContainer Push{..} = do
  if "refs/heads/" `isPrefixOf` pushRef
    then do
      (code, out, err) <- readProcessWithExitCode "sudo"
        ([ "docker"
        , "run"
        , "-d"
        , "-v", "/var/run/docker.sock:/var/run/docker.sock"
        , "-v", "/secrets/github-deploy-ssh:/home/worker/.ssh"
        , "images.reesd.com/reesd/builder"
        , "/home/worker/checkout-and-build.sh"
        , repoSshUrl pushRepository
        , repoName pushRepository ++ ".git"
        , drop (length ("refs/heads/" :: String)) pushRef
        ])
        ""
      case code of
        ExitFailure _ -> return $ Left $ "Error starting service: " ++ out ++ err
        ExitSuccess -> return . Right . head $ words out -- TODO Might explode.

    else return $ Left $ "Can't parse ref " ++ pushRef ++ "."

-- | Represent a (simplified) GitHub Hooks `push` event.
-- In practice, this it can be considered as a `docker build` command.
data Push = Push
  { pushRef :: String
  -- ^ E.g. "refs/heads/master" for the branch "master".
  , pushRepository :: Repository
  , pushCommits :: [Commit]
  }
  deriving Show

data Repository = Repository
  { repoName :: String
  , repoFullName :: String
  , repoSshUrl :: String
  }
  deriving Show

data Commit = Commit
  { commitId :: String
  , commitAdded :: [String]
  , commitRemoved :: [String]
  , commitModified :: [String]
  }
  deriving Show

instance FromJSON Push where
  parseJSON (Object v) = do
    ref <- v .: "ref"
    repo <- v .: "repository"
    commits <- v .: "commits"
    return Push
      { pushRef = ref
      , pushRepository = repo
      , pushCommits = commits
      }
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Object v) = do
    name' <- v .: "name"
    fullname <- v .: "full_name"
    sshUrl <- v .: "ssh_url"
    return Repository
      { repoName = name'
      , repoFullName = fullname
      , repoSshUrl = sshUrl
      }
  parseJSON _ = mzero

instance FromJSON Commit where
  parseJSON (Object v) = do
    id' <- v .: "id"
    added <- v .: "added"
    removed <- v .: "removed"
    modified <- v .: "modified"
    return Commit
      { commitId = id'
      , commitAdded = added
      , commitRemoved = removed
      , commitModified = modified
      }
  parseJSON _ = mzero

instance ToJSON Push where
  toJSON Push{..} = object
    [ "ref" .= pushRef
    , "repository" .= toJSON pushRepository
    , "commits" .= toJSON pushCommits
    ]

instance ToJSON Repository where
  toJSON Repository{..} = object
    [ "name" .= repoName
    , "full_name" .= repoFullName
    , "ssh_url" .= repoSshUrl
    ]

instance ToJSON Commit where
  toJSON Commit{..} = object
    [ "id" .= commitId
    , "added" .= commitAdded
    , "removed" .= commitRemoved
    , "modified" .= commitModified
    ]

-- | Return all files added, removed or modified in this push.
pushTouchedFiles :: Push -> [String]
pushTouchedFiles Push{..} = nub (concatMap (flip concatMap pushCommits) [commitAdded, commitRemoved, commitModified])

-- | Represent a `docker run` command.
data Run = Run
  { runImage :: String
  , runCommand :: [String]
  , runBindDaemonSocket :: Bool
  -- ^ Expose the /var/run/docker.sock to the container ?
  , runBindDirectories :: [(String, String)]
  -- ^ Directories to share with the host.
  , runPrivileged :: Bool
  -- ^ Whether the run uses "--privileged".
  , runTryTag :: Maybe String
  -- ^ Try using that tag before fallbacking to `runImage`.
  }
  deriving Show

instance FromJSON Run where
  parseJSON (Object v) = do
    image <- v .: "image"
    command <- v .: "command"
    daemonSocket <- v .:? "bind-daemon-socket"
    bindDirectories <- v .:? "bind-directories"
    privileged <- v.:? "privileged"
    tryTag <- v.:? "try-tag"
    return Run
      { runImage = image
      , runCommand = command
      , runBindDaemonSocket = maybe False id daemonSocket
      , runBindDirectories = maybe [] id bindDirectories
      , runPrivileged = maybe False id privileged
      , runTryTag = tryTag
      }
  parseJSON _ = mzero

instance ToJSON Run where
  toJSON Run{..} = object $
    [ "image" .= runImage
    , "command" .= runCommand
    , "bind-daemon-socket" .= runBindDaemonSocket
    , "bind-directories" .= runBindDirectories
    , "privileged" .= runPrivileged
    ]
    ++ (maybe [] (\t -> ["try-tag" .= t]) runTryTag)
