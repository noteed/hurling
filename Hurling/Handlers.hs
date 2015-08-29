{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Hurling.Handlers where

import Control.Monad (mzero)
import Data.Aeson (fromJSON, Value(Object), Result(..), FromJSON(..), (.:))
import Data.ByteString (ByteString)
import Data.List (isPrefixOf)
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
data Push = Push
  { pushRef :: String
  -- ^ E.g. "refs/heads/master" for the branch "master".
  , pushRepository :: Repository
  }
  deriving Show

data Repository = Repository
  { repoName :: String
  , repoSshUrl :: String
  }
  deriving Show

instance FromJSON Push where
  parseJSON (Object v) = do
    ref <- v .: "ref"
    repo <- v .: "repository"
    return Push
      { pushRef = ref
      , pushRepository = repo
      }
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Object v) = do
    name' <- v .: "name"
    sshUrl <- v .: "ssh_url"
    return Repository
      { repoName = name'
      , repoSshUrl = sshUrl
      }
  parseJSON _ = mzero
