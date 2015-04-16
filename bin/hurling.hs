{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Control.Monad (mzero)
import Data.Aeson (fromJSON, Value(Object), Result(..), FromJSON(..), (.:))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Database.PostgreSQL.Simple
import Paths_hurling (version)
import System.Console.CmdArgs.Implicit
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)

import qualified Database.PostgreSQL.Queue as Q

----------------------------------------------------------------------
-- Command-line
----------------------------------------------------------------------

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdWork
    , cmdDummy
    ]
  &= summary versionString
  &= program "hurling"

-- | String with the program name, version and copyright.
versionString :: String
versionString =
  "hurling " ++ showVersion version ++ " - Copyright (c) 2015 Vo Minh Thu."

-- | Data type representing the different command-line subcommands.
data Cmd =
    Work
    { cmdDatabaseUrl :: String
    , cmdQueueName :: String
    , cmdWorkOnce :: Bool
    }
  | Dummy
  deriving (Data, Typeable)

-- | Create a 'Work' command.
cmdWork :: Cmd
cmdWork = Work
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  , cmdQueueName = "hurling"
    &= explicit
    &= name "queue"
    &= help "Queue name."
  , cmdWorkOnce = def
    &= explicit
    &= name "once"
    &= help "Process a single job then exit."
  } &= help "A dummy worker; it prints to stdout the job method and arguments."
    &= explicit
    &= name "work"

-- | Create a 'Dummy' command.
cmdDummy :: Cmd
cmdDummy = Dummy
    &= help "Dummy command."
    &= explicit
    &= name "dummy"

-- | Run a sub-command.
runCmd :: Cmd -> IO ()
runCmd Work{..} = do
  con <- connectPostgreSQL $ pack cmdDatabaseUrl
  _ <- execute_ con "SET application_name='hurling'"
  w_ <- Q.defaultWorker
  let w = w_
        { Q.workerQueue = Q.Queue (pack cmdQueueName)
        , Q.workerHandler = handler
        }
  if cmdWorkOnce
    then Q.work con w
    else Q.start con w
  close con

runCmd Dummy{..} = putStrLn "Hurr hurr."

----------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------

handler :: ByteString -> Value -> IO ()
handler _ value =
  case fromJSON value of
    Error err -> putStrLn err
    Success s -> do
      e <- runContainer "TODO" s
      case e of
        Left err -> putStrLn err
        Right i -> putStrLn i

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

type Hostname = String

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
