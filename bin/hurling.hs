{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Database.PostgreSQL.Simple
import Paths_hurling (version)
import System.Console.CmdArgs.Implicit

import qualified Database.PostgreSQL.Queue as Q

import Hurling.Handlers (handler, githubHooksHandler)

----------------------------------------------------------------------
-- Command-line
----------------------------------------------------------------------

main :: IO ()
main = (runCmd =<<) $ cmdArgs $
  modes
    [ cmdWork
    , cmdGitHubHooks
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
  | GitHubHooks
    { cmdDatabaseUrl :: String
    }
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

-- | Create a 'GitHubHooks' command.
cmdGitHubHooks :: Cmd
cmdGitHubHooks = GitHubHooks
  { cmdDatabaseUrl = def
    &= explicit
    &= name "database-url"
    &= help "Database URL."
  } &= help "Process GitHub Hooks `push` event."
    &= explicit
    &= name "ghhooks"

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

runCmd GitHubHooks{..} = do
  con <- connectPostgreSQL $ pack cmdDatabaseUrl
  _ <- execute_ con "SET application_name='hurling'"
  w_ <- Q.defaultWorker
  let w = w_
        { Q.workerQueue = Q.Queue "ghhooks"
        , Q.workerHandler = githubHooksHandler
        }
  Q.start con w
  close con
