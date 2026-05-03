{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module PMS.Infra.Agent.Process.DS.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Reader
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import qualified Data.Text as T
import Control.Monad.Except
import Control.Monad (when)
import qualified Control.Exception.Safe as E
import System.Exit
import qualified Data.ByteString as BS
import Data.Aeson
import qualified System.Process as S
import qualified Data.Map.Strict as Map

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM

import PMS.Infra.Agent.Process.DM.Type
import qualified PMS.Infra.Agent.Process.DM.Constant as DM_CONST
import PMS.Infra.Agent.Process.DS.Utility


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| cmd2task .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.AgentProcessCommand AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.AgentProcessCommand
    go = do
      queue <- view DM.agentProcessQueueDomainData <$> lift ask
      dat <- liftIO $ STM.atomically $ STM.readTQueue queue
      return dat

---------------------------------------------------------------------------------
-- |
--
cmd2task :: ConduitT DM.AgentProcessCommand (IOTask ()) AppContext ()
cmd2task = await >>= \case
  Just cmd -> flip catchError (errHdl cmd) $ do
    lift (go cmd) >>= yield >> cmd2task
  Nothing -> do
    $logWarnS DM._LOGTAG "cmd2task: await returns nothing. skip."
    cmd2task

  where
    errHdl :: DM.AgentProcessCommand -> String -> ConduitT DM.AgentProcessCommand (IOTask ()) AppContext ()
    errHdl procCmd msg = do
      let jsonrpc = DM.getJsonRpcAgentProcessCommand procCmd
      $logWarnS DM._LOGTAG $ T.pack $ "cmd2task: exception occurred. skip. " ++ msg
      lift $ errorToolsCallResponse jsonrpc $ "cmd2task: exception occurred. skip. " ++ msg
      cmd2task

    go :: DM.AgentProcessCommand -> AppContext (IOTask ())
    go (DM.AgentProcessEchoCommand      dat) = genEchoTask dat
    go (DM.AgentProcessRunCommand       dat) = genProcRunTask dat
    go (DM.AgentProcessReadCommand      dat) = genProcReadTask dat
    go (DM.AgentProcessWriteCommand     dat) = genProcWriteTask dat
    go (DM.AgentProcessTerminateCommand dat) = genProcTerminateTask dat

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IOTask ()) Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT (IOTask ()) Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink

    go :: (IO ()) -> AppContext ()
    go task = do
      $logDebugS DM._LOGTAG "sink: start async."
      _ <- liftIOE $ async task
      $logDebugS DM._LOGTAG "sink: end async."
      return ()


---------------------------------------------------------------------------------
-- |
--
genEchoTask :: DM.AgentProcessEchoCommandData -> AppContext (IOTask ())
genEchoTask dat = do
  resQ <- view DM.responseQueueDomainData <$> lift ask
  let val = dat^.DM.valueAgentProcessEchoCommandData

  $logDebugS DM._LOGTAG $ T.pack $ "echoTask: echo : " ++ val
  return $ echoTask resQ dat val


-- |
--
echoTask :: STM.TQueue DM.McpResponse -> DM.AgentProcessEchoCommandData -> String -> IOTask ()
echoTask resQ cmdDat val = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Agent.Process.DS.Core.echoTask run. " ++ val
  toolsCallResponse resQ (cmdDat^.DM.jsonrpcAgentProcessEchoCommandData) ExitSuccess val ""
  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.echoTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ (cmdDat^.DM.jsonrpcAgentProcessEchoCommandData) (ExitFailure 1) "" (show e)


---------------------------------------------------------------------------------
-- |
--
genProcRunTask :: DM.AgentProcessRunCommandData -> AppContext (IOTask ())
genProcRunTask cmdDat = do
  let argsBS = DM.unRawJsonByteString $ cmdDat^.DM.argumentsAgentProcessRunCommandData
  resQ      <- view DM.responseQueueDomainData <$> lift ask
  procTMVar <- view processAppData <$> ask
  argsDat   <- liftEither $ eitherDecode argsBS
  let cmd       = argsDat^.commandProcRunToolParams
      argsArray = maybe [] id (argsDat^.argumentsProcRunToolParams)
      envMap    = argsDat^.environmentProcRunToolParams
      addEnv    = maybe [] Map.toList envMap
  -- whitelist check: deny all if list is empty or cmd not in list
  allowedCmds <- view DM.allowedAgentCmdsDomainData <$> lift ask
  when (null allowedCmds || cmd `notElem` allowedCmds) $
    throwError $ "command not allowed: " ++ cmd
  return $ procRunTask cmdDat resQ procTMVar cmd argsArray addEnv


-- |
--
procRunTask :: DM.AgentProcessRunCommandData
            -> STM.TQueue DM.McpResponse
            -> STM.TMVar (Maybe ProcData)
            -> String
            -> [String]
            -> [(String, String)]
            -> IOTask ()
procRunTask cmdDat resQ procTMVar cmd argsArray addEnv = flip E.catchAny errHdl $ do
  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.procRunTask run."
  STM.atomically (STM.takeTMVar procTMVar) >>= \case
    Just p -> do
      STM.atomically $ STM.putTMVar procTMVar (Just p)
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Process.DS.Core.procRunTask: process is already running."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "process is already running."
    Nothing -> do
      runProc procTMVar cmd argsArray addEnv
      let payload = "command: " ++ truncate10 cmd
                 ++ ", arguments: " ++ truncate10 (show argsArray)
                 ++ ", environment: " ++ truncate10 (show addEnv)
      toolsCallResponse resQ jsonRpc ExitSuccess payload ""
  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.procRunTask end."
  where
    jsonRpc = cmdDat^.DM.jsonrpcAgentProcessRunCommandData
    errHdl :: E.SomeException -> IO ()
    errHdl e = do
      _ <- STM.atomically $ STM.tryTakeTMVar procTMVar
      STM.atomically $ STM.putTMVar procTMVar Nothing
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)

    -- |
    -- Truncate a string to at most 10 characters, appending "..." if truncated.
    truncate10 :: String -> String
    truncate10 s
      | length s <= 10 = s
      | otherwise      = take 10 s ++ "..."


---------------------------------------------------------------------------------
-- |
-- Generate a task to terminate the running process.
genProcTerminateTask :: DM.AgentProcessTerminateCommandData -> AppContext (IOTask ())
genProcTerminateTask dat = do
  $logDebugS DM._LOGTAG $ T.pack $ "genProcTerminateTask called. "
  procTMVar <- view processAppData <$> ask
  resQ <- view DM.responseQueueDomainData <$> lift ask
  return $ procTerminateTask dat resQ procTMVar

-- |
-- Terminate the running process and reset the TMVar to Nothing.
-- Always returns ExitSuccess if terminateProcess completes without exception.
-- The exit code from waitForProcess is recorded in the log only.
procTerminateTask :: DM.AgentProcessTerminateCommandData
                  -> STM.TQueue DM.McpResponse
                  -> STM.TMVar (Maybe ProcData)
                  -> IOTask ()
procTerminateTask cmdDat resQ procTMVar = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Agent.Process.DS.Core.procTerminateTask run. "
  let jsonRpc = cmdDat^.DM.jsonrpcAgentProcessTerminateCommandData

  STM.atomically (STM.swapTMVar procTMVar Nothing) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Process.DS.Core.procTerminateTask: process is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "process is not started."
    Just procDat -> do
      let pHdl = procDat^.pHdlProcData
      S.terminateProcess pHdl
      exitCode <- S.waitForProcess pHdl
      hPutStrLn stderr $ "[INFO] PMS.Infra.Agent.Process.DS.Core.procTerminateTask closeProc : " ++ show exitCode
      toolsCallResponse resQ jsonRpc ExitSuccess "" "process is terminated."

  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.procTerminateTask end."

  where
    errHdl :: E.SomeException -> IO ()
    errHdl e = toolsCallResponse resQ (cmdDat^.DM.jsonrpcAgentProcessTerminateCommandData) (ExitFailure 1) "" (show e)


---------------------------------------------------------------------------------
-- |
-- Use DM_CONST._PROC_READ_WAIT_MSEC (100msec) as the timeout value.
-- External config (timeoutMicrosecDomainData) is not used.
genProcReadTask :: DM.AgentProcessReadCommandData -> AppContext (IOTask ())
genProcReadTask cmdData = do
  resQ      <- view DM.responseQueueDomainData <$> lift ask
  procTMVar <- view processAppData <$> ask
  let argsBS     = DM.unRawJsonByteString $ cmdData^.DM.argumentsAgentProcessReadCommandData
  argsDat <- liftEither $ eitherDecode $ argsBS
  let size       = argsDat^.argumentsProcIntToolParams
      actualSize = min size DM_CONST._READ_BUFFER_SIZE
  return $ procReadTask cmdData resQ procTMVar DM_CONST._PROC_READ_WAIT_MSEC actualSize

-- |
--
procReadTask :: DM.AgentProcessReadCommandData
             -> STM.TQueue DM.McpResponse
             -> STM.TMVar (Maybe ProcData)
             -> Int
             -> Int
             -> IOTask ()
procReadTask cmdDat resQ procTMVar tout actualSize = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Agent.Process.DS.Core.procReadTask run. actualSize: " ++ show actualSize

  STM.atomically (STM.readTMVar procTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Process.DS.Core.procReadTask: process is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "process is not started."
    Just p -> do
      output <- readProc p tout actualSize
      let result = bs2strUTF8 output
      toolsCallResponse resQ jsonRpc ExitSuccess result ""

  where
    jsonRpc = cmdDat^.DM.jsonrpcAgentProcessReadCommandData
    errHdl e = toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)


---------------------------------------------------------------------------------
-- |
--
genProcWriteTask :: DM.AgentProcessWriteCommandData -> AppContext (IOTask ())
genProcWriteTask cmdData = do
  let argsBS = DM.unRawJsonByteString $ cmdData^.DM.argumentsAgentProcessWriteCommandData
  resQ <- view DM.responseQueueDomainData <$> lift ask
  procTMVar <- view processAppData <$> ask
  argsDat <- liftEither $ eitherDecode $ argsBS
  let args = argsDat^.argumentsProcStringToolParams
  return $ procWriteTask cmdData resQ procTMVar args

-- |
--
procWriteTask :: DM.AgentProcessWriteCommandData
              -> STM.TQueue DM.McpResponse
              -> STM.TMVar (Maybe ProcData)
              -> String
              -> IOTask ()
procWriteTask cmdDat resQ procTMVar args = flip E.catchAny errHdl $ do
  hPutStrLn stderr $ "[INFO] PMS.Infra.Agent.Process.DS.Core.procWriteTask run. " ++ args

  STM.atomically (STM.readTMVar procTMVar) >>= \case
    Nothing -> do
      hPutStrLn stderr "[ERROR] PMS.Infra.Agent.Process.DS.Core.procWriteTask: process is not started."
      toolsCallResponse resQ jsonRpc (ExitFailure 1) "" "process is not started."
    Just p -> do
      let wHdl = p^.wHdLProcData
          cmd = str2bsUTF8 $ appendLF args
      BS.hPut wHdl cmd
      hFlush wHdl
      toolsCallResponse resQ jsonRpc ExitSuccess "success" ""

  where
    jsonRpc = cmdDat^.DM.jsonrpcAgentProcessWriteCommandData
    errHdl e = toolsCallResponse resQ jsonRpc (ExitFailure 1) "" (show e)

