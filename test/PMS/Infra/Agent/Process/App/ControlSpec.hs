{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module PMS.Infra.Agent.Process.App.ControlSpec (spec) where

import Test.Hspec
import Control.Concurrent
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Lens
import Data.Default
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Infra.Agent.Process.App.Control as SUT
import qualified PMS.Infra.Agent.Process.DM.Type as SUT

-- |
--
data SpecContext = SpecContext {
                   _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext    :: SUT.AppData
                 , _threadIdSpecContext   :: Maybe (Async ())
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  appDat <- SUT.defaultAppData
  return SpecContext {
           _domainDataSpecContext = domDat
         , _appDataSpecContext    = appDat
         , _threadIdSpecContext   = Nothing
         }

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $
    afterAll tearDownOnce .
      beforeWith setUp .
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] EXECUTED ONLY ONCE BEFORE ALL TESTS START."
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] EXECUTED ONLY ONCE AFTER ALL TESTS FINISH."

-- |
-- Each test gets a fresh domDat/appDat and a running SUT thread.
-- cmd.exe is added to the whitelist so that TC-02 / TC-04 keep working.
--
setUp :: SpecContext -> IO SpecContext
setUp ctx = do
  putStrLn "[INFO] EXECUTED BEFORE EACH TEST STARTS."
  domDat <- DM.defaultDomainData
  let domDat' = domDat { DM._allowedAgentCmdsDomainData = ["cmd.exe"] }
  appDat <- SUT.defaultAppData
  thId   <- async $ SUT.runWithAppData appDat domDat'
  return ctx {
               _domainDataSpecContext = domDat'
             , _appDataSpecContext    = appDat
             , _threadIdSpecContext   = Just thId
             }

-- |
-- Build a SpecContext with a custom whitelist and start the SUT thread.
--
setUpWithAllowList :: SpecContext -> [String] -> IO SpecContext
setUpWithAllowList ctx allowList = do
  domDat <- DM.defaultDomainData
  let domDat' = domDat { DM._allowedAgentCmdsDomainData = allowList }
  appDat <- SUT.defaultAppData
  thId   <- async $ SUT.runWithAppData appDat domDat'
  return ctx {
               _domainDataSpecContext = domDat'
             , _appDataSpecContext    = appDat
             , _threadIdSpecContext   = Just thId
             }

-- |
-- Cancel the SUT thread after each test.
--
tearDown :: SpecContext -> IO ()
tearDown ctx = do
  putStrLn "[INFO] EXECUTED AFTER EACH TEST FINISHES."
  case ctx^.threadIdSpecContext of
    Nothing  -> return ()
    Just thId -> cancel thId

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- |
-- Send a command to the SUT and receive one response.
--
sendAndReceive :: DM.DomainData
               -> DM.AgentProcessCommand
               -> IO DM.McpToolsCallResponseData
sendAndReceive domDat cmd = do
  let cmdQ = domDat^.DM.agentProcessQueueDomainData
      resQ = domDat^.DM.responseQueueDomainData
  STM.atomically $ STM.writeTQueue cmdQ cmd
  (DM.McpToolsCallResponse dat) <- STM.atomically $ STM.readTQueue resQ
  return dat

-- |
-- Build a minimal JsonRpcRequest for testing.
--
mkJsonRpc :: String -> DM.JsonRpcRequest
mkJsonRpc method = def { DM._methodJsonRpcRequest = method }

-- |
-- Encode ProcRunToolParams to RawJsonByteString.
--
mkRunArgs :: String -> [String] -> DM.RawJsonByteString
mkRunArgs cmd args =
  DM.RawJsonByteString $ encode $ SUT.ProcRunToolParams
    { SUT._commandProcRunToolParams     = cmd
    , SUT._argumentsProcRunToolParams   = Just args
    , SUT._environmentProcRunToolParams = Nothing
    }

-- |
-- Encode ProcIntToolParams (read size) to RawJsonByteString.
--
mkReadArgs :: Int -> DM.RawJsonByteString
mkReadArgs size =
  DM.RawJsonByteString $ encode $ SUT.ProcIntToolParams
    { SUT._argumentsProcIntToolParams = size }

-- |
-- Encode ProcStringToolParams (write payload) to RawJsonByteString.
--
mkWriteArgs :: String -> DM.RawJsonByteString
mkWriteArgs s =
  DM.RawJsonByteString $ encode $ SUT.ProcStringToolParams
    { SUT._argumentsProcStringToolParams = s }

-- |
-- Extract the isError flag from a response.
--
isError :: DM.McpToolsCallResponseData -> Bool
isError dat =
  dat^.DM.resultMcpToolsCallResponseData^.DM.isErrorMcpToolsCallResponseResult

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

-- |
--
run :: SpecWith SpecContext
run = do

  -- TC-01: echo command smoke test
  describe "TC-01: echo command" $ do
    context "when AgentProcessEchoCommand is sent" $ do
      it "should return the same value with isError=False" $ \ctx -> do
        putStrLn "[INFO] TC-01 start."
        let domDat  = ctx^.domainDataSpecContext
            expect  = "hello-echo"
            echoCmd = DM.AgentProcessEchoCommand $ DM.AgentProcessEchoCommandData
                        { DM._jsonrpcAgentProcessEchoCommandData = mkJsonRpc "echo"
                        , DM._valueAgentProcessEchoCommandData   = expect
                        }
        dat <- sendAndReceive domDat echoCmd
        isError dat `shouldBe` False
        let contents = dat^.DM.resultMcpToolsCallResponseData^.DM.contentMcpToolsCallResponseResult
            actual   = DM._textMcpToolsCallResponseResultContent (head contents)
        actual `shouldBe` expect

  -- TC-02: run -> read -> write -> read -> terminate smoke test
  describe "TC-02: run -> read -> write -> read -> terminate" $ do
    context "when cmd.exe is started and interacted with" $ do
      it "should succeed for each step with isError=False" $ \ctx -> do
        putStrLn "[INFO] TC-02 start."
        let domDat = ctx^.domainDataSpecContext

        -- run
        let runCmd = DM.AgentProcessRunCommand $ DM.AgentProcessRunCommandData
                       { DM._jsonrpcAgentProcessRunCommandData   = mkJsonRpc "agent-proc-run"
                       , DM._nameAgentProcessRunCommandData      = "agent-proc-run"
                       , DM._argumentsAgentProcessRunCommandData = mkRunArgs "cmd.exe" []
                       }
        runDat <- sendAndReceive domDat runCmd
        isError runDat `shouldBe` False

        -- read (discard the initial prompt output after cmd.exe starts)
        threadDelay (500 * 1000)  -- wait 500ms for the process to start
        let readCmd = DM.AgentProcessReadCommand $ DM.AgentProcessReadCommandData
                        { DM._jsonrpcAgentProcessReadCommandData   = mkJsonRpc "agent-proc-read"
                        , DM._argumentsAgentProcessReadCommandData = mkReadArgs 4096
                        }
        readDat1 <- sendAndReceive domDat readCmd
        isError readDat1 `shouldBe` False

        -- write: send "echo hello\n"
        let writeCmd = DM.AgentProcessWriteCommand $ DM.AgentProcessWriteCommandData
                         { DM._jsonrpcAgentProcessWriteCommandData   = mkJsonRpc "agent-proc-write"
                         , DM._argumentsAgentProcessWriteCommandData = mkWriteArgs "echo hello"
                         }
        writeDat <- sendAndReceive domDat writeCmd
        isError writeDat `shouldBe` False

        -- read (read the echo output)
        threadDelay (500 * 1000)  -- wait 500ms for the output to be available
        readDat2 <- sendAndReceive domDat readCmd
        isError readDat2 `shouldBe` False

        -- terminate
        let termCmd = DM.AgentProcessTerminateCommand $ DM.AgentProcessTerminateCommandData
                        { DM._jsonrpcAgentProcessTerminateCommandData = mkJsonRpc "agent-proc-terminate"
                        }
        termDat <- sendAndReceive domDat termCmd
        isError termDat `shouldBe` False

  -- TC-03: terminate without prior run
  describe "TC-03: terminate without prior run" $ do
    context "when terminate is sent before run" $ do
      it "should return isError=True" $ \ctx -> do
        putStrLn "[INFO] TC-03 start."
        let domDat = ctx^.domainDataSpecContext
            termCmd = DM.AgentProcessTerminateCommand $ DM.AgentProcessTerminateCommandData
                        { DM._jsonrpcAgentProcessTerminateCommandData = mkJsonRpc "agent-proc-terminate"
                        }
        dat <- sendAndReceive domDat termCmd
        isError dat `shouldBe` True

  -- TC-04: double run (process already running)
  describe "TC-04: double run" $ do
    context "when run is sent twice" $ do
      it "should return isError=True for the second run" $ \ctx -> do
        putStrLn "[INFO] TC-04 start."
        let domDat = ctx^.domainDataSpecContext
            runCmd = DM.AgentProcessRunCommand $ DM.AgentProcessRunCommandData
                       { DM._jsonrpcAgentProcessRunCommandData   = mkJsonRpc "agent-proc-run"
                       , DM._nameAgentProcessRunCommandData      = "agent-proc-run"
                       , DM._argumentsAgentProcessRunCommandData = mkRunArgs "cmd.exe" []
                       }

        -- first run: should succeed
        runDat1 <- sendAndReceive domDat runCmd
        isError runDat1 `shouldBe` False

        -- second run: should fail with error
        runDat2 <- sendAndReceive domDat runCmd
        isError runDat2 `shouldBe` True

        -- cleanup: terminate
        let termCmd = DM.AgentProcessTerminateCommand $ DM.AgentProcessTerminateCommandData
                        { DM._jsonrpcAgentProcessTerminateCommandData = mkJsonRpc "agent-proc-terminate"
                        }
        _ <- sendAndReceive domDat termCmd
        return ()

  -- TC-05: Whitelist - allowed command succeeds
  describe "TC-05: whitelist - allowed command" $ do
    context "when cmd.exe is in agentAllowedCmds" $ do
      it "should start the process and return isError=False" $ \ctx -> do
        putStrLn "[INFO] TC-05 start."
        ctx' <- setUpWithAllowList ctx ["cmd.exe"]
        let domDat = ctx'^.domainDataSpecContext
            runCmd = DM.AgentProcessRunCommand $ DM.AgentProcessRunCommandData
                       { DM._jsonrpcAgentProcessRunCommandData   = mkJsonRpc "agent-proc-run"
                       , DM._nameAgentProcessRunCommandData      = "agent-proc-run"
                       , DM._argumentsAgentProcessRunCommandData = mkRunArgs "cmd.exe" []
                       }
        runDat <- sendAndReceive domDat runCmd
        isError runDat `shouldBe` False
        -- cleanup
        let termCmd = DM.AgentProcessTerminateCommand $ DM.AgentProcessTerminateCommandData
                        { DM._jsonrpcAgentProcessTerminateCommandData = mkJsonRpc "agent-proc-terminate"
                        }
        _ <- sendAndReceive domDat termCmd
        tearDown ctx'

  -- TC-06: Whitelist - denied command returns error message
  describe "TC-06: whitelist - denied command" $ do
    context "when powershell.exe is NOT in agentAllowedCmds" $ do
      it "should return isError=True with 'command not allowed' message" $ \ctx -> do
        putStrLn "[INFO] TC-06 start."
        ctx' <- setUpWithAllowList ctx ["cmd.exe"]
        let domDat = ctx'^.domainDataSpecContext
            runCmd = DM.AgentProcessRunCommand $ DM.AgentProcessRunCommandData
                       { DM._jsonrpcAgentProcessRunCommandData   = mkJsonRpc "agent-proc-run"
                       , DM._nameAgentProcessRunCommandData      = "agent-proc-run"
                       , DM._argumentsAgentProcessRunCommandData = mkRunArgs "powershell.exe" []
                       }
        runDat <- sendAndReceive domDat runCmd
        isError runDat `shouldBe` True
        let contents = runDat^.DM.resultMcpToolsCallResponseData^.DM.contentMcpToolsCallResponseResult
            errText  = DM._textMcpToolsCallResponseResultContent (head contents)
        errText `shouldBe` "cmd2task: exception occurred. skip. command not allowed: powershell.exe"
        tearDown ctx'

  -- TC-07: Whitelist - empty list denies all commands
  describe "TC-07: whitelist - empty list denies all" $ do
    context "when agentAllowedCmds is empty" $ do
      it "should return isError=True for any command" $ \ctx -> do
        putStrLn "[INFO] TC-07 start."
        ctx' <- setUpWithAllowList ctx []
        let domDat = ctx'^.domainDataSpecContext
            runCmd = DM.AgentProcessRunCommand $ DM.AgentProcessRunCommandData
                       { DM._jsonrpcAgentProcessRunCommandData   = mkJsonRpc "agent-proc-run"
                       , DM._nameAgentProcessRunCommandData      = "agent-proc-run"
                       , DM._argumentsAgentProcessRunCommandData = mkRunArgs "cmd.exe" []
                       }
        runDat <- sendAndReceive domDat runCmd
        isError runDat `shouldBe` True
        let contents = runDat^.DM.resultMcpToolsCallResponseData^.DM.contentMcpToolsCallResponseResult
            errText  = DM._textMcpToolsCallResponseResultContent (head contents)
        errText `shouldBe` "cmd2task: exception occurred. skip. command not allowed: cmd.exe"
        tearDown ctx'
