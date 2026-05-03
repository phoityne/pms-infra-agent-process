{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module PMS.Infra.Agent.Process.DS.Utility where

import System.IO
import Control.Lens
import System.Exit
import System.Log.FastLogger
import qualified Control.Exception.Safe as E
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified System.Process as S
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.String.AnsiEscapeCodes.Strip.Text as ANSI
import qualified System.Environment as Env

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.Constant as DM
import qualified PMS.Domain.Model.DS.Utility as DM
import PMS.Infra.Agent.Process.DM.Type

-- |
--
runApp :: DM.DomainData -> AppData -> TimedFastLogger -> AppContext a -> IO (Either DM.ErrorData a)
runApp domDat appDat logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runReaderT ctx appDat


-- |
--
liftIOE :: IO a -> AppContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show

---------------------------------------------------------------------------------
-- |
--
toolsCallResponse :: STM.TQueue DM.McpResponse
                  -> DM.JsonRpcRequest
                  -> ExitCode
                  -> String
                  -> String
                  -> IO ()
toolsCallResponse resQ jsonRpc code outStr errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" outStr
                , DM.McpToolsCallResponseResultContent "text" errStr
                ]
      result = DM.McpToolsCallResponseResult {
                  DM._contentMcpToolsCallResponseResult = content
                , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                }
      resDat = DM.McpToolsCallResponseData jsonRpc result
      res = DM.McpToolsCallResponse resDat

  STM.atomically $ STM.writeTQueue resQ res

-- |
--
errorToolsCallResponse :: DM.JsonRpcRequest -> String -> AppContext ()
errorToolsCallResponse jsonRpc errStr = do
  let content = [ DM.McpToolsCallResponseResultContent "text" errStr ]
      result = DM.McpToolsCallResponseResult {
                  DM._contentMcpToolsCallResponseResult = content
                , DM._isErrorMcpToolsCallResponseResult = True
                }
      resDat = DM.McpToolsCallResponseData jsonRpc result
      res = DM.McpToolsCallResponse resDat

  resQ <- view DM.responseQueueDomainData <$> lift ask
  liftIOE $ STM.atomically $ STM.writeTQueue resQ res

-- |
--
runProc :: STM.TMVar (Maybe ProcData) -> String -> [String] -> [(String, String)] -> IO ()
runProc procVar cmd args addEnv = do

  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.procRunTask.runProc start."

  (fromPtyHandle, toProcHandle) <- S.createPipe
  (fromProcHandle, toPtyHandle) <- S.createPipe
  -- (fromProcEHandle, toPtyEHandle) <- S.createPipe

  hSetBuffering toProcHandle   NoBuffering
  hSetBuffering fromProcHandle NoBuffering
  hSetBuffering fromPtyHandle  NoBuffering
  hSetBuffering toPtyHandle    NoBuffering

  baseEnv <- Env.getEnvironment
  let cwd = Nothing
      runEnvs = Just $ addEnv ++ baseEnv

  hPutStrLn stderr $ "[INFO] env = " ++ show runEnvs
  hPutStrLn stderr $ "[INFO] cmd = " ++ cmd
  hPutStrLn stderr $ "[INFO] args = " ++ show args

  pHdl <- S.runProcess cmd args cwd runEnvs (Just fromPtyHandle) (Just toPtyHandle) (Just toPtyHandle)
  -- pHdl <- S.runProcess cmd args cwd runEnvs (Just fromPtyHandle) (Just toPtyHandle) (Just toPtyEHandle)
  let procData = ProcData {
                  _wHdLProcData = toProcHandle
                , _rHdlProcData = fromProcHandle
                , _eHdlProcData = fromProcHandle
--                , _eHdlProcData = fromProcEHandle
                , _pHdlProcData = pHdl
                }

  STM.atomically $ STM.putTMVar procVar (Just procData)

  hPutStrLn stderr "[INFO] PMS.Infra.Agent.Process.DS.Core.procRunTask.runProc end."

-- |
-- If no data is available (timeout), return an empty ByteString instead of an error.
readProc :: ProcData -> Int -> Int -> IO BS.ByteString
readProc dat tout size = do
  let hdl = dat^.rHdlProcData
  ready <- hWaitForInput hdl tout
  if ready
    then BS.hGetSome hdl size
    else return BS.empty

---------------------------------------------------------------------------------
-- |
-- Append LF to the end of the string only if it does not already end with '\n'.
-- If the string is empty, return LF only to avoid exception from 'last'.
appendLF :: String -> String
appendLF str
  | null str         = DM._LF
  | last str /= '\n' = str ++ DM._LF
  | otherwise        = str

-- |
-- Encode a String to a UTF-8 encoded ByteString.
str2bsUTF8 :: String -> BS.ByteString
str2bsUTF8 = TE.encodeUtf8 . T.pack

-- |
-- Decode a ByteString to a String using UTF-8 (lenient) and strip ANSI escape sequences.
bs2strUTF8 :: BS.ByteString -> String
bs2strUTF8 = T.unpack . ANSI.stripAnsiEscapeCodes . TE.decodeUtf8With TEE.lenientDecode
