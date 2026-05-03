module PMS.Infra.Agent.Process.DM.Constant where

--------------------------------------------------------------------------------
-- |
--
_LOG_FILE_NAME :: String
_LOG_FILE_NAME = "pms-infra-agent-process.log"

-- |
--
_READ_BUFFER_SIZE :: Int
_READ_BUFFER_SIZE = 4096

-- | Wait time for proc-read input (milliseconds).
-- Minimum wait to return an empty string immediately when no data is available.
_PROC_READ_WAIT_MSEC :: Int
_PROC_READ_WAIT_MSEC = 100
