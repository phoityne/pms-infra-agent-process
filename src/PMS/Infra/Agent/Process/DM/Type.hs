{-# LANGUAGE TemplateHaskell #-}

module PMS.Infra.Agent.Process.DM.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.Default
import Data.Aeson.TH
import Data.Map.Strict (Map)
import qualified Control.Concurrent.STM as STM
import qualified System.Process as S
import qualified GHC.IO.Handle as S

import qualified PMS.Domain.Model.DM.Type as DM
import qualified PMS.Domain.Model.DM.TH as DM

-- |
--
data ProcData = ProcData {
    _wHdLProcData :: S.Handle
  , _rHdlProcData :: S.Handle
  , _eHdlProcData :: S.Handle
  , _pHdlProcData :: S.ProcessHandle
  }

makeLenses ''ProcData

-- |
--
data AppData = AppData {
               _processAppData :: STM.TMVar (Maybe ProcData)
             }

makeLenses ''AppData

defaultAppData :: IO AppData
defaultAppData = do
  mgrVar <- STM.newTMVarIO Nothing
  return AppData {
           _processAppData = mgrVar
         }

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))

-- |
--
type IOTask = IO


--------------------------------------------------------------------------------------------
-- |
-- agent-proc-run の options オブジェクト
--
data ProcRunToolParams =
  ProcRunToolParams {
    _commandProcRunToolParams     :: String
  , _argumentsProcRunToolParams   :: Maybe [String]
  , _environmentProcRunToolParams :: Maybe (Map String String)
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ProcRunToolParams", omitNothingFields = True} ''ProcRunToolParams)
makeLenses ''ProcRunToolParams

instance Default ProcRunToolParams where
  def = ProcRunToolParams {
        _commandProcRunToolParams     = def
      , _argumentsProcRunToolParams   = def
      , _environmentProcRunToolParams = def
      }

-- |
-- agent-proc-read の arguments
--
data ProcIntToolParams =
  ProcIntToolParams {
    _argumentsProcIntToolParams :: Int
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ProcIntToolParams", omitNothingFields = True} ''ProcIntToolParams)
makeLenses ''ProcIntToolParams

instance Default ProcIntToolParams where
  def = ProcIntToolParams {
        _argumentsProcIntToolParams = def
      }

-- |
-- agent-proc-write の arguments
--
data ProcStringToolParams =
  ProcStringToolParams {
    _argumentsProcStringToolParams :: String
  } deriving (Show, Read, Eq)

$(deriveJSON defaultOptions {fieldLabelModifier = DM.dropDataName "ProcStringToolParams", omitNothingFields = True} ''ProcStringToolParams)
makeLenses ''ProcStringToolParams

instance Default ProcStringToolParams where
  def = ProcStringToolParams {
        _argumentsProcStringToolParams = def
      }
