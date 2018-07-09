{-# LANGUAGE DeriveGeneric, FlexibleContexts, TemplateHaskell #-}

module Echidna.Config where

import Control.Monad.Catch    (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens
import Control.Exception      (Exception)
import Control.Monad.Reader   (ReaderT, runReaderT)
import Data.Aeson
import GHC.Generics

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

import EVM.Types (Addr, W256)

import Echidna.Property

data Config = Config
  { _solcArgs :: Maybe String
  , _epochs :: Int
  , _testLimit :: Int
  , _returnType :: PropertyType
  , _range :: Int
  , _gasLimit :: W256 
  , _contractAddr :: Addr
  , _sender       :: Addr
  , _addrList     :: Maybe [Addr] }
  deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config

------------------------------------
-- Defaults

defaultContractAddr :: Addr
defaultContractAddr = 0x00a329c0648769a73afac7f9381e08fb43dbea72

otherContractAddr :: Addr
otherContractAddr = 0x67518339e369ab3d591d3569ab0a0d83b2ff5198

defaultSender :: Addr
defaultSender = 0x00a329c0648769a73afac7f9381e08fb43dbea70

defaultConfig :: Config
defaultConfig = Config
  { _solcArgs = Nothing
  , _epochs = 2
  , _returnType = ShouldReturnTrue
  , _testLimit = 10000
  , _range = 10
  , _gasLimit = 0xffffffffffffffff 
  , _contractAddr = defaultContractAddr
  , _sender       = defaultSender
  , _addrList     = Just [defaultContractAddr, defaultSender, otherContractAddr, 0x0] }

withDefaultConfig :: ReaderT Config m a -> m a
withDefaultConfig = (flip runReaderT) defaultConfig


------------------------------------
-- Parser

data ParseException = ParseException FilePath

instance Show ParseException where
  show (ParseException f) = "Could not parse config file " ++ (show f)

instance Exception ParseException

parseConfig :: (MonadThrow m, MonadIO m) => FilePath -> m Config
parseConfig file = do
    content <- liftIO $ BS.readFile file
    let parsedContent = Y.decode content :: Maybe Config
    case parsedContent of
        Nothing -> throwM (ParseException file)
        (Just c) -> return c  
