{-# LANGUAGE LambdaCase, FlexibleContexts, KindSignatures  #-}

module Main where

import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))
import Hedgehog.Internal.Seed     (random)
--import Hedgehog.Internal.Gen      (sample, printWith)

import Control.Monad.State.Strict (evalState)
import Control.Monad              (replicateM_)
import System.Environment         (getArgs)
import Data.Maybe (listToMaybe)
import Data.Text (pack, Text)

import EVM (VM, VMResult(..))

import Echidna.Exec
import Echidna.ABI
import Echidna.Solidity
--import Echidna.Internal.Runner (runProperty)

main :: IO ()
main = getArgs >>= \case
  []  -> putStrLn "Please provide a solidity file to analyze"
  filepath:args -> do
    (v,a,ts) <- loadSolidity filepath $ pack <$> listToMaybe args
    let prop = ePropertySeq v a (`checkSTest` (head ts)) 10 
    let gen = ePropertyGen v a (`checkSTest` (head ts)) 10
    replicateM_ 100 $ testProp prop gen

testProp :: Property -> Gen (Sequential t1 t) -> IO ()
testProp prop gen = do
                      seed <- random
                      b <- runProperty prop seed 1
                      if b then 
                          putStrLn $ printWith printCallSeq 1 seed gen
                      else
                          return ()

checkSTest :: VM -> Text -> Bool
checkSTest v t = case evalState (execCall (t, [])) v of
    (VMSuccess _) -> True
    _             -> False
