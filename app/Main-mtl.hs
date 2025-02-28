{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Repeatedly tries to install a random package from ::haskell (that is not
-- already installed and has at least one unmasked version)
--
-- - Notes successes, errors, packages that try to downgrade, and packages that
--   fail to resolve
-- - Rudimentary logger to files: /tmp/random-pkg-*
-- - Runs `haskell-updater` after every attempt -- Aborts script if it fails
-- - Aborts on on SIGINT (Ctrl+C)
-- Based on hololeap's build-random-haskell-pkgs.bash
module Main where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar,
                                          tryTakeMVar)
import           Control.Monad           (void)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.State     (StateT, evalStateT)
import qualified Data.ByteString         as B (writeFile)
import qualified Data.ByteString.Char8   as B (pack, hPutStrLn, putStrLn)
import           Data.Maybe              (isNothing)
import           GHRB.Core               (MonadGHRB, St, logOutput,
                                          readProcessWithExitCode, stderr,
                                          stdout, buildEmptyState, Running(Running))
import           GHRB.IO                 (randomBuild, terminate)
import qualified System.IO               as IO (stderr, stdout)
import           System.IO               (BufferMode (NoBuffering),
                                          hSetBuffering)
import           System.Posix            (Handler (Catch), addSignal,
                                          emptySignalSet, installHandler,
                                          sigINT, sigKILL, sigTERM)
import qualified System.Process          as SP (readProcessWithExitCode)
import           System.Random           (newStdGen)

instance MonadGHRB (StateT St IO) where
  readProcessWithExitCode fp args input =
    liftIO $ SP.readProcessWithExitCode fp args input
  stdout message = liftIO $ B.putStrLn message
  stderr message = liftIO $ B.hPutStrLn IO.stderr (B.pack message)
  logOutput filepath output = liftIO $ B.writeFile filepath (B.pack output)

builder :: MVar () -> StateT St IO ()
builder interrupt = do
  interrupted <- liftIO $ tryTakeMVar interrupt
  running <- randomBuild
  if isNothing interrupted && running == Running
    then randomBuild >> builder interrupt
    else (stdout . B.pack $ "Hello World")
           >> terminate (Just . B.pack $ "interrupted") >> pure ()

main :: IO ()
main = do
  interrupt <- newEmptyMVar
  initialState <- buildEmptyState <$> newStdGen
  void . hSetBuffering IO.stdout $ NoBuffering
  void . hSetBuffering IO.stderr $ NoBuffering
  void . installHandler sigINT (Catch $ putMVar interrupt ())
    $ Just . addSignal sigKILL . addSignal sigTERM $ emptySignalSet
  void . evalStateT (builder interrupt) $ initialState
