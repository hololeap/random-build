{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
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

import           Control.Concurrent.MVar            (MVar, newEmptyMVar,
                                                     putMVar, tryTakeMVar)
import           Control.Monad                      (void)
import           Control.Monad.State                (MonadState, state)
import qualified Data.ByteString.Char8              as B (pack)
import           Data.Maybe                         (isNothing)
import           Effectful                          (Eff, IOE, liftIO, runEff,
                                                     (:>))
import           Effectful.Console.ByteString       (Console, runConsole)
import qualified Effectful.Console.ByteString       as EC (putStrLn)
import           Effectful.FileSystem               (FileSystem, runFileSystem)
import qualified Effectful.FileSystem.IO            as IO (stderr, stdout)
import qualified Effectful.FileSystem.IO.ByteString as EF (hPutStrLn, writeFile)
import           Effectful.Process                  (Process, runProcess)
import qualified Effectful.Process                  as EP (readProcessWithExitCode)
import           Effectful.State.Static.Shared      (State, evalState)
import qualified Effectful.State.Static.Shared      as ES (state)
import           GHRB.Core                          (MonadGHRB,
                                                     Running (Running), St,
                                                     buildEmptyState, logOutput,
                                                     readProcessWithExitCode,
                                                     stderr, stdout)
import           GHRB.IO                            (randomBuild, terminate)
import           System.IO                          (BufferMode (NoBuffering),
                                                     hSetBuffering)
import           System.Posix.Signals               (Handler (Catch), addSignal,
                                                     emptySignalSet,
                                                     installHandler, sigINT,
                                                     sigKILL, sigTERM)
import           System.Random                      (newStdGen)

instance ( IOE :> es
         , Console :> es
         , Process :> es
         , FileSystem :> es
         , MonadState St (Eff es)
         ) =>
         MonadGHRB (Eff es) where
  readProcessWithExitCode = EP.readProcessWithExitCode
  stdout = EC.putStrLn
  stderr = EF.hPutStrLn IO.stderr . B.pack
  logOutput filepath = EF.writeFile filepath . B.pack

instance (MonadState a (Eff es), State a :> es) => MonadState a (Eff es) where
  state = ES.state

builder ::
     (IOE :> es, Process :> es, FileSystem :> es, State St :> es, Console :> es)
  => MVar ()
  -> Eff es ()
builder interrupt = do
  interrupted <- liftIO $ tryTakeMVar interrupt
  running <- randomBuild
  if isNothing interrupted && running == Running
    then randomBuild >> builder interrupt
    else (stdout . B.pack $ "Hello World")
           >> terminate (Just . B.pack $ "interrupted")
           >> pure ()

main :: IO ()
main = do
  interrupt <- newEmptyMVar
  initialState <- buildEmptyState <$> liftIO newStdGen
  hSetBuffering IO.stdout NoBuffering
  hSetBuffering IO.stderr NoBuffering
  void . installHandler sigINT (Catch $ putMVar interrupt ())
    $ Just . addSignal sigKILL . addSignal sigTERM
    $ emptySignalSet
  void
    . runEff
    . runFileSystem
    . runConsole
    . runProcess
    . evalState initialState
    $ builder interrupt
