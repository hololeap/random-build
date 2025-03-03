{-# LANGUAGE DataKinds             #-}
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

import           Control.Monad                      (void)
import           Control.Monad.Reader               (MonadReader, ask, local)
import           Control.Monad.State                (MonadState, state)
import           CoreMain                           (runMain)
import qualified Data.ByteString.Char8              as B (pack)
import           Effectful                          (Eff, IOE, runEff, (:>))
import           Effectful.Console.ByteString       (Console, runConsole)
import           Effectful.FileSystem               (FileSystem, runFileSystem)
import qualified Effectful.FileSystem.IO            as IO (stderr, stdout)
import qualified Effectful.FileSystem.IO.ByteString as EF (appendFile,
                                                           hPutStrLn, writeFile)
import           Effectful.Process                  (Process, runProcess)
import qualified Effectful.Process                  as EP (readProcessWithExitCode)
import           Effectful.Reader.Static            (Reader, asks, runReader)
import qualified Effectful.Reader.Static            as ER (ask, local)
import           Effectful.State.Static.Shared      (State, evalState)
import qualified Effectful.State.Static.Shared      as ES (state)
import           GHRB.Core                          (Args, MonadGHRB,
                                                     Output (OutFile, Std),
                                                     St, bStderr, getOutputMode, getErrMode,
                                                     logOutput,
                                                     readProcessWithExitCode,
                                                     stdout)

instance ( IOE :> es
         , Console :> es
         , Process :> es
         , FileSystem :> es
         , Reader Args :> es
         , MonadReader Args (Eff es)
         , MonadState St (Eff es)
         ) =>
         MonadGHRB (Eff es) where
  readProcessWithExitCode = EP.readProcessWithExitCode
  stdout message = do
    outmode <- asks getOutputMode
    case outmode of
      Std     -> EF.hPutStrLn IO.stdout message
      OutFile fp -> EF.appendFile fp message
  bStderr message = do
    errmode <- asks getErrMode
    case errmode of
      Nothing -> pure ()
      Just Std -> EF.hPutStrLn IO.stderr message
      Just (OutFile fp) -> EF.appendFile fp message
  logOutput filepath = EF.writeFile filepath . B.pack

instance (MonadState a (Eff es), State a :> es) => MonadState a (Eff es) where
  state = ES.state

instance (MonadReader a (Eff es), Reader a :> es) => MonadReader a (Eff es) where
  ask = ER.ask
  local = ER.local

runGHRB ::
     St
  -> Args
  -> Eff [Reader Args, State St, Process, Console, FileSystem, IOE]
       ()
  -> IO ()
runGHRB initialState args =
  void
    . runEff
    . runFileSystem
    . runConsole
    . runProcess
    . evalState initialState
    . runReader args

main :: IO ()
main = runMain runGHRB
