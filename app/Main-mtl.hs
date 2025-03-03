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

import           Control.Exception.Safe (finally)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Control.Monad.State    (StateT, evalStateT)
import           CoreMain               (runMain)
import qualified Data.ByteString        as B (appendFile, writeFile)
import qualified Data.ByteString.Char8  as B (hPutStrLn)
import           GHRB.Core              (Args, MonadGHRB, St, appendFile,
                                         hPutStrLn, readProcessWithExitCode,
                                         writeFile)
import           GHRB.IO                (terminate)
import qualified System.Process         as SP (readProcessWithExitCode)

instance MonadGHRB (StateT St (ReaderT Args IO)) where
  readProcessWithExitCode fp args input =
    liftIO $ SP.readProcessWithExitCode fp args input
  writeFile fp output = liftIO $ B.writeFile fp output
  appendFile fp output = liftIO $ B.appendFile fp output
  hPutStrLn handle message = liftIO $ B.hPutStrLn handle message

runGHRB :: St -> Args -> StateT St (ReaderT Args IO) () -> IO ()
runGHRB initialState args builder =
  flip runReaderT args . flip evalStateT initialState
    $ finally builder terminate

main :: IO ()
main = runMain runGHRB
