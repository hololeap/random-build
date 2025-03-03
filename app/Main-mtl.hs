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

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (StateT, evalStateT)
import           CoreMain               (runMain)
import qualified Data.ByteString        as B (writeFile, appendFile)
import qualified Data.ByteString.Char8  as B (hPutStrLn)
import           GHRB.Core              (MonadGHRB, St, 
                                         readProcessWithExitCode, Args, hPutStrLn, writeFile, appendFile) 
import qualified System.Process         as SP (readProcessWithExitCode)
import Control.Monad.Reader (ReaderT, runReaderT)

instance MonadGHRB (StateT St (ReaderT Args IO)) where
  readProcessWithExitCode fp args input =
    liftIO $ SP.readProcessWithExitCode fp args input
  writeFile fp output = liftIO $ B.writeFile fp output
  appendFile fp output = liftIO $ B.appendFile fp output
  hPutStrLn handle message = liftIO $ B.hPutStrLn handle message

runGHRB :: St -> Args -> StateT St (ReaderT Args IO) () -> IO ()
runGHRB initialState args builder = void . flip runReaderT args $ evalStateT builder initialState

main :: IO ()
main = runMain runGHRB
