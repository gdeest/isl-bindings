-- | Leak-detection harness for ISL test suites.
--
-- ISL prints a diagnostic to stderr from 'isl_ctx_free' when the context is
-- freed while it still owns objects. 'assertNoIslLeaks' runs an action with
-- fd 2 redirected to a temporary file and scans the captured stderr for
-- that diagnostic; any match fails the test.
--
-- fd 2 redirection is process-global, so concurrent callers are serialized
-- via a module-level lock. Pair with a sequential tasty 'TestTree' for
-- leak-checked tests; the rest of the suite can still run in parallel.
module Isl.LeakCheck
  ( assertNoIslLeaks
  , withCapturedStderr
  ) where

import Control.Concurrent.MVar
import Control.Exception (bracket, throwIO, ErrorCall(..))
import Control.Monad (when)
import Data.List (isInfixOf)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.IO
  (dup, dupTo, closeFd, stdError, openFd, defaultFileFlags,
   OpenMode(ReadWrite), OpenFileFlags(..))
import System.Posix.Process (getProcessID)

{-# NOINLINE redirectLock #-}
redirectLock :: MVar ()
redirectLock = unsafePerformIO (newMVar ())

-- | Run 'act' with fd 2 redirected to a tempfile; return its contents.
-- Real stderr is restored on exit even if 'act' throws.
withCapturedStderr :: IO a -> IO (a, String)
withCapturedStderr act = withMVar redirectLock $ \() -> do
  tmp <- mkTempPath
  bracket
    (do let flags = defaultFileFlags { trunc = True, creat = Just 0o600 }
        writeFd <- openFd tmp ReadWrite flags
        hFlush stderr
        savedFd <- dup stdError
        _ <- dupTo writeFd stdError
        closeFd writeFd
        pure savedFd)
    (\savedFd -> do
        hFlush stderr
        _ <- dupTo savedFd stdError
        closeFd savedFd
        removeFile tmp)
    (\_savedFd -> do
        r <- act
        hFlush stderr
        captured <- strictReadFile tmp
        pure (r, captured))

mkTempPath :: IO FilePath
mkTempPath = do
  tmpDir <- maybe "/tmp" id <$> lookupEnv "TMPDIR"
  pid <- getProcessID
  pure (tmpDir <> "/isl-leakcheck-" <> show pid <> ".err")

-- | Read a file and fully force its contents before returning.
strictReadFile :: FilePath -> IO String
strictReadFile p = do
  s <- readFile p
  _ <- length s `seq` pure ()
  pure s

-- | Run 'act' and fail (via 'ErrorCall') if ISL's end-of-life diagnostic
-- appears on stderr. Captured stderr is always re-emitted to the real
-- stderr so the developer sees the diagnostic.
assertNoIslLeaks :: String -> IO a -> IO a
assertNoIslLeaks label act = do
  (r, s) <- withCapturedStderr act
  when (not (null s)) $ hPutStr stderr s
  when (leakDetected s) $
    throwIO (ErrorCall (label ++ ": ISL reported outstanding objects at ctx free:\n" ++ s))
  pure r

-- | Heuristic match against ISL's ctx-not-empty diagnostic. The exact
-- wording varies across ISL versions; anchor on the word \"ctx\" plus a
-- negative-state token.
leakDetected :: String -> Bool
leakDetected = any isLeakLine . lines
  where
    isLeakLine l =
      let ll = map toLower l
      in "ctx" `isInfixOf` ll
         && ("not freed" `isInfixOf` ll
             || "not empty" `isInfixOf` ll
             || "still" `isInfixOf` ll
             || "leaked" `isInfixOf` ll)
    toLower c
      | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c
