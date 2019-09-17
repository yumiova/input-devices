module Input.Device (observe) where

import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket)
import Input.Control (Control)
import System.Directory (listDirectory)
import System.Posix.IO (OpenMode (ReadOnly), closeFd, defaultFileFlags, openFd)

observePath :: FilePath -> Control a -> IO ()
observePath filePath _ = withFd $ \fd -> print (filePath, fd)
  where
    flags = defaultFileFlags
    withFd = bracket (openFd filePath ReadOnly Nothing flags) closeFd

observe :: Control a -> IO ()
observe control = do
  inputDevices <- listDirectory prefix
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices ((`observePath` control) . (prefix <>))
  where
    prefix = "/dev/input/"
