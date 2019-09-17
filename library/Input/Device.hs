module Input.Device (observe) where

import Control.Concurrent.Async (forConcurrently_)
import Input.Control (Control)
import System.Directory (listDirectory)

observePath :: FilePath -> Control a -> IO ()
observePath filePath _ = print filePath

observe :: Control a -> IO ()
observe control = do
  inputDevices <- listDirectory prefix
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices ((`observePath` control) . (prefix <>))
  where
    prefix = "/dev/input/"
