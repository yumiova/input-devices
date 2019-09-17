module Input.Device (observe) where

import Control.Concurrent.Async (forConcurrently_)
import Input.Control (Control)
import System.Directory (listDirectory)

observe :: Control a -> IO ()
observe _ = do
  inputDevices <- listDirectory "/dev/input"
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices print
