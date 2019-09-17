module Input.Device (observe) where

import Input.Control (Control)
import System.Directory (listDirectory)

observe :: Control a -> IO ()
observe _ = do
  inputDevices <- listDirectory "/dev/input"
  let eventDevices = filter ((== "event") . take 5) inputDevices
  print eventDevices
