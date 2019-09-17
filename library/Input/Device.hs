module Input.Device (observe) where

import Input.Control (Control)

observe :: Control a -> IO ()
observe _ = pure ()
