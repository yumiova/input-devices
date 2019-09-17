{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Device
  ( observe
    )
where

import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket)
import Foreign.C (CInt (CInt))
import Input.Control (Control)
import qualified Language.C.Inline as C (baseCtx, context, exp, include)
import System.Directory (listDirectory)
import System.Libevdev (libevdevCtx)
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    openFd
    )
import System.Posix.Types (Fd (Fd))

C.context (C.baseCtx <> libevdevCtx)

C.include "<libevdev/libevdev.h>"

observePath :: FilePath -> Control a -> IO ()
observePath filePath _ =
  withFd $ \(Fd fd) -> withLibevdev $ \libevdev -> do
    [C.exp| void { libevdev_set_fd($(struct libevdev *libevdev), $(int fd)) } |]
    print (filePath, fd, libevdev)
  where
    flags = defaultFileFlags {nonBlock = True}
    withFd = bracket (openFd filePath ReadOnly Nothing flags) closeFd
    withLibevdev =
      bracket
        [C.exp| struct libevdev * { libevdev_new() } |]
        (\libevdev -> [C.exp| void { libevdev_free($(struct libevdev *libevdev)) } |])

observe :: Control a -> IO ()
observe control = do
  inputDevices <- listDirectory prefix
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices ((`observePath` control) . (prefix <>))
  where
    prefix = "/dev/input/"
