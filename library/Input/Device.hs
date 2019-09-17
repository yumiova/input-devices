{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Device
  ( observe
    )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket)
import Foreign (Ptr, alloca, peek)
import Foreign.C (CInt (CInt))
import Input.Control (Control ((:<)))
import qualified Language.C.Inline as C (baseCtx, context, exp, include)
import System.Directory (listDirectory)
import System.Libevdev (Libevdev, libevdevCtx)
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

observeDevice :: Show a => Ptr Libevdev -> Control a -> IO ()
observeDevice libevdev ~(a :< f) =
  alloca $ \eventPtr -> do
    print a
    [C.exp| void {
      libevdev_next_event(
        $(struct libevdev *libevdev),
        LIBEVDEV_READ_FLAG_NORMAL,
        $(struct input_event *eventPtr)
      )
    } |]
    threadDelay 8192
    event <- peek eventPtr
    observeDevice libevdev (f event)

observePath :: Show a => FilePath -> Control a -> IO ()
observePath filePath control =
  withFd $ \(Fd fd) -> withLibevdev $ \libevdev -> do
    [C.exp| void { libevdev_set_fd($(struct libevdev *libevdev), $(int fd)) } |]
    observeDevice libevdev control
  where
    flags = defaultFileFlags {nonBlock = True}
    withFd = bracket (openFd filePath ReadOnly Nothing flags) closeFd
    withLibevdev =
      bracket
        [C.exp| struct libevdev * { libevdev_new() } |]
        (\libevdev -> [C.exp| void { libevdev_free($(struct libevdev *libevdev)) } |])

observe :: Show a => Control a -> IO ()
observe control = do
  inputDevices <- listDirectory prefix
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices ((`observePath` control) . (prefix <>))
  where
    prefix = "/dev/input/"
