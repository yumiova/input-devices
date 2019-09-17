{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Device
  ( observe
    )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket)
import Data.Foldable (foldlM)
import Foreign (Ptr, alloca, peek)
import Foreign.C (CInt (CInt))
import Input.Control (Control ((:<)))
import qualified Language.C.Inline as C (baseCtx, context, exp, include, pure)
import System.Directory (listDirectory)
import System.Libevdev (InputEvent, Libevdev, libevdevCtx)
import System.Posix.IO
  ( OpenFileFlags (nonBlock),
    OpenMode (ReadOnly),
    closeFd,
    defaultFileFlags,
    openFd
    )
import System.Posix.Types (Fd (Fd))

C.context (C.baseCtx <> libevdevCtx)

C.include "<errno.h>"

C.include "<libevdev/libevdev.h>"

popEvents :: Ptr Libevdev -> IO [InputEvent]
popEvents libevdev = alloca go
  where
    dispatch status target
      | status == [C.pure| int { -EAGAIN } |] = pure []
      | status == [C.pure| int { LIBEVDEV_READ_STATUS_SUCCESS } |] =
        do
          event <- peek target
          (event :) <$> go target
      | status == [C.pure| int { LIBEVDEV_READ_STATUS_SYNC } |] =
        do
          putStrLn "unimplemented: input device syncing"
          pure []
      | otherwise = error ("unimplemented: status" <> show status)
    go target = do
      status <-
        [C.exp| int {
          libevdev_next_event(
            $(struct libevdev *libevdev),
            LIBEVDEV_READ_FLAG_NORMAL,
            $(struct input_event *target)
          )
        } |]
      dispatch status target

observeStep :: Show a => Ptr Libevdev -> Control a -> IO (Control a)
observeStep libevdev control = do
  events <- popEvents libevdev
  foldlM apply control events
  where
    apply (a :< f) event = do
      print a
      pure (f event)

observeDevice :: Show a => Ptr Libevdev -> Control a -> IO ()
observeDevice libevdev control = do
  increment <- observeStep libevdev control
  threadDelay 8192
  observeDevice libevdev increment

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
