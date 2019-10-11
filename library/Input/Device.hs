{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Input.Device
  ( observe
    )
where

import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_)
import Control.Exception (bracket)
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Profunctor
import Foreign (Ptr, alloca, peek)
import Foreign.C (CInt (CInt))
import Input.Sink (Sink)
import Input.Source (Source (runSource))
import qualified Input.Source as Source (Stream ((:<)))
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

data Control a b
  = Control (Sink a) (Source b)

instance Profunctor Control where
  dimap f g ~(Control sink source) = Control (contramap f sink) (g <$> source)

instance Functor (Control a) where
  fmap f ~(Control sink source) = Control sink (fmap f source)

instance Applicative (Control a) where

  pure a = Control conquer (pure a)

  ~(Control lsink fsource) <*> ~(Control rsink source) =
    Control (divide (id &&& id) lsink rsink) (fsource <*> source)

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
      | otherwise = error ("unimplemented: status " <> show status)
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

observeDevice :: Show a => Ptr Libevdev -> Source.Stream a -> IO ()
observeDevice libevdev (a Source.:< f) = do
  delta <- popEvents libevdev
  if not (null delta) then print a else pure ()
  let increment = f delta
  threadDelay 8192
  observeDevice libevdev increment

observePath :: Show a => FilePath -> Source a -> IO ()
observePath filePath control =
  withFd $ \(Fd fd) -> withLibevdev $ \libevdev -> do
    [C.exp| void { libevdev_set_fd($(struct libevdev *libevdev), $(int fd)) } |]
    minitial <- runSource control libevdev
    case minitial of
      Nothing -> pure ()
      Just initial -> observeDevice libevdev initial
  where
    flags = defaultFileFlags {nonBlock = True}
    withFd = bracket (openFd filePath ReadOnly Nothing flags) closeFd
    withLibevdev =
      bracket
        [C.exp| struct libevdev * { libevdev_new() } |]
        ( \libevdev ->
            [C.exp| void { libevdev_free($(struct libevdev *libevdev)) } |]
          )

observe :: Show a => Source a -> IO ()
observe control = do
  inputDevices <- listDirectory prefix
  let eventDevices = filter ((== "event") . take 5) inputDevices
  forConcurrently_ eventDevices ((`observePath` control) . (prefix <>))
  where
    prefix = "/dev/input/"
