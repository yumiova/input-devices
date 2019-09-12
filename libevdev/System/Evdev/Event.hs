module System.Evdev.Event
  ( Ev
      ( SynEv,
        KeyEv,
        RelEv,
        AbsEv,
        MscEv,
        SwEv,
        LedEv,
        SndEv,
        RepEv,
        FfEv,
        PwrEv,
        FfStatusEv,
        MaxEv
        )
    )
where

data Ev
  = SynEv
  | KeyEv
  | RelEv
  | AbsEv
  | MscEv
  | SwEv
  | LedEv
  | SndEv
  | RepEv
  | FfEv
  | PwrEv
  | FfStatusEv
  | MaxEv
