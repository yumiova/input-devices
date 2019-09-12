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
        ),
    Syn (ReportSyn, ConfigSyn, MtReportSyn, DroppedSyn)
    )
where

data Ev
  = SynEv Syn
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

data Syn
  = ReportSyn
  | ConfigSyn
  | MtReportSyn
  | DroppedSyn
