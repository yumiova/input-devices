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
    Syn (ReportSyn, ConfigSyn, MtReportSyn, DroppedSyn),
    Key
      ( ReservedKey,
        EscKey,
        OneKey,
        TwoKey,
        ThreeKey,
        FourKey,
        FiveKey,
        SixKey,
        SevenKey,
        EightKey,
        NineKey,
        ZeroKey,
        MinusKey,
        EqualKey,
        BackspaceKey,
        TabKey,
        QKey,
        WKey,
        EKey,
        RKey,
        TKey,
        YKey,
        UKey,
        IKey,
        OKey,
        PKey,
        LeftbraceKey,
        RightbraceKey,
        EnterKey,
        LeftctrlKey,
        AKey,
        SKey,
        DKey,
        FKey,
        GKey,
        HKey,
        JKey,
        KKey,
        LKey,
        SemicolonKey,
        ApostropheKey,
        GraveKey,
        LeftshiftKey,
        BackslashKey,
        ZKey,
        XKey,
        CKey,
        VKey,
        BKey,
        NKey,
        MKey,
        CommaKey,
        DotKey,
        SlashKey,
        RightshiftKey,
        KpasteriskKey,
        LeftaltKey,
        SpaceKey,
        CapslockKey,
        F1Key,
        F2Key,
        F3Key,
        F4Key,
        F5Key,
        F6Key,
        F7Key,
        F8Key,
        F9Key,
        F10Key,
        NumlockKey,
        ScrolllockKey,
        Kp7Key,
        Kp8Key,
        Kp9Key,
        KpminusKey,
        Kp4Key,
        Kp5Key,
        Kp6Key,
        KpplusKey,
        Kp1Key,
        Kp2Key,
        Kp3Key,
        Kp0Key,
        KpdotKey,
        ZenkakuhankakuKey,
        OneZeroTwoNdKey,
        F11Key,
        F12Key,
        RoKey,
        KatakanaKey,
        HiraganaKey,
        HenkanKey,
        KatakanahiraganaKey,
        MuhenkanKey,
        KpjpcommaKey,
        KpenterKey,
        RightctrlKey,
        KpslashKey,
        SysrqKey,
        RightaltKey,
        LinefeedKey,
        HomeKey,
        UpKey,
        PageupKey,
        LeftKey,
        RightKey,
        EndKey,
        DownKey,
        PagedownKey,
        InsertKey,
        DeleteKey,
        MacroKey,
        MuteKey,
        VolumedownKey,
        VolumeupKey,
        PowerKey,
        KpequalKey,
        KpplusminusKey,
        PauseKey,
        ScaleKey,
        KpcommaKey,
        HangeulKey,
        HanguelKey,
        HanjaKey,
        YenKey,
        LeftmetaKey,
        RightmetaKey,
        ComposeKey,
        StopKey,
        AgainKey,
        PropsKey,
        UndoKey,
        FrontKey,
        CopyKey,
        OpenKey,
        PasteKey,
        FindKey,
        CutKey,
        HelpKey,
        MenuKey,
        CalcKey,
        SetupKey,
        SleepKey,
        WakeupKey,
        FileKey,
        SendfileKey,
        DeletefileKey,
        XferKey,
        Prog1Key,
        Prog2Key,
        WwwKey,
        MsdosKey,
        CoffeeKey,
        ScreenlockKey,
        RotateDisplayKey,
        DirectionKey,
        CyclewindowsKey,
        MailKey,
        BookmarksKey,
        ComputerKey,
        BackKey,
        ForwardKey,
        ClosecdKey,
        EjectcdKey,
        EjectclosecdKey,
        NextsongKey,
        PlaypauseKey,
        PrevioussongKey,
        StopcdKey,
        RecordKey,
        RewindKey,
        PhoneKey,
        IsoKey,
        ConfigKey,
        HomepageKey,
        RefreshKey,
        ExitKey,
        MoveKey,
        EditKey,
        ScrollupKey,
        ScrolldownKey,
        KpleftparenKey,
        KprightparenKey,
        NewKey,
        RedoKey,
        F13Key,
        F14Key,
        F15Key,
        F16Key,
        F17Key,
        F18Key,
        F19Key,
        F20Key,
        F21Key,
        F22Key,
        F23Key,
        F24Key,
        PlaycdKey,
        PausecdKey,
        Prog3Key,
        Prog4Key,
        DashboardKey,
        SuspendKey,
        CloseKey,
        PlayKey,
        FastforwardKey,
        BassboostKey,
        PrintKey,
        HpKey,
        CameraKey,
        SoundKey,
        QuestionKey,
        EmailKey,
        ChatKey,
        SearchKey,
        ConnectKey,
        FinanceKey,
        SportKey,
        ShopKey,
        AlteraseKey,
        CancelKey,
        BrightnessdownKey,
        BrightnessupKey,
        MediaKey,
        SwitchvideomodeKey,
        KbdillumtoggleKey,
        KbdillumdownKey,
        KbdillumupKey,
        SendKey,
        ReplyKey,
        ForwardmailKey,
        SaveKey,
        DocumentsKey,
        BatteryKey,
        BluetoothKey,
        WlanKey,
        UwbKey,
        UnknownKey,
        VideoNextKey,
        VideoPrevKey,
        BrightnessCycleKey,
        BrightnessAutoKey,
        BrightnessZeroKey,
        DisplayOffKey,
        WwanKey,
        WimaxKey,
        RfkillKey,
        MicmuteKey,
        OkKey,
        SelectKey,
        GotoKey,
        ClearKey,
        Power2Key,
        OptionKey,
        InfoKey,
        TimeKey,
        VendorKey,
        ArchiveKey,
        ProgramKey,
        ChannelKey,
        FavoritesKey,
        EpgKey,
        PvrKey,
        MhpKey,
        LanguageKey,
        TitleKey,
        SubtitleKey,
        AngleKey,
        FullScreenKey,
        ZoomKey,
        ModeKey,
        KeyboardKey,
        AspectRatioKey,
        ScreenKey,
        PcKey,
        TvKey,
        Tv2Key,
        VcrKey,
        Vcr2Key,
        SatKey,
        Sat2Key,
        CdKey,
        TapeKey,
        RadioKey,
        TunerKey,
        PlayerKey,
        TextKey,
        DvdKey,
        AuxKey,
        Mp3Key,
        AudioKey,
        VideoKey,
        DirectoryKey,
        ListKey,
        MemoKey,
        CalendarKey,
        RedKey,
        GreenKey,
        YellowKey,
        BlueKey,
        ChannelupKey,
        ChanneldownKey,
        FirstKey,
        LastKey,
        AbKey,
        NextKey,
        RestartKey,
        SlowKey,
        ShuffleKey,
        BreakKey,
        PreviousKey,
        DigitsKey,
        TeenKey,
        TwenKey,
        VideophoneKey,
        GamesKey,
        ZoominKey,
        ZoomoutKey,
        ZoomresetKey,
        WordprocessorKey,
        EditorKey,
        SpreadsheetKey,
        GraphicseditorKey,
        PresentationKey,
        DatabaseKey,
        NewsKey,
        VoicemailKey,
        AddressbookKey,
        MessengerKey,
        DisplaytoggleKey,
        BrightnessToggleKey,
        SpellcheckKey,
        LogoffKey,
        DollarKey,
        EuroKey,
        FramebackKey,
        FrameforwardKey,
        ContextMenuKey,
        MediaRepeatKey,
        TenchannelsupKey,
        TenchannelsdownKey,
        ImagesKey,
        DelEolKey,
        DelEosKey,
        InsLineKey,
        DelLineKey,
        FnKey,
        FnEscKey,
        FnF1Key,
        FnF2Key,
        FnF3Key,
        FnF4Key,
        FnF5Key,
        FnF6Key,
        FnF7Key,
        FnF8Key,
        FnF9Key,
        FnF10Key,
        FnF11Key,
        FnF12Key,
        Fn1Key,
        Fn2Key,
        FnDKey,
        FnEKey,
        FnFKey,
        FnSKey,
        FnBKey,
        BrlDot1Key,
        BrlDot2Key,
        BrlDot3Key,
        BrlDot4Key,
        BrlDot5Key,
        BrlDot6Key,
        BrlDot7Key,
        BrlDot8Key,
        BrlDot9Key,
        BrlDot10Key,
        Numeric0Key,
        Numeric1Key,
        Numeric2Key,
        Numeric3Key,
        Numeric4Key,
        Numeric5Key,
        Numeric6Key,
        Numeric7Key,
        Numeric8Key,
        Numeric9Key,
        NumericStarKey,
        NumericPoundKey,
        NumericAKey,
        NumericBKey,
        NumericCKey,
        NumericDKey,
        CameraFocusKey,
        WpsButtonKey,
        TouchpadToggleKey,
        TouchpadOnKey,
        TouchpadOffKey,
        CameraZoominKey,
        CameraZoomoutKey,
        CameraUpKey,
        CameraDownKey,
        CameraLeftKey,
        CameraRightKey,
        AttendantOnKey,
        AttendantOffKey,
        AttendantToggleKey,
        LightsToggleKey,
        AlsToggleKey,
        RotateLockToggleKey,
        ButtonconfigKey,
        TaskmanagerKey,
        JournalKey,
        ControlpanelKey,
        AppselectKey,
        ScreensaverKey,
        VoicecommandKey,
        AssistantKey,
        KbdLayoutNextKey,
        BrightnessMinKey,
        BrightnessMaxKey,
        KbdinputassistPrevKey,
        KbdinputassistNextKey,
        KbdinputassistPrevgroupKey,
        KbdinputassistNextgroupKey,
        KbdinputassistAcceptKey,
        KbdinputassistCancelKey,
        RightUpKey,
        RightDownKey,
        LeftUpKey,
        LeftDownKey,
        RootMenuKey,
        MediaTopMenuKey,
        Numeric11Key,
        Numeric12Key,
        AudioDescKey,
        ThreedModeKey,
        NextFavoriteKey,
        StopRecordKey,
        PauseRecordKey,
        VodKey,
        UnmuteKey,
        FastreverseKey,
        SlowreverseKey,
        DataKey,
        OnscreenKeyboardKey
        ),
    Rel
      ( XRel,
        YRel,
        ZRel,
        RxRel,
        RyRel,
        RzRel,
        HwheelRel,
        DialRel,
        WheelRel,
        MiscRel,
        ReservedRel,
        WheelHiResRel,
        HwheelHiResRel
        )
    )
where

data Ev
  = SynEv Syn
  | KeyEv Key
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

data Key
  = ReservedKey
  | EscKey
  | OneKey
  | TwoKey
  | ThreeKey
  | FourKey
  | FiveKey
  | SixKey
  | SevenKey
  | EightKey
  | NineKey
  | ZeroKey
  | MinusKey
  | EqualKey
  | BackspaceKey
  | TabKey
  | QKey
  | WKey
  | EKey
  | RKey
  | TKey
  | YKey
  | UKey
  | IKey
  | OKey
  | PKey
  | LeftbraceKey
  | RightbraceKey
  | EnterKey
  | LeftctrlKey
  | AKey
  | SKey
  | DKey
  | FKey
  | GKey
  | HKey
  | JKey
  | KKey
  | LKey
  | SemicolonKey
  | ApostropheKey
  | GraveKey
  | LeftshiftKey
  | BackslashKey
  | ZKey
  | XKey
  | CKey
  | VKey
  | BKey
  | NKey
  | MKey
  | CommaKey
  | DotKey
  | SlashKey
  | RightshiftKey
  | KpasteriskKey
  | LeftaltKey
  | SpaceKey
  | CapslockKey
  | F1Key
  | F2Key
  | F3Key
  | F4Key
  | F5Key
  | F6Key
  | F7Key
  | F8Key
  | F9Key
  | F10Key
  | NumlockKey
  | ScrolllockKey
  | Kp7Key
  | Kp8Key
  | Kp9Key
  | KpminusKey
  | Kp4Key
  | Kp5Key
  | Kp6Key
  | KpplusKey
  | Kp1Key
  | Kp2Key
  | Kp3Key
  | Kp0Key
  | KpdotKey
  | ZenkakuhankakuKey
  | OneZeroTwoNdKey
  | F11Key
  | F12Key
  | RoKey
  | KatakanaKey
  | HiraganaKey
  | HenkanKey
  | KatakanahiraganaKey
  | MuhenkanKey
  | KpjpcommaKey
  | KpenterKey
  | RightctrlKey
  | KpslashKey
  | SysrqKey
  | RightaltKey
  | LinefeedKey
  | HomeKey
  | UpKey
  | PageupKey
  | LeftKey
  | RightKey
  | EndKey
  | DownKey
  | PagedownKey
  | InsertKey
  | DeleteKey
  | MacroKey
  | MuteKey
  | VolumedownKey
  | VolumeupKey
  | PowerKey
  | KpequalKey
  | KpplusminusKey
  | PauseKey
  | ScaleKey
  | KpcommaKey
  | HangeulKey
  | HanguelKey
  | HanjaKey
  | YenKey
  | LeftmetaKey
  | RightmetaKey
  | ComposeKey
  | StopKey
  | AgainKey
  | PropsKey
  | UndoKey
  | FrontKey
  | CopyKey
  | OpenKey
  | PasteKey
  | FindKey
  | CutKey
  | HelpKey
  | MenuKey
  | CalcKey
  | SetupKey
  | SleepKey
  | WakeupKey
  | FileKey
  | SendfileKey
  | DeletefileKey
  | XferKey
  | Prog1Key
  | Prog2Key
  | WwwKey
  | MsdosKey
  | CoffeeKey
  | ScreenlockKey
  | RotateDisplayKey
  | DirectionKey
  | CyclewindowsKey
  | MailKey
  | BookmarksKey
  | ComputerKey
  | BackKey
  | ForwardKey
  | ClosecdKey
  | EjectcdKey
  | EjectclosecdKey
  | NextsongKey
  | PlaypauseKey
  | PrevioussongKey
  | StopcdKey
  | RecordKey
  | RewindKey
  | PhoneKey
  | IsoKey
  | ConfigKey
  | HomepageKey
  | RefreshKey
  | ExitKey
  | MoveKey
  | EditKey
  | ScrollupKey
  | ScrolldownKey
  | KpleftparenKey
  | KprightparenKey
  | NewKey
  | RedoKey
  | F13Key
  | F14Key
  | F15Key
  | F16Key
  | F17Key
  | F18Key
  | F19Key
  | F20Key
  | F21Key
  | F22Key
  | F23Key
  | F24Key
  | PlaycdKey
  | PausecdKey
  | Prog3Key
  | Prog4Key
  | DashboardKey
  | SuspendKey
  | CloseKey
  | PlayKey
  | FastforwardKey
  | BassboostKey
  | PrintKey
  | HpKey
  | CameraKey
  | SoundKey
  | QuestionKey
  | EmailKey
  | ChatKey
  | SearchKey
  | ConnectKey
  | FinanceKey
  | SportKey
  | ShopKey
  | AlteraseKey
  | CancelKey
  | BrightnessdownKey
  | BrightnessupKey
  | MediaKey
  | SwitchvideomodeKey
  | KbdillumtoggleKey
  | KbdillumdownKey
  | KbdillumupKey
  | SendKey
  | ReplyKey
  | ForwardmailKey
  | SaveKey
  | DocumentsKey
  | BatteryKey
  | BluetoothKey
  | WlanKey
  | UwbKey
  | UnknownKey
  | VideoNextKey
  | VideoPrevKey
  | BrightnessCycleKey
  | BrightnessAutoKey
  | BrightnessZeroKey
  | DisplayOffKey
  | WwanKey
  | WimaxKey
  | RfkillKey
  | MicmuteKey
  | OkKey
  | SelectKey
  | GotoKey
  | ClearKey
  | Power2Key
  | OptionKey
  | InfoKey
  | TimeKey
  | VendorKey
  | ArchiveKey
  | ProgramKey
  | ChannelKey
  | FavoritesKey
  | EpgKey
  | PvrKey
  | MhpKey
  | LanguageKey
  | TitleKey
  | SubtitleKey
  | AngleKey
  | FullScreenKey
  | ZoomKey
  | ModeKey
  | KeyboardKey
  | AspectRatioKey
  | ScreenKey
  | PcKey
  | TvKey
  | Tv2Key
  | VcrKey
  | Vcr2Key
  | SatKey
  | Sat2Key
  | CdKey
  | TapeKey
  | RadioKey
  | TunerKey
  | PlayerKey
  | TextKey
  | DvdKey
  | AuxKey
  | Mp3Key
  | AudioKey
  | VideoKey
  | DirectoryKey
  | ListKey
  | MemoKey
  | CalendarKey
  | RedKey
  | GreenKey
  | YellowKey
  | BlueKey
  | ChannelupKey
  | ChanneldownKey
  | FirstKey
  | LastKey
  | AbKey
  | NextKey
  | RestartKey
  | SlowKey
  | ShuffleKey
  | BreakKey
  | PreviousKey
  | DigitsKey
  | TeenKey
  | TwenKey
  | VideophoneKey
  | GamesKey
  | ZoominKey
  | ZoomoutKey
  | ZoomresetKey
  | WordprocessorKey
  | EditorKey
  | SpreadsheetKey
  | GraphicseditorKey
  | PresentationKey
  | DatabaseKey
  | NewsKey
  | VoicemailKey
  | AddressbookKey
  | MessengerKey
  | DisplaytoggleKey
  | BrightnessToggleKey
  | SpellcheckKey
  | LogoffKey
  | DollarKey
  | EuroKey
  | FramebackKey
  | FrameforwardKey
  | ContextMenuKey
  | MediaRepeatKey
  | TenchannelsupKey
  | TenchannelsdownKey
  | ImagesKey
  | DelEolKey
  | DelEosKey
  | InsLineKey
  | DelLineKey
  | FnKey
  | FnEscKey
  | FnF1Key
  | FnF2Key
  | FnF3Key
  | FnF4Key
  | FnF5Key
  | FnF6Key
  | FnF7Key
  | FnF8Key
  | FnF9Key
  | FnF10Key
  | FnF11Key
  | FnF12Key
  | Fn1Key
  | Fn2Key
  | FnDKey
  | FnEKey
  | FnFKey
  | FnSKey
  | FnBKey
  | BrlDot1Key
  | BrlDot2Key
  | BrlDot3Key
  | BrlDot4Key
  | BrlDot5Key
  | BrlDot6Key
  | BrlDot7Key
  | BrlDot8Key
  | BrlDot9Key
  | BrlDot10Key
  | Numeric0Key
  | Numeric1Key
  | Numeric2Key
  | Numeric3Key
  | Numeric4Key
  | Numeric5Key
  | Numeric6Key
  | Numeric7Key
  | Numeric8Key
  | Numeric9Key
  | NumericStarKey
  | NumericPoundKey
  | NumericAKey
  | NumericBKey
  | NumericCKey
  | NumericDKey
  | CameraFocusKey
  | WpsButtonKey
  | TouchpadToggleKey
  | TouchpadOnKey
  | TouchpadOffKey
  | CameraZoominKey
  | CameraZoomoutKey
  | CameraUpKey
  | CameraDownKey
  | CameraLeftKey
  | CameraRightKey
  | AttendantOnKey
  | AttendantOffKey
  | AttendantToggleKey
  | LightsToggleKey
  | AlsToggleKey
  | RotateLockToggleKey
  | ButtonconfigKey
  | TaskmanagerKey
  | JournalKey
  | ControlpanelKey
  | AppselectKey
  | ScreensaverKey
  | VoicecommandKey
  | AssistantKey
  | KbdLayoutNextKey
  | BrightnessMinKey
  | BrightnessMaxKey
  | KbdinputassistPrevKey
  | KbdinputassistNextKey
  | KbdinputassistPrevgroupKey
  | KbdinputassistNextgroupKey
  | KbdinputassistAcceptKey
  | KbdinputassistCancelKey
  | RightUpKey
  | RightDownKey
  | LeftUpKey
  | LeftDownKey
  | RootMenuKey
  | MediaTopMenuKey
  | Numeric11Key
  | Numeric12Key
  | AudioDescKey
  | ThreedModeKey
  | NextFavoriteKey
  | StopRecordKey
  | PauseRecordKey
  | VodKey
  | UnmuteKey
  | FastreverseKey
  | SlowreverseKey
  | DataKey
  | OnscreenKeyboardKey

data Rel
  = XRel
  | YRel
  | ZRel
  | RxRel
  | RyRel
  | RzRel
  | HwheelRel
  | DialRel
  | WheelRel
  | MiscRel
  | ReservedRel
  | WheelHiResRel
  | HwheelHiResRel
