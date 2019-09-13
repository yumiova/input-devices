{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
        FfStatusEv
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
        MiscBtn,
        ZeroBtn,
        OneBtn,
        TwoBtn,
        ThreeBtn,
        FourBtn,
        FiveBtn,
        SixBtn,
        SevenBtn,
        EightBtn,
        NineBtn,
        MouseBtn,
        LeftBtn,
        RightBtn,
        MiddleBtn,
        SideBtn,
        ExtraBtn,
        ForwardBtn,
        BackBtn,
        TaskBtn,
        JoystickBtn,
        TriggerBtn,
        ThumbBtn,
        Thumb2Btn,
        TopBtn,
        Top2Btn,
        PinkieBtn,
        BaseBtn,
        Base2Btn,
        Base3Btn,
        Base4Btn,
        Base5Btn,
        Base6Btn,
        DeadBtn,
        GamepadBtn,
        SouthBtn,
        ABtn,
        EastBtn,
        BBtn,
        CBtn,
        NorthBtn,
        XBtn,
        WestBtn,
        YBtn,
        ZBtn,
        TlBtn,
        TrBtn,
        Tl2Btn,
        Tr2Btn,
        SelectBtn,
        StartBtn,
        ModeBtn,
        ThumblBtn,
        ThumbrBtn,
        DigiBtn,
        ToolPenBtn,
        ToolRubberBtn,
        ToolBrushBtn,
        ToolPencilBtn,
        ToolAirbrushBtn,
        ToolFingerBtn,
        ToolMouseBtn,
        ToolLensBtn,
        ToolQuinttapBtn,
        Stylus3Btn,
        TouchBtn,
        StylusBtn,
        Stylus2Btn,
        ToolDoubletapBtn,
        ToolTripletapBtn,
        ToolQuadtapBtn,
        WheelBtn,
        GearDownBtn,
        GearUpBtn,
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
        ZoomKey,
        ModeKey,
        KeyboardKey,
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
        TenChannelsupKey,
        TenChannelsdownKey,
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
        DpadUpBtn,
        DpadDownBtn,
        DpadLeftBtn,
        DpadRightBtn,
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
        ThreeDModeKey,
        NextFavoriteKey,
        StopRecordKey,
        PauseRecordKey,
        VodKey,
        UnmuteKey,
        FastreverseKey,
        SlowreverseKey,
        DataKey,
        OnscreenKeyboardKey,
        TriggerHappyBtn,
        TriggerHappy1Btn,
        TriggerHappy2Btn,
        TriggerHappy3Btn,
        TriggerHappy4Btn,
        TriggerHappy5Btn,
        TriggerHappy6Btn,
        TriggerHappy7Btn,
        TriggerHappy8Btn,
        TriggerHappy9Btn,
        TriggerHappy10Btn,
        TriggerHappy11Btn,
        TriggerHappy12Btn,
        TriggerHappy13Btn,
        TriggerHappy14Btn,
        TriggerHappy15Btn,
        TriggerHappy16Btn,
        TriggerHappy17Btn,
        TriggerHappy18Btn,
        TriggerHappy19Btn,
        TriggerHappy20Btn,
        TriggerHappy21Btn,
        TriggerHappy22Btn,
        TriggerHappy23Btn,
        TriggerHappy24Btn,
        TriggerHappy25Btn,
        TriggerHappy26Btn,
        TriggerHappy27Btn,
        TriggerHappy28Btn,
        TriggerHappy29Btn,
        TriggerHappy30Btn,
        TriggerHappy31Btn,
        TriggerHappy32Btn,
        TriggerHappy33Btn,
        TriggerHappy34Btn,
        TriggerHappy35Btn,
        TriggerHappy36Btn,
        TriggerHappy37Btn,
        TriggerHappy38Btn,
        TriggerHappy39Btn,
        TriggerHappy40Btn
        ),
    decodeKey,
    encodeKey,
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
        MiscRel
        ),
    decodeRel,
    encodeRel,
    Abs
      ( XAbs,
        YAbs,
        ZAbs,
        RxAbs,
        RyAbs,
        RzAbs,
        ThrottleAbs,
        RudderAbs,
        WheelAbs,
        GasAbs,
        BrakeAbs,
        Hat0xAbs,
        Hat0yAbs,
        Hat1xAbs,
        Hat1yAbs,
        Hat2xAbs,
        Hat2yAbs,
        Hat3xAbs,
        Hat3yAbs,
        PressureAbs,
        DistanceAbs,
        TiltXAbs,
        TiltYAbs,
        ToolWidthAbs,
        VolumeAbs,
        MiscAbs,
        ReservedAbs,
        MtSlotAbs,
        MtTouchMajorAbs,
        MtTouchMinorAbs,
        MtWidthMajorAbs,
        MtWidthMinorAbs,
        MtOrientationAbs,
        MtPositionXAbs,
        MtPositionYAbs,
        MtToolTypeAbs,
        MtBlobIdAbs,
        MtTrackingIdAbs,
        MtPressureAbs,
        MtDistanceAbs,
        MtToolXAbs,
        MtToolYAbs
        ),
    decodeAbs,
    encodeAbs,
    Sw
      ( LidSw,
        TabletModeSw,
        HeadphoneInsertSw,
        RfkillAllSw,
        RadioSw,
        MicrophoneInsertSw,
        DockSw,
        LineoutInsertSw,
        JackPhysicalInsertSw,
        VideooutInsertSw,
        CameraLensCoverSw,
        KeypadSlideSw,
        FrontProximitySw,
        RotateLockSw,
        LineinInsertSw,
        MuteDeviceSw,
        PenInsertedSw
        ),
    encodeSw,
    Msc
      ( SerialMsc,
        PulseledMsc,
        GestureMsc,
        RawMsc,
        ScanMsc,
        TimestampMsc
        ),
    Led
      ( NumlLed,
        CapslLed,
        ScrolllLed,
        ComposeLed,
        KanaLed,
        SleepLed,
        SuspendLed,
        MuteLed,
        MiscLed,
        MailLed,
        ChargingLed
        ),
    Rep
      ( DelayRep,
        PeriodRep
        ),
    Snd
      ( ClickSnd,
        BellSnd,
        ToneSnd
        )
    )
where

import Data.Monoid (First (First, getFirst))
import Data.Word (Word16)
import qualified Language.C.Inline as C
import Prelude hiding (abs)

C.include "<stdint.h>"

C.include "<linux/input.h>"

data Ev
  = SynEv Syn
  | KeyEv Key
  | RelEv Rel
  | AbsEv Abs
  | MscEv Msc
  | SwEv Sw
  | LedEv Led
  | SndEv Snd
  | RepEv Rep
  | FfEv
  | PwrEv
  | FfStatusEv

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
  | MiscBtn
  | ZeroBtn
  | OneBtn
  | TwoBtn
  | ThreeBtn
  | FourBtn
  | FiveBtn
  | SixBtn
  | SevenBtn
  | EightBtn
  | NineBtn
  | MouseBtn
  | LeftBtn
  | RightBtn
  | MiddleBtn
  | SideBtn
  | ExtraBtn
  | ForwardBtn
  | BackBtn
  | TaskBtn
  | JoystickBtn
  | TriggerBtn
  | ThumbBtn
  | Thumb2Btn
  | TopBtn
  | Top2Btn
  | PinkieBtn
  | BaseBtn
  | Base2Btn
  | Base3Btn
  | Base4Btn
  | Base5Btn
  | Base6Btn
  | DeadBtn
  | GamepadBtn
  | SouthBtn
  | ABtn
  | EastBtn
  | BBtn
  | CBtn
  | NorthBtn
  | XBtn
  | WestBtn
  | YBtn
  | ZBtn
  | TlBtn
  | TrBtn
  | Tl2Btn
  | Tr2Btn
  | SelectBtn
  | StartBtn
  | ModeBtn
  | ThumblBtn
  | ThumbrBtn
  | DigiBtn
  | ToolPenBtn
  | ToolRubberBtn
  | ToolBrushBtn
  | ToolPencilBtn
  | ToolAirbrushBtn
  | ToolFingerBtn
  | ToolMouseBtn
  | ToolLensBtn
  | ToolQuinttapBtn
  | Stylus3Btn
  | TouchBtn
  | StylusBtn
  | Stylus2Btn
  | ToolDoubletapBtn
  | ToolTripletapBtn
  | ToolQuadtapBtn
  | WheelBtn
  | GearDownBtn
  | GearUpBtn
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
  | ZoomKey
  | ModeKey
  | KeyboardKey
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
  | TenChannelsupKey
  | TenChannelsdownKey
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
  | DpadUpBtn
  | DpadDownBtn
  | DpadLeftBtn
  | DpadRightBtn
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
  | ThreeDModeKey
  | NextFavoriteKey
  | StopRecordKey
  | PauseRecordKey
  | VodKey
  | UnmuteKey
  | FastreverseKey
  | SlowreverseKey
  | DataKey
  | OnscreenKeyboardKey
  | TriggerHappyBtn
  | TriggerHappy1Btn
  | TriggerHappy2Btn
  | TriggerHappy3Btn
  | TriggerHappy4Btn
  | TriggerHappy5Btn
  | TriggerHappy6Btn
  | TriggerHappy7Btn
  | TriggerHappy8Btn
  | TriggerHappy9Btn
  | TriggerHappy10Btn
  | TriggerHappy11Btn
  | TriggerHappy12Btn
  | TriggerHappy13Btn
  | TriggerHappy14Btn
  | TriggerHappy15Btn
  | TriggerHappy16Btn
  | TriggerHappy17Btn
  | TriggerHappy18Btn
  | TriggerHappy19Btn
  | TriggerHappy20Btn
  | TriggerHappy21Btn
  | TriggerHappy22Btn
  | TriggerHappy23Btn
  | TriggerHappy24Btn
  | TriggerHappy25Btn
  | TriggerHappy26Btn
  | TriggerHappy27Btn
  | TriggerHappy28Btn
  | TriggerHappy29Btn
  | TriggerHappy30Btn
  | TriggerHappy31Btn
  | TriggerHappy32Btn
  | TriggerHappy33Btn
  | TriggerHappy34Btn
  | TriggerHappy35Btn
  | TriggerHappy36Btn
  | TriggerHappy37Btn
  | TriggerHappy38Btn
  | TriggerHappy39Btn
  | TriggerHappy40Btn
  deriving (Bounded, Enum)

decodeKey :: Word16 -> Maybe Key
decodeKey code = getFirst (foldMap (First . match) [minBound .. maxBound])
  where
    match key
      | encodeKey key == code = Just key
      | otherwise = Nothing

encodeKey :: Key -> Word16
encodeKey ReservedKey = [C.pure| uint16_t { KEY_RESERVED } |]
encodeKey EscKey = [C.pure| uint16_t { KEY_ESC } |]
encodeKey OneKey = [C.pure| uint16_t { KEY_1 } |]
encodeKey TwoKey = [C.pure| uint16_t { KEY_2 } |]
encodeKey ThreeKey = [C.pure| uint16_t { KEY_3 } |]
encodeKey FourKey = [C.pure| uint16_t { KEY_4 } |]
encodeKey FiveKey = [C.pure| uint16_t { KEY_5 } |]
encodeKey SixKey = [C.pure| uint16_t { KEY_6 } |]
encodeKey SevenKey = [C.pure| uint16_t { KEY_7 } |]
encodeKey EightKey = [C.pure| uint16_t { KEY_8 } |]
encodeKey NineKey = [C.pure| uint16_t { KEY_9 } |]
encodeKey ZeroKey = [C.pure| uint16_t { KEY_0 } |]
encodeKey MinusKey = [C.pure| uint16_t { KEY_MINUS } |]
encodeKey EqualKey = [C.pure| uint16_t { KEY_EQUAL } |]
encodeKey BackspaceKey = [C.pure| uint16_t { KEY_BACKSPACE } |]
encodeKey TabKey = [C.pure| uint16_t { KEY_TAB } |]
encodeKey QKey = [C.pure| uint16_t { KEY_Q } |]
encodeKey WKey = [C.pure| uint16_t { KEY_W } |]
encodeKey EKey = [C.pure| uint16_t { KEY_E } |]
encodeKey RKey = [C.pure| uint16_t { KEY_R } |]
encodeKey TKey = [C.pure| uint16_t { KEY_T } |]
encodeKey YKey = [C.pure| uint16_t { KEY_Y } |]
encodeKey UKey = [C.pure| uint16_t { KEY_U } |]
encodeKey IKey = [C.pure| uint16_t { KEY_I } |]
encodeKey OKey = [C.pure| uint16_t { KEY_O } |]
encodeKey PKey = [C.pure| uint16_t { KEY_P } |]
encodeKey LeftbraceKey = [C.pure| uint16_t { KEY_LEFTBRACE } |]
encodeKey RightbraceKey = [C.pure| uint16_t { KEY_RIGHTBRACE } |]
encodeKey EnterKey = [C.pure| uint16_t { KEY_ENTER } |]
encodeKey LeftctrlKey = [C.pure| uint16_t { KEY_LEFTCTRL } |]
encodeKey AKey = [C.pure| uint16_t { KEY_A } |]
encodeKey SKey = [C.pure| uint16_t { KEY_S } |]
encodeKey DKey = [C.pure| uint16_t { KEY_D } |]
encodeKey FKey = [C.pure| uint16_t { KEY_F } |]
encodeKey GKey = [C.pure| uint16_t { KEY_G } |]
encodeKey HKey = [C.pure| uint16_t { KEY_H } |]
encodeKey JKey = [C.pure| uint16_t { KEY_J } |]
encodeKey KKey = [C.pure| uint16_t { KEY_K } |]
encodeKey LKey = [C.pure| uint16_t { KEY_L } |]
encodeKey SemicolonKey = [C.pure| uint16_t { KEY_SEMICOLON } |]
encodeKey ApostropheKey = [C.pure| uint16_t { KEY_APOSTROPHE } |]
encodeKey GraveKey = [C.pure| uint16_t { KEY_GRAVE } |]
encodeKey LeftshiftKey = [C.pure| uint16_t { KEY_LEFTSHIFT } |]
encodeKey BackslashKey = [C.pure| uint16_t { KEY_BACKSLASH } |]
encodeKey ZKey = [C.pure| uint16_t { KEY_Z } |]
encodeKey XKey = [C.pure| uint16_t { KEY_X } |]
encodeKey CKey = [C.pure| uint16_t { KEY_C } |]
encodeKey VKey = [C.pure| uint16_t { KEY_V } |]
encodeKey BKey = [C.pure| uint16_t { KEY_B } |]
encodeKey NKey = [C.pure| uint16_t { KEY_N } |]
encodeKey MKey = [C.pure| uint16_t { KEY_M } |]
encodeKey CommaKey = [C.pure| uint16_t { KEY_COMMA } |]
encodeKey DotKey = [C.pure| uint16_t { KEY_DOT } |]
encodeKey SlashKey = [C.pure| uint16_t { KEY_SLASH } |]
encodeKey RightshiftKey = [C.pure| uint16_t { KEY_RIGHTSHIFT } |]
encodeKey KpasteriskKey = [C.pure| uint16_t { KEY_KPASTERISK } |]
encodeKey LeftaltKey = [C.pure| uint16_t { KEY_LEFTALT } |]
encodeKey SpaceKey = [C.pure| uint16_t { KEY_SPACE } |]
encodeKey CapslockKey = [C.pure| uint16_t { KEY_CAPSLOCK } |]
encodeKey F1Key = [C.pure| uint16_t { KEY_F1 } |]
encodeKey F2Key = [C.pure| uint16_t { KEY_F2 } |]
encodeKey F3Key = [C.pure| uint16_t { KEY_F3 } |]
encodeKey F4Key = [C.pure| uint16_t { KEY_F4 } |]
encodeKey F5Key = [C.pure| uint16_t { KEY_F5 } |]
encodeKey F6Key = [C.pure| uint16_t { KEY_F6 } |]
encodeKey F7Key = [C.pure| uint16_t { KEY_F7 } |]
encodeKey F8Key = [C.pure| uint16_t { KEY_F8 } |]
encodeKey F9Key = [C.pure| uint16_t { KEY_F9 } |]
encodeKey F10Key = [C.pure| uint16_t { KEY_F10 } |]
encodeKey NumlockKey = [C.pure| uint16_t { KEY_NUMLOCK } |]
encodeKey ScrolllockKey = [C.pure| uint16_t { KEY_SCROLLLOCK } |]
encodeKey Kp7Key = [C.pure| uint16_t { KEY_KP7 } |]
encodeKey Kp8Key = [C.pure| uint16_t { KEY_KP8 } |]
encodeKey Kp9Key = [C.pure| uint16_t { KEY_KP9 } |]
encodeKey KpminusKey = [C.pure| uint16_t { KEY_KPMINUS } |]
encodeKey Kp4Key = [C.pure| uint16_t { KEY_KP4 } |]
encodeKey Kp5Key = [C.pure| uint16_t { KEY_KP5 } |]
encodeKey Kp6Key = [C.pure| uint16_t { KEY_KP6 } |]
encodeKey KpplusKey = [C.pure| uint16_t { KEY_KPPLUS } |]
encodeKey Kp1Key = [C.pure| uint16_t { KEY_KP1 } |]
encodeKey Kp2Key = [C.pure| uint16_t { KEY_KP2 } |]
encodeKey Kp3Key = [C.pure| uint16_t { KEY_KP3 } |]
encodeKey Kp0Key = [C.pure| uint16_t { KEY_KP0 } |]
encodeKey KpdotKey = [C.pure| uint16_t { KEY_KPDOT } |]
encodeKey ZenkakuhankakuKey = [C.pure| uint16_t { KEY_ZENKAKUHANKAKU } |]
encodeKey OneZeroTwoNdKey = [C.pure| uint16_t { KEY_102ND } |]
encodeKey F11Key = [C.pure| uint16_t { KEY_F11 } |]
encodeKey F12Key = [C.pure| uint16_t { KEY_F12 } |]
encodeKey RoKey = [C.pure| uint16_t { KEY_RO } |]
encodeKey KatakanaKey = [C.pure| uint16_t { KEY_KATAKANA } |]
encodeKey HiraganaKey = [C.pure| uint16_t { KEY_HIRAGANA } |]
encodeKey HenkanKey = [C.pure| uint16_t { KEY_HENKAN } |]
encodeKey KatakanahiraganaKey = [C.pure| uint16_t { KEY_KATAKANAHIRAGANA } |]
encodeKey MuhenkanKey = [C.pure| uint16_t { KEY_MUHENKAN } |]
encodeKey KpjpcommaKey = [C.pure| uint16_t { KEY_KPJPCOMMA } |]
encodeKey KpenterKey = [C.pure| uint16_t { KEY_KPENTER } |]
encodeKey RightctrlKey = [C.pure| uint16_t { KEY_RIGHTCTRL } |]
encodeKey KpslashKey = [C.pure| uint16_t { KEY_KPSLASH } |]
encodeKey SysrqKey = [C.pure| uint16_t { KEY_SYSRQ } |]
encodeKey RightaltKey = [C.pure| uint16_t { KEY_RIGHTALT } |]
encodeKey LinefeedKey = [C.pure| uint16_t { KEY_LINEFEED } |]
encodeKey HomeKey = [C.pure| uint16_t { KEY_HOME } |]
encodeKey UpKey = [C.pure| uint16_t { KEY_UP } |]
encodeKey PageupKey = [C.pure| uint16_t { KEY_PAGEUP } |]
encodeKey LeftKey = [C.pure| uint16_t { KEY_LEFT } |]
encodeKey RightKey = [C.pure| uint16_t { KEY_RIGHT } |]
encodeKey EndKey = [C.pure| uint16_t { KEY_END } |]
encodeKey DownKey = [C.pure| uint16_t { KEY_DOWN } |]
encodeKey PagedownKey = [C.pure| uint16_t { KEY_PAGEDOWN } |]
encodeKey InsertKey = [C.pure| uint16_t { KEY_INSERT } |]
encodeKey DeleteKey = [C.pure| uint16_t { KEY_DELETE } |]
encodeKey MacroKey = [C.pure| uint16_t { KEY_MACRO } |]
encodeKey MuteKey = [C.pure| uint16_t { KEY_MUTE } |]
encodeKey VolumedownKey = [C.pure| uint16_t { KEY_VOLUMEDOWN } |]
encodeKey VolumeupKey = [C.pure| uint16_t { KEY_VOLUMEUP } |]
encodeKey PowerKey = [C.pure| uint16_t { KEY_POWER } |]
encodeKey KpequalKey = [C.pure| uint16_t { KEY_KPEQUAL } |]
encodeKey KpplusminusKey = [C.pure| uint16_t { KEY_KPPLUSMINUS } |]
encodeKey PauseKey = [C.pure| uint16_t { KEY_PAUSE } |]
encodeKey ScaleKey = [C.pure| uint16_t { KEY_SCALE } |]
encodeKey KpcommaKey = [C.pure| uint16_t { KEY_KPCOMMA } |]
encodeKey HangeulKey = [C.pure| uint16_t { KEY_HANGEUL } |]
encodeKey HanguelKey = [C.pure| uint16_t { KEY_HANGUEL } |]
encodeKey HanjaKey = [C.pure| uint16_t { KEY_HANJA } |]
encodeKey YenKey = [C.pure| uint16_t { KEY_YEN } |]
encodeKey LeftmetaKey = [C.pure| uint16_t { KEY_LEFTMETA } |]
encodeKey RightmetaKey = [C.pure| uint16_t { KEY_RIGHTMETA } |]
encodeKey ComposeKey = [C.pure| uint16_t { KEY_COMPOSE } |]
encodeKey StopKey = [C.pure| uint16_t { KEY_STOP } |]
encodeKey AgainKey = [C.pure| uint16_t { KEY_AGAIN } |]
encodeKey PropsKey = [C.pure| uint16_t { KEY_PROPS } |]
encodeKey UndoKey = [C.pure| uint16_t { KEY_UNDO } |]
encodeKey FrontKey = [C.pure| uint16_t { KEY_FRONT } |]
encodeKey CopyKey = [C.pure| uint16_t { KEY_COPY } |]
encodeKey OpenKey = [C.pure| uint16_t { KEY_OPEN } |]
encodeKey PasteKey = [C.pure| uint16_t { KEY_PASTE } |]
encodeKey FindKey = [C.pure| uint16_t { KEY_FIND } |]
encodeKey CutKey = [C.pure| uint16_t { KEY_CUT } |]
encodeKey HelpKey = [C.pure| uint16_t { KEY_HELP } |]
encodeKey MenuKey = [C.pure| uint16_t { KEY_MENU } |]
encodeKey CalcKey = [C.pure| uint16_t { KEY_CALC } |]
encodeKey SetupKey = [C.pure| uint16_t { KEY_SETUP } |]
encodeKey SleepKey = [C.pure| uint16_t { KEY_SLEEP } |]
encodeKey WakeupKey = [C.pure| uint16_t { KEY_WAKEUP } |]
encodeKey FileKey = [C.pure| uint16_t { KEY_FILE } |]
encodeKey SendfileKey = [C.pure| uint16_t { KEY_SENDFILE } |]
encodeKey DeletefileKey = [C.pure| uint16_t { KEY_DELETEFILE } |]
encodeKey XferKey = [C.pure| uint16_t { KEY_XFER } |]
encodeKey Prog1Key = [C.pure| uint16_t { KEY_PROG1 } |]
encodeKey Prog2Key = [C.pure| uint16_t { KEY_PROG2 } |]
encodeKey WwwKey = [C.pure| uint16_t { KEY_WWW } |]
encodeKey MsdosKey = [C.pure| uint16_t { KEY_MSDOS } |]
encodeKey CoffeeKey = [C.pure| uint16_t { KEY_COFFEE } |]
encodeKey ScreenlockKey = [C.pure| uint16_t { KEY_SCREENLOCK } |]
encodeKey RotateDisplayKey = [C.pure| uint16_t { KEY_ROTATE_DISPLAY } |]
encodeKey DirectionKey = [C.pure| uint16_t { KEY_DIRECTION } |]
encodeKey CyclewindowsKey = [C.pure| uint16_t { KEY_CYCLEWINDOWS } |]
encodeKey MailKey = [C.pure| uint16_t { KEY_MAIL } |]
encodeKey BookmarksKey = [C.pure| uint16_t { KEY_BOOKMARKS } |]
encodeKey ComputerKey = [C.pure| uint16_t { KEY_COMPUTER } |]
encodeKey BackKey = [C.pure| uint16_t { KEY_BACK } |]
encodeKey ForwardKey = [C.pure| uint16_t { KEY_FORWARD } |]
encodeKey ClosecdKey = [C.pure| uint16_t { KEY_CLOSECD } |]
encodeKey EjectcdKey = [C.pure| uint16_t { KEY_EJECTCD } |]
encodeKey EjectclosecdKey = [C.pure| uint16_t { KEY_EJECTCLOSECD } |]
encodeKey NextsongKey = [C.pure| uint16_t { KEY_NEXTSONG } |]
encodeKey PlaypauseKey = [C.pure| uint16_t { KEY_PLAYPAUSE } |]
encodeKey PrevioussongKey = [C.pure| uint16_t { KEY_PREVIOUSSONG } |]
encodeKey StopcdKey = [C.pure| uint16_t { KEY_STOPCD } |]
encodeKey RecordKey = [C.pure| uint16_t { KEY_RECORD } |]
encodeKey RewindKey = [C.pure| uint16_t { KEY_REWIND } |]
encodeKey PhoneKey = [C.pure| uint16_t { KEY_PHONE } |]
encodeKey IsoKey = [C.pure| uint16_t { KEY_ISO } |]
encodeKey ConfigKey = [C.pure| uint16_t { KEY_CONFIG } |]
encodeKey HomepageKey = [C.pure| uint16_t { KEY_HOMEPAGE } |]
encodeKey RefreshKey = [C.pure| uint16_t { KEY_REFRESH } |]
encodeKey ExitKey = [C.pure| uint16_t { KEY_EXIT } |]
encodeKey MoveKey = [C.pure| uint16_t { KEY_MOVE } |]
encodeKey EditKey = [C.pure| uint16_t { KEY_EDIT } |]
encodeKey ScrollupKey = [C.pure| uint16_t { KEY_SCROLLUP } |]
encodeKey ScrolldownKey = [C.pure| uint16_t { KEY_SCROLLDOWN } |]
encodeKey KpleftparenKey = [C.pure| uint16_t { KEY_KPLEFTPAREN } |]
encodeKey KprightparenKey = [C.pure| uint16_t { KEY_KPRIGHTPAREN } |]
encodeKey NewKey = [C.pure| uint16_t { KEY_NEW } |]
encodeKey RedoKey = [C.pure| uint16_t { KEY_REDO } |]
encodeKey F13Key = [C.pure| uint16_t { KEY_F13 } |]
encodeKey F14Key = [C.pure| uint16_t { KEY_F14 } |]
encodeKey F15Key = [C.pure| uint16_t { KEY_F15 } |]
encodeKey F16Key = [C.pure| uint16_t { KEY_F16 } |]
encodeKey F17Key = [C.pure| uint16_t { KEY_F17 } |]
encodeKey F18Key = [C.pure| uint16_t { KEY_F18 } |]
encodeKey F19Key = [C.pure| uint16_t { KEY_F19 } |]
encodeKey F20Key = [C.pure| uint16_t { KEY_F20 } |]
encodeKey F21Key = [C.pure| uint16_t { KEY_F21 } |]
encodeKey F22Key = [C.pure| uint16_t { KEY_F22 } |]
encodeKey F23Key = [C.pure| uint16_t { KEY_F23 } |]
encodeKey F24Key = [C.pure| uint16_t { KEY_F24 } |]
encodeKey PlaycdKey = [C.pure| uint16_t { KEY_PLAYCD } |]
encodeKey PausecdKey = [C.pure| uint16_t { KEY_PAUSECD } |]
encodeKey Prog3Key = [C.pure| uint16_t { KEY_PROG3 } |]
encodeKey Prog4Key = [C.pure| uint16_t { KEY_PROG4 } |]
encodeKey DashboardKey = [C.pure| uint16_t { KEY_DASHBOARD } |]
encodeKey SuspendKey = [C.pure| uint16_t { KEY_SUSPEND } |]
encodeKey CloseKey = [C.pure| uint16_t { KEY_CLOSE } |]
encodeKey PlayKey = [C.pure| uint16_t { KEY_PLAY } |]
encodeKey FastforwardKey = [C.pure| uint16_t { KEY_FASTFORWARD } |]
encodeKey BassboostKey = [C.pure| uint16_t { KEY_BASSBOOST } |]
encodeKey PrintKey = [C.pure| uint16_t { KEY_PRINT } |]
encodeKey HpKey = [C.pure| uint16_t { KEY_HP } |]
encodeKey CameraKey = [C.pure| uint16_t { KEY_CAMERA } |]
encodeKey SoundKey = [C.pure| uint16_t { KEY_SOUND } |]
encodeKey QuestionKey = [C.pure| uint16_t { KEY_QUESTION } |]
encodeKey EmailKey = [C.pure| uint16_t { KEY_EMAIL } |]
encodeKey ChatKey = [C.pure| uint16_t { KEY_CHAT } |]
encodeKey SearchKey = [C.pure| uint16_t { KEY_SEARCH } |]
encodeKey ConnectKey = [C.pure| uint16_t { KEY_CONNECT } |]
encodeKey FinanceKey = [C.pure| uint16_t { KEY_FINANCE } |]
encodeKey SportKey = [C.pure| uint16_t { KEY_SPORT } |]
encodeKey ShopKey = [C.pure| uint16_t { KEY_SHOP } |]
encodeKey AlteraseKey = [C.pure| uint16_t { KEY_ALTERASE } |]
encodeKey CancelKey = [C.pure| uint16_t { KEY_CANCEL } |]
encodeKey BrightnessdownKey = [C.pure| uint16_t { KEY_BRIGHTNESSDOWN } |]
encodeKey BrightnessupKey = [C.pure| uint16_t { KEY_BRIGHTNESSUP } |]
encodeKey MediaKey = [C.pure| uint16_t { KEY_MEDIA } |]
encodeKey SwitchvideomodeKey = [C.pure| uint16_t { KEY_SWITCHVIDEOMODE } |]
encodeKey KbdillumtoggleKey = [C.pure| uint16_t { KEY_KBDILLUMTOGGLE } |]
encodeKey KbdillumdownKey = [C.pure| uint16_t { KEY_KBDILLUMDOWN } |]
encodeKey KbdillumupKey = [C.pure| uint16_t { KEY_KBDILLUMUP } |]
encodeKey SendKey = [C.pure| uint16_t { KEY_SEND } |]
encodeKey ReplyKey = [C.pure| uint16_t { KEY_REPLY } |]
encodeKey ForwardmailKey = [C.pure| uint16_t { KEY_FORWARDMAIL } |]
encodeKey SaveKey = [C.pure| uint16_t { KEY_SAVE } |]
encodeKey DocumentsKey = [C.pure| uint16_t { KEY_DOCUMENTS } |]
encodeKey BatteryKey = [C.pure| uint16_t { KEY_BATTERY } |]
encodeKey BluetoothKey = [C.pure| uint16_t { KEY_BLUETOOTH } |]
encodeKey WlanKey = [C.pure| uint16_t { KEY_WLAN } |]
encodeKey UwbKey = [C.pure| uint16_t { KEY_UWB } |]
encodeKey UnknownKey = [C.pure| uint16_t { KEY_UNKNOWN } |]
encodeKey VideoNextKey = [C.pure| uint16_t { KEY_VIDEO_NEXT } |]
encodeKey VideoPrevKey = [C.pure| uint16_t { KEY_VIDEO_PREV } |]
encodeKey BrightnessCycleKey = [C.pure| uint16_t { KEY_BRIGHTNESS_CYCLE } |]
encodeKey BrightnessAutoKey = [C.pure| uint16_t { KEY_BRIGHTNESS_AUTO } |]
encodeKey BrightnessZeroKey = [C.pure| uint16_t { KEY_BRIGHTNESS_ZERO } |]
encodeKey DisplayOffKey = [C.pure| uint16_t { KEY_DISPLAY_OFF } |]
encodeKey WwanKey = [C.pure| uint16_t { KEY_WWAN } |]
encodeKey WimaxKey = [C.pure| uint16_t { KEY_WIMAX } |]
encodeKey RfkillKey = [C.pure| uint16_t { KEY_RFKILL } |]
encodeKey MicmuteKey = [C.pure| uint16_t { KEY_MICMUTE } |]
encodeKey MiscBtn = [C.pure| uint16_t { BTN_MISC } |]
encodeKey ZeroBtn = [C.pure| uint16_t { BTN_0 } |]
encodeKey OneBtn = [C.pure| uint16_t { BTN_1 } |]
encodeKey TwoBtn = [C.pure| uint16_t { BTN_2 } |]
encodeKey ThreeBtn = [C.pure| uint16_t { BTN_3 } |]
encodeKey FourBtn = [C.pure| uint16_t { BTN_4 } |]
encodeKey FiveBtn = [C.pure| uint16_t { BTN_5 } |]
encodeKey SixBtn = [C.pure| uint16_t { BTN_6 } |]
encodeKey SevenBtn = [C.pure| uint16_t { BTN_7 } |]
encodeKey EightBtn = [C.pure| uint16_t { BTN_8 } |]
encodeKey NineBtn = [C.pure| uint16_t { BTN_9 } |]
encodeKey MouseBtn = [C.pure| uint16_t { BTN_MOUSE } |]
encodeKey LeftBtn = [C.pure| uint16_t { BTN_LEFT } |]
encodeKey RightBtn = [C.pure| uint16_t { BTN_RIGHT } |]
encodeKey MiddleBtn = [C.pure| uint16_t { BTN_MIDDLE } |]
encodeKey SideBtn = [C.pure| uint16_t { BTN_SIDE } |]
encodeKey ExtraBtn = [C.pure| uint16_t { BTN_EXTRA } |]
encodeKey ForwardBtn = [C.pure| uint16_t { BTN_FORWARD } |]
encodeKey BackBtn = [C.pure| uint16_t { BTN_BACK } |]
encodeKey TaskBtn = [C.pure| uint16_t { BTN_TASK } |]
encodeKey JoystickBtn = [C.pure| uint16_t { BTN_JOYSTICK } |]
encodeKey TriggerBtn = [C.pure| uint16_t { BTN_TRIGGER } |]
encodeKey ThumbBtn = [C.pure| uint16_t { BTN_THUMB } |]
encodeKey Thumb2Btn = [C.pure| uint16_t { BTN_THUMB2 } |]
encodeKey TopBtn = [C.pure| uint16_t { BTN_TOP } |]
encodeKey Top2Btn = [C.pure| uint16_t { BTN_TOP2 } |]
encodeKey PinkieBtn = [C.pure| uint16_t { BTN_PINKIE } |]
encodeKey BaseBtn = [C.pure| uint16_t { BTN_BASE } |]
encodeKey Base2Btn = [C.pure| uint16_t { BTN_BASE2 } |]
encodeKey Base3Btn = [C.pure| uint16_t { BTN_BASE3 } |]
encodeKey Base4Btn = [C.pure| uint16_t { BTN_BASE4 } |]
encodeKey Base5Btn = [C.pure| uint16_t { BTN_BASE5 } |]
encodeKey Base6Btn = [C.pure| uint16_t { BTN_BASE6 } |]
encodeKey DeadBtn = [C.pure| uint16_t { BTN_DEAD } |]
encodeKey GamepadBtn = [C.pure| uint16_t { BTN_GAMEPAD } |]
encodeKey SouthBtn = [C.pure| uint16_t { BTN_SOUTH } |]
encodeKey ABtn = [C.pure| uint16_t { BTN_A } |]
encodeKey EastBtn = [C.pure| uint16_t { BTN_EAST } |]
encodeKey BBtn = [C.pure| uint16_t { BTN_B } |]
encodeKey CBtn = [C.pure| uint16_t { BTN_C } |]
encodeKey NorthBtn = [C.pure| uint16_t { BTN_NORTH } |]
encodeKey XBtn = [C.pure| uint16_t { BTN_X } |]
encodeKey WestBtn = [C.pure| uint16_t { BTN_WEST } |]
encodeKey YBtn = [C.pure| uint16_t { BTN_Y } |]
encodeKey ZBtn = [C.pure| uint16_t { BTN_Z } |]
encodeKey TlBtn = [C.pure| uint16_t { BTN_TL } |]
encodeKey TrBtn = [C.pure| uint16_t { BTN_TR } |]
encodeKey Tl2Btn = [C.pure| uint16_t { BTN_TL2 } |]
encodeKey Tr2Btn = [C.pure| uint16_t { BTN_TR2 } |]
encodeKey SelectBtn = [C.pure| uint16_t { BTN_SELECT } |]
encodeKey StartBtn = [C.pure| uint16_t { BTN_START } |]
encodeKey ModeBtn = [C.pure| uint16_t { BTN_MODE } |]
encodeKey ThumblBtn = [C.pure| uint16_t { BTN_THUMBL } |]
encodeKey ThumbrBtn = [C.pure| uint16_t { BTN_THUMBR } |]
encodeKey DigiBtn = [C.pure| uint16_t { BTN_DIGI } |]
encodeKey ToolPenBtn = [C.pure| uint16_t { BTN_TOOL_PEN } |]
encodeKey ToolRubberBtn = [C.pure| uint16_t { BTN_TOOL_RUBBER } |]
encodeKey ToolBrushBtn = [C.pure| uint16_t { BTN_TOOL_BRUSH } |]
encodeKey ToolPencilBtn = [C.pure| uint16_t { BTN_TOOL_PENCIL } |]
encodeKey ToolAirbrushBtn = [C.pure| uint16_t { BTN_TOOL_AIRBRUSH } |]
encodeKey ToolFingerBtn = [C.pure| uint16_t { BTN_TOOL_FINGER } |]
encodeKey ToolMouseBtn = [C.pure| uint16_t { BTN_TOOL_MOUSE } |]
encodeKey ToolLensBtn = [C.pure| uint16_t { BTN_TOOL_LENS } |]
encodeKey ToolQuinttapBtn = [C.pure| uint16_t { BTN_TOOL_QUINTTAP } |]
encodeKey Stylus3Btn = [C.pure| uint16_t { BTN_STYLUS3 } |]
encodeKey TouchBtn = [C.pure| uint16_t { BTN_TOUCH } |]
encodeKey StylusBtn = [C.pure| uint16_t { BTN_STYLUS } |]
encodeKey Stylus2Btn = [C.pure| uint16_t { BTN_STYLUS2 } |]
encodeKey ToolDoubletapBtn = [C.pure| uint16_t { BTN_TOOL_DOUBLETAP } |]
encodeKey ToolTripletapBtn = [C.pure| uint16_t { BTN_TOOL_TRIPLETAP } |]
encodeKey ToolQuadtapBtn = [C.pure| uint16_t { BTN_TOOL_QUADTAP } |]
encodeKey WheelBtn = [C.pure| uint16_t { BTN_WHEEL } |]
encodeKey GearDownBtn = [C.pure| uint16_t { BTN_GEAR_DOWN } |]
encodeKey GearUpBtn = [C.pure| uint16_t { BTN_GEAR_UP } |]
encodeKey OkKey = [C.pure| uint16_t { KEY_OK } |]
encodeKey SelectKey = [C.pure| uint16_t { KEY_SELECT } |]
encodeKey GotoKey = [C.pure| uint16_t { KEY_GOTO } |]
encodeKey ClearKey = [C.pure| uint16_t { KEY_CLEAR } |]
encodeKey Power2Key = [C.pure| uint16_t { KEY_POWER2 } |]
encodeKey OptionKey = [C.pure| uint16_t { KEY_OPTION } |]
encodeKey InfoKey = [C.pure| uint16_t { KEY_INFO } |]
encodeKey TimeKey = [C.pure| uint16_t { KEY_TIME } |]
encodeKey VendorKey = [C.pure| uint16_t { KEY_VENDOR } |]
encodeKey ArchiveKey = [C.pure| uint16_t { KEY_ARCHIVE } |]
encodeKey ProgramKey = [C.pure| uint16_t { KEY_PROGRAM } |]
encodeKey ChannelKey = [C.pure| uint16_t { KEY_CHANNEL } |]
encodeKey FavoritesKey = [C.pure| uint16_t { KEY_FAVORITES } |]
encodeKey EpgKey = [C.pure| uint16_t { KEY_EPG } |]
encodeKey PvrKey = [C.pure| uint16_t { KEY_PVR } |]
encodeKey MhpKey = [C.pure| uint16_t { KEY_MHP } |]
encodeKey LanguageKey = [C.pure| uint16_t { KEY_LANGUAGE } |]
encodeKey TitleKey = [C.pure| uint16_t { KEY_TITLE } |]
encodeKey SubtitleKey = [C.pure| uint16_t { KEY_SUBTITLE } |]
encodeKey AngleKey = [C.pure| uint16_t { KEY_ANGLE } |]
encodeKey ZoomKey = [C.pure| uint16_t { KEY_ZOOM } |]
encodeKey ModeKey = [C.pure| uint16_t { KEY_MODE } |]
encodeKey KeyboardKey = [C.pure| uint16_t { KEY_KEYBOARD } |]
encodeKey ScreenKey = [C.pure| uint16_t { KEY_SCREEN } |]
encodeKey PcKey = [C.pure| uint16_t { KEY_PC } |]
encodeKey TvKey = [C.pure| uint16_t { KEY_TV } |]
encodeKey Tv2Key = [C.pure| uint16_t { KEY_TV2 } |]
encodeKey VcrKey = [C.pure| uint16_t { KEY_VCR } |]
encodeKey Vcr2Key = [C.pure| uint16_t { KEY_VCR2 } |]
encodeKey SatKey = [C.pure| uint16_t { KEY_SAT } |]
encodeKey Sat2Key = [C.pure| uint16_t { KEY_SAT2 } |]
encodeKey CdKey = [C.pure| uint16_t { KEY_CD } |]
encodeKey TapeKey = [C.pure| uint16_t { KEY_TAPE } |]
encodeKey RadioKey = [C.pure| uint16_t { KEY_RADIO } |]
encodeKey TunerKey = [C.pure| uint16_t { KEY_TUNER } |]
encodeKey PlayerKey = [C.pure| uint16_t { KEY_PLAYER } |]
encodeKey TextKey = [C.pure| uint16_t { KEY_TEXT } |]
encodeKey DvdKey = [C.pure| uint16_t { KEY_DVD } |]
encodeKey AuxKey = [C.pure| uint16_t { KEY_AUX } |]
encodeKey Mp3Key = [C.pure| uint16_t { KEY_MP3 } |]
encodeKey AudioKey = [C.pure| uint16_t { KEY_AUDIO } |]
encodeKey VideoKey = [C.pure| uint16_t { KEY_VIDEO } |]
encodeKey DirectoryKey = [C.pure| uint16_t { KEY_DIRECTORY } |]
encodeKey ListKey = [C.pure| uint16_t { KEY_LIST } |]
encodeKey MemoKey = [C.pure| uint16_t { KEY_MEMO } |]
encodeKey CalendarKey = [C.pure| uint16_t { KEY_CALENDAR } |]
encodeKey RedKey = [C.pure| uint16_t { KEY_RED } |]
encodeKey GreenKey = [C.pure| uint16_t { KEY_GREEN } |]
encodeKey YellowKey = [C.pure| uint16_t { KEY_YELLOW } |]
encodeKey BlueKey = [C.pure| uint16_t { KEY_BLUE } |]
encodeKey ChannelupKey = [C.pure| uint16_t { KEY_CHANNELUP } |]
encodeKey ChanneldownKey = [C.pure| uint16_t { KEY_CHANNELDOWN } |]
encodeKey FirstKey = [C.pure| uint16_t { KEY_FIRST } |]
encodeKey LastKey = [C.pure| uint16_t { KEY_LAST } |]
encodeKey AbKey = [C.pure| uint16_t { KEY_AB } |]
encodeKey NextKey = [C.pure| uint16_t { KEY_NEXT } |]
encodeKey RestartKey = [C.pure| uint16_t { KEY_RESTART } |]
encodeKey SlowKey = [C.pure| uint16_t { KEY_SLOW } |]
encodeKey ShuffleKey = [C.pure| uint16_t { KEY_SHUFFLE } |]
encodeKey BreakKey = [C.pure| uint16_t { KEY_BREAK } |]
encodeKey PreviousKey = [C.pure| uint16_t { KEY_PREVIOUS } |]
encodeKey DigitsKey = [C.pure| uint16_t { KEY_DIGITS } |]
encodeKey TeenKey = [C.pure| uint16_t { KEY_TEEN } |]
encodeKey TwenKey = [C.pure| uint16_t { KEY_TWEN } |]
encodeKey VideophoneKey = [C.pure| uint16_t { KEY_VIDEOPHONE } |]
encodeKey GamesKey = [C.pure| uint16_t { KEY_GAMES } |]
encodeKey ZoominKey = [C.pure| uint16_t { KEY_ZOOMIN } |]
encodeKey ZoomoutKey = [C.pure| uint16_t { KEY_ZOOMOUT } |]
encodeKey ZoomresetKey = [C.pure| uint16_t { KEY_ZOOMRESET } |]
encodeKey WordprocessorKey = [C.pure| uint16_t { KEY_WORDPROCESSOR } |]
encodeKey EditorKey = [C.pure| uint16_t { KEY_EDITOR } |]
encodeKey SpreadsheetKey = [C.pure| uint16_t { KEY_SPREADSHEET } |]
encodeKey GraphicseditorKey = [C.pure| uint16_t { KEY_GRAPHICSEDITOR } |]
encodeKey PresentationKey = [C.pure| uint16_t { KEY_PRESENTATION } |]
encodeKey DatabaseKey = [C.pure| uint16_t { KEY_DATABASE } |]
encodeKey NewsKey = [C.pure| uint16_t { KEY_NEWS } |]
encodeKey VoicemailKey = [C.pure| uint16_t { KEY_VOICEMAIL } |]
encodeKey AddressbookKey = [C.pure| uint16_t { KEY_ADDRESSBOOK } |]
encodeKey MessengerKey = [C.pure| uint16_t { KEY_MESSENGER } |]
encodeKey DisplaytoggleKey = [C.pure| uint16_t { KEY_DISPLAYTOGGLE } |]
encodeKey BrightnessToggleKey = [C.pure| uint16_t { KEY_BRIGHTNESS_TOGGLE } |]
encodeKey SpellcheckKey = [C.pure| uint16_t { KEY_SPELLCHECK } |]
encodeKey LogoffKey = [C.pure| uint16_t { KEY_LOGOFF } |]
encodeKey DollarKey = [C.pure| uint16_t { KEY_DOLLAR } |]
encodeKey EuroKey = [C.pure| uint16_t { KEY_EURO } |]
encodeKey FramebackKey = [C.pure| uint16_t { KEY_FRAMEBACK } |]
encodeKey FrameforwardKey = [C.pure| uint16_t { KEY_FRAMEFORWARD } |]
encodeKey ContextMenuKey = [C.pure| uint16_t { KEY_CONTEXT_MENU } |]
encodeKey MediaRepeatKey = [C.pure| uint16_t { KEY_MEDIA_REPEAT } |]
encodeKey TenChannelsupKey = [C.pure| uint16_t { KEY_10CHANNELSUP } |]
encodeKey TenChannelsdownKey = [C.pure| uint16_t { KEY_10CHANNELSDOWN } |]
encodeKey ImagesKey = [C.pure| uint16_t { KEY_IMAGES } |]
encodeKey DelEolKey = [C.pure| uint16_t { KEY_DEL_EOL } |]
encodeKey DelEosKey = [C.pure| uint16_t { KEY_DEL_EOS } |]
encodeKey InsLineKey = [C.pure| uint16_t { KEY_INS_LINE } |]
encodeKey DelLineKey = [C.pure| uint16_t { KEY_DEL_LINE } |]
encodeKey FnKey = [C.pure| uint16_t { KEY_FN } |]
encodeKey FnEscKey = [C.pure| uint16_t { KEY_FN_ESC } |]
encodeKey FnF1Key = [C.pure| uint16_t { KEY_FN_F1 } |]
encodeKey FnF2Key = [C.pure| uint16_t { KEY_FN_F2 } |]
encodeKey FnF3Key = [C.pure| uint16_t { KEY_FN_F3 } |]
encodeKey FnF4Key = [C.pure| uint16_t { KEY_FN_F4 } |]
encodeKey FnF5Key = [C.pure| uint16_t { KEY_FN_F5 } |]
encodeKey FnF6Key = [C.pure| uint16_t { KEY_FN_F6 } |]
encodeKey FnF7Key = [C.pure| uint16_t { KEY_FN_F7 } |]
encodeKey FnF8Key = [C.pure| uint16_t { KEY_FN_F8 } |]
encodeKey FnF9Key = [C.pure| uint16_t { KEY_FN_F9 } |]
encodeKey FnF10Key = [C.pure| uint16_t { KEY_FN_F10 } |]
encodeKey FnF11Key = [C.pure| uint16_t { KEY_FN_F11 } |]
encodeKey FnF12Key = [C.pure| uint16_t { KEY_FN_F12 } |]
encodeKey Fn1Key = [C.pure| uint16_t { KEY_FN_1 } |]
encodeKey Fn2Key = [C.pure| uint16_t { KEY_FN_2 } |]
encodeKey FnDKey = [C.pure| uint16_t { KEY_FN_D } |]
encodeKey FnEKey = [C.pure| uint16_t { KEY_FN_E } |]
encodeKey FnFKey = [C.pure| uint16_t { KEY_FN_F } |]
encodeKey FnSKey = [C.pure| uint16_t { KEY_FN_S } |]
encodeKey FnBKey = [C.pure| uint16_t { KEY_FN_B } |]
encodeKey BrlDot1Key = [C.pure| uint16_t { KEY_BRL_DOT1 } |]
encodeKey BrlDot2Key = [C.pure| uint16_t { KEY_BRL_DOT2 } |]
encodeKey BrlDot3Key = [C.pure| uint16_t { KEY_BRL_DOT3 } |]
encodeKey BrlDot4Key = [C.pure| uint16_t { KEY_BRL_DOT4 } |]
encodeKey BrlDot5Key = [C.pure| uint16_t { KEY_BRL_DOT5 } |]
encodeKey BrlDot6Key = [C.pure| uint16_t { KEY_BRL_DOT6 } |]
encodeKey BrlDot7Key = [C.pure| uint16_t { KEY_BRL_DOT7 } |]
encodeKey BrlDot8Key = [C.pure| uint16_t { KEY_BRL_DOT8 } |]
encodeKey BrlDot9Key = [C.pure| uint16_t { KEY_BRL_DOT9 } |]
encodeKey BrlDot10Key = [C.pure| uint16_t { KEY_BRL_DOT10 } |]
encodeKey Numeric0Key = [C.pure| uint16_t { KEY_NUMERIC_0 } |]
encodeKey Numeric1Key = [C.pure| uint16_t { KEY_NUMERIC_1 } |]
encodeKey Numeric2Key = [C.pure| uint16_t { KEY_NUMERIC_2 } |]
encodeKey Numeric3Key = [C.pure| uint16_t { KEY_NUMERIC_3 } |]
encodeKey Numeric4Key = [C.pure| uint16_t { KEY_NUMERIC_4 } |]
encodeKey Numeric5Key = [C.pure| uint16_t { KEY_NUMERIC_5 } |]
encodeKey Numeric6Key = [C.pure| uint16_t { KEY_NUMERIC_6 } |]
encodeKey Numeric7Key = [C.pure| uint16_t { KEY_NUMERIC_7 } |]
encodeKey Numeric8Key = [C.pure| uint16_t { KEY_NUMERIC_8 } |]
encodeKey Numeric9Key = [C.pure| uint16_t { KEY_NUMERIC_9 } |]
encodeKey NumericStarKey = [C.pure| uint16_t { KEY_NUMERIC_STAR } |]
encodeKey NumericPoundKey = [C.pure| uint16_t { KEY_NUMERIC_POUND } |]
encodeKey NumericAKey = [C.pure| uint16_t { KEY_NUMERIC_A } |]
encodeKey NumericBKey = [C.pure| uint16_t { KEY_NUMERIC_B } |]
encodeKey NumericCKey = [C.pure| uint16_t { KEY_NUMERIC_C } |]
encodeKey NumericDKey = [C.pure| uint16_t { KEY_NUMERIC_D } |]
encodeKey CameraFocusKey = [C.pure| uint16_t { KEY_CAMERA_FOCUS } |]
encodeKey WpsButtonKey = [C.pure| uint16_t { KEY_WPS_BUTTON } |]
encodeKey TouchpadToggleKey = [C.pure| uint16_t { KEY_TOUCHPAD_TOGGLE } |]
encodeKey TouchpadOnKey = [C.pure| uint16_t { KEY_TOUCHPAD_ON } |]
encodeKey TouchpadOffKey = [C.pure| uint16_t { KEY_TOUCHPAD_OFF } |]
encodeKey CameraZoominKey = [C.pure| uint16_t { KEY_CAMERA_ZOOMIN } |]
encodeKey CameraZoomoutKey = [C.pure| uint16_t { KEY_CAMERA_ZOOMOUT } |]
encodeKey CameraUpKey = [C.pure| uint16_t { KEY_CAMERA_UP } |]
encodeKey CameraDownKey = [C.pure| uint16_t { KEY_CAMERA_DOWN } |]
encodeKey CameraLeftKey = [C.pure| uint16_t { KEY_CAMERA_LEFT } |]
encodeKey CameraRightKey = [C.pure| uint16_t { KEY_CAMERA_RIGHT } |]
encodeKey AttendantOnKey = [C.pure| uint16_t { KEY_ATTENDANT_ON } |]
encodeKey AttendantOffKey = [C.pure| uint16_t { KEY_ATTENDANT_OFF } |]
encodeKey AttendantToggleKey = [C.pure| uint16_t { KEY_ATTENDANT_TOGGLE } |]
encodeKey LightsToggleKey = [C.pure| uint16_t { KEY_LIGHTS_TOGGLE } |]
encodeKey DpadUpBtn = [C.pure| uint16_t { BTN_DPAD_UP } |]
encodeKey DpadDownBtn = [C.pure| uint16_t { BTN_DPAD_DOWN } |]
encodeKey DpadLeftBtn = [C.pure| uint16_t { BTN_DPAD_LEFT } |]
encodeKey DpadRightBtn = [C.pure| uint16_t { BTN_DPAD_RIGHT } |]
encodeKey AlsToggleKey = [C.pure| uint16_t { KEY_ALS_TOGGLE } |]
encodeKey RotateLockToggleKey = [C.pure| uint16_t { KEY_ROTATE_LOCK_TOGGLE } |]
encodeKey ButtonconfigKey = [C.pure| uint16_t { KEY_BUTTONCONFIG } |]
encodeKey TaskmanagerKey = [C.pure| uint16_t { KEY_TASKMANAGER } |]
encodeKey JournalKey = [C.pure| uint16_t { KEY_JOURNAL } |]
encodeKey ControlpanelKey = [C.pure| uint16_t { KEY_CONTROLPANEL } |]
encodeKey AppselectKey = [C.pure| uint16_t { KEY_APPSELECT } |]
encodeKey ScreensaverKey = [C.pure| uint16_t { KEY_SCREENSAVER } |]
encodeKey VoicecommandKey = [C.pure| uint16_t { KEY_VOICECOMMAND } |]
encodeKey AssistantKey = [C.pure| uint16_t { KEY_ASSISTANT } |]
encodeKey BrightnessMinKey = [C.pure| uint16_t { KEY_BRIGHTNESS_MIN } |]
encodeKey BrightnessMaxKey = [C.pure| uint16_t { KEY_BRIGHTNESS_MAX } |]
encodeKey KbdinputassistPrevKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_PREV } |]
encodeKey KbdinputassistNextKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_NEXT } |]
encodeKey KbdinputassistPrevgroupKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_PREVGROUP } |]
encodeKey KbdinputassistNextgroupKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_NEXTGROUP } |]
encodeKey KbdinputassistAcceptKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_ACCEPT } |]
encodeKey KbdinputassistCancelKey =
  [C.pure| uint16_t { KEY_KBDINPUTASSIST_CANCEL } |]
encodeKey RightUpKey = [C.pure| uint16_t { KEY_RIGHT_UP } |]
encodeKey RightDownKey = [C.pure| uint16_t { KEY_RIGHT_DOWN } |]
encodeKey LeftUpKey = [C.pure| uint16_t { KEY_LEFT_UP } |]
encodeKey LeftDownKey = [C.pure| uint16_t { KEY_LEFT_DOWN } |]
encodeKey RootMenuKey = [C.pure| uint16_t { KEY_ROOT_MENU } |]
encodeKey MediaTopMenuKey = [C.pure| uint16_t { KEY_MEDIA_TOP_MENU } |]
encodeKey Numeric11Key = [C.pure| uint16_t { KEY_NUMERIC_11 } |]
encodeKey Numeric12Key = [C.pure| uint16_t { KEY_NUMERIC_12 } |]
encodeKey AudioDescKey = [C.pure| uint16_t { KEY_AUDIO_DESC } |]
encodeKey ThreeDModeKey = [C.pure| uint16_t { KEY_3D_MODE } |]
encodeKey NextFavoriteKey = [C.pure| uint16_t { KEY_NEXT_FAVORITE } |]
encodeKey StopRecordKey = [C.pure| uint16_t { KEY_STOP_RECORD } |]
encodeKey PauseRecordKey = [C.pure| uint16_t { KEY_PAUSE_RECORD } |]
encodeKey VodKey = [C.pure| uint16_t { KEY_VOD } |]
encodeKey UnmuteKey = [C.pure| uint16_t { KEY_UNMUTE } |]
encodeKey FastreverseKey = [C.pure| uint16_t { KEY_FASTREVERSE } |]
encodeKey SlowreverseKey = [C.pure| uint16_t { KEY_SLOWREVERSE } |]
encodeKey DataKey = [C.pure| uint16_t { KEY_DATA } |]
encodeKey OnscreenKeyboardKey = [C.pure| uint16_t { KEY_ONSCREEN_KEYBOARD } |]
encodeKey TriggerHappyBtn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY } |]
encodeKey TriggerHappy1Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY1 } |]
encodeKey TriggerHappy2Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY2 } |]
encodeKey TriggerHappy3Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY3 } |]
encodeKey TriggerHappy4Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY4 } |]
encodeKey TriggerHappy5Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY5 } |]
encodeKey TriggerHappy6Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY6 } |]
encodeKey TriggerHappy7Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY7 } |]
encodeKey TriggerHappy8Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY8 } |]
encodeKey TriggerHappy9Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY9 } |]
encodeKey TriggerHappy10Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY10 } |]
encodeKey TriggerHappy11Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY11 } |]
encodeKey TriggerHappy12Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY12 } |]
encodeKey TriggerHappy13Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY13 } |]
encodeKey TriggerHappy14Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY14 } |]
encodeKey TriggerHappy15Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY15 } |]
encodeKey TriggerHappy16Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY16 } |]
encodeKey TriggerHappy17Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY17 } |]
encodeKey TriggerHappy18Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY18 } |]
encodeKey TriggerHappy19Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY19 } |]
encodeKey TriggerHappy20Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY20 } |]
encodeKey TriggerHappy21Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY21 } |]
encodeKey TriggerHappy22Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY22 } |]
encodeKey TriggerHappy23Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY23 } |]
encodeKey TriggerHappy24Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY24 } |]
encodeKey TriggerHappy25Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY25 } |]
encodeKey TriggerHappy26Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY26 } |]
encodeKey TriggerHappy27Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY27 } |]
encodeKey TriggerHappy28Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY28 } |]
encodeKey TriggerHappy29Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY29 } |]
encodeKey TriggerHappy30Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY30 } |]
encodeKey TriggerHappy31Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY31 } |]
encodeKey TriggerHappy32Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY32 } |]
encodeKey TriggerHappy33Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY33 } |]
encodeKey TriggerHappy34Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY34 } |]
encodeKey TriggerHappy35Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY35 } |]
encodeKey TriggerHappy36Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY36 } |]
encodeKey TriggerHappy37Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY37 } |]
encodeKey TriggerHappy38Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY38 } |]
encodeKey TriggerHappy39Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY39 } |]
encodeKey TriggerHappy40Btn = [C.pure| uint16_t { BTN_TRIGGER_HAPPY40 } |]

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
  deriving (Bounded, Enum)

decodeRel :: Word16 -> Maybe Rel
decodeRel code = getFirst (foldMap (First . match) [minBound .. maxBound])
  where
    match rel
      | encodeRel rel == code = Just rel
      | otherwise = Nothing

encodeRel :: Rel -> Word16
encodeRel XRel = [C.pure| uint16_t { REL_X } |]
encodeRel YRel = [C.pure| uint16_t { REL_Y } |]
encodeRel ZRel = [C.pure| uint16_t { REL_Z } |]
encodeRel RxRel = [C.pure| uint16_t { REL_RX } |]
encodeRel RyRel = [C.pure| uint16_t { REL_RY } |]
encodeRel RzRel = [C.pure| uint16_t { REL_RZ } |]
encodeRel HwheelRel = [C.pure| uint16_t { REL_HWHEEL } |]
encodeRel DialRel = [C.pure| uint16_t { REL_DIAL } |]
encodeRel WheelRel = [C.pure| uint16_t { REL_WHEEL } |]
encodeRel MiscRel = [C.pure| uint16_t { REL_MISC } |]

data Abs
  = XAbs
  | YAbs
  | ZAbs
  | RxAbs
  | RyAbs
  | RzAbs
  | ThrottleAbs
  | RudderAbs
  | WheelAbs
  | GasAbs
  | BrakeAbs
  | Hat0xAbs
  | Hat0yAbs
  | Hat1xAbs
  | Hat1yAbs
  | Hat2xAbs
  | Hat2yAbs
  | Hat3xAbs
  | Hat3yAbs
  | PressureAbs
  | DistanceAbs
  | TiltXAbs
  | TiltYAbs
  | ToolWidthAbs
  | VolumeAbs
  | MiscAbs
  | ReservedAbs
  | MtSlotAbs
  | MtTouchMajorAbs
  | MtTouchMinorAbs
  | MtWidthMajorAbs
  | MtWidthMinorAbs
  | MtOrientationAbs
  | MtPositionXAbs
  | MtPositionYAbs
  | MtToolTypeAbs
  | MtBlobIdAbs
  | MtTrackingIdAbs
  | MtPressureAbs
  | MtDistanceAbs
  | MtToolXAbs
  | MtToolYAbs
  deriving (Bounded, Enum)

decodeAbs :: Word16 -> Maybe Abs
decodeAbs code = getFirst (foldMap (First . match) [minBound .. maxBound])
  where
    match abs
      | encodeAbs abs == code = Just abs
      | otherwise = Nothing

encodeAbs :: Abs -> Word16
encodeAbs XAbs = [C.pure| uint16_t { ABS_X } |]
encodeAbs YAbs = [C.pure| uint16_t { ABS_Y } |]
encodeAbs ZAbs = [C.pure| uint16_t { ABS_Z } |]
encodeAbs RxAbs = [C.pure| uint16_t { ABS_RX } |]
encodeAbs RyAbs = [C.pure| uint16_t { ABS_RY } |]
encodeAbs RzAbs = [C.pure| uint16_t { ABS_RZ } |]
encodeAbs ThrottleAbs = [C.pure| uint16_t { ABS_THROTTLE } |]
encodeAbs RudderAbs = [C.pure| uint16_t { ABS_RUDDER } |]
encodeAbs WheelAbs = [C.pure| uint16_t { ABS_WHEEL } |]
encodeAbs GasAbs = [C.pure| uint16_t { ABS_GAS } |]
encodeAbs BrakeAbs = [C.pure| uint16_t { ABS_BRAKE } |]
encodeAbs Hat0xAbs = [C.pure| uint16_t { ABS_HAT0X } |]
encodeAbs Hat0yAbs = [C.pure| uint16_t { ABS_HAT0Y } |]
encodeAbs Hat1xAbs = [C.pure| uint16_t { ABS_HAT1X } |]
encodeAbs Hat1yAbs = [C.pure| uint16_t { ABS_HAT1Y } |]
encodeAbs Hat2xAbs = [C.pure| uint16_t { ABS_HAT2X } |]
encodeAbs Hat2yAbs = [C.pure| uint16_t { ABS_HAT2Y } |]
encodeAbs Hat3xAbs = [C.pure| uint16_t { ABS_HAT3X } |]
encodeAbs Hat3yAbs = [C.pure| uint16_t { ABS_HAT3Y } |]
encodeAbs PressureAbs = [C.pure| uint16_t { ABS_PRESSURE } |]
encodeAbs DistanceAbs = [C.pure| uint16_t { ABS_DISTANCE } |]
encodeAbs TiltXAbs = [C.pure| uint16_t { ABS_TILT_X } |]
encodeAbs TiltYAbs = [C.pure| uint16_t { ABS_TILT_Y } |]
encodeAbs ToolWidthAbs = [C.pure| uint16_t { ABS_TOOL_WIDTH } |]
encodeAbs VolumeAbs = [C.pure| uint16_t { ABS_VOLUME } |]
encodeAbs MiscAbs = [C.pure| uint16_t { ABS_MISC } |]
encodeAbs ReservedAbs = [C.pure| uint16_t { ABS_RESERVED } |]
encodeAbs MtSlotAbs = [C.pure| uint16_t { ABS_MT_SLOT } |]
encodeAbs MtTouchMajorAbs = [C.pure| uint16_t { ABS_MT_TOUCH_MAJOR } |]
encodeAbs MtTouchMinorAbs = [C.pure| uint16_t { ABS_MT_TOUCH_MINOR } |]
encodeAbs MtWidthMajorAbs = [C.pure| uint16_t { ABS_MT_WIDTH_MAJOR } |]
encodeAbs MtWidthMinorAbs = [C.pure| uint16_t { ABS_MT_WIDTH_MINOR } |]
encodeAbs MtOrientationAbs = [C.pure| uint16_t { ABS_MT_ORIENTATION } |]
encodeAbs MtPositionXAbs = [C.pure| uint16_t { ABS_MT_POSITION_X } |]
encodeAbs MtPositionYAbs = [C.pure| uint16_t { ABS_MT_POSITION_Y } |]
encodeAbs MtToolTypeAbs = [C.pure| uint16_t { ABS_MT_TOOL_TYPE } |]
encodeAbs MtBlobIdAbs = [C.pure| uint16_t { ABS_MT_BLOB_ID } |]
encodeAbs MtTrackingIdAbs = [C.pure| uint16_t { ABS_MT_TRACKING_ID } |]
encodeAbs MtPressureAbs = [C.pure| uint16_t { ABS_MT_PRESSURE } |]
encodeAbs MtDistanceAbs = [C.pure| uint16_t { ABS_MT_DISTANCE } |]
encodeAbs MtToolXAbs = [C.pure| uint16_t { ABS_MT_TOOL_X } |]
encodeAbs MtToolYAbs = [C.pure| uint16_t { ABS_MT_TOOL_Y } |]

data Sw
  = LidSw
  | TabletModeSw
  | HeadphoneInsertSw
  | RfkillAllSw
  | RadioSw
  | MicrophoneInsertSw
  | DockSw
  | LineoutInsertSw
  | JackPhysicalInsertSw
  | VideooutInsertSw
  | CameraLensCoverSw
  | KeypadSlideSw
  | FrontProximitySw
  | RotateLockSw
  | LineinInsertSw
  | MuteDeviceSw
  | PenInsertedSw

encodeSw :: Sw -> Word16
encodeSw LidSw = [C.pure| uint16_t { SW_LID } |]
encodeSw TabletModeSw = [C.pure| uint16_t { SW_TABLET_MODE } |]
encodeSw HeadphoneInsertSw = [C.pure| uint16_t { SW_HEADPHONE_INSERT } |]
encodeSw RfkillAllSw = [C.pure| uint16_t { SW_RFKILL_ALL } |]
encodeSw RadioSw = [C.pure| uint16_t { SW_RADIO } |]
encodeSw MicrophoneInsertSw = [C.pure| uint16_t { SW_MICROPHONE_INSERT } |]
encodeSw DockSw = [C.pure| uint16_t { SW_DOCK } |]
encodeSw LineoutInsertSw = [C.pure| uint16_t { SW_LINEOUT_INSERT } |]
encodeSw JackPhysicalInsertSw = [C.pure| uint16_t { SW_JACK_PHYSICAL_INSERT } |]
encodeSw VideooutInsertSw = [C.pure| uint16_t { SW_VIDEOOUT_INSERT } |]
encodeSw CameraLensCoverSw = [C.pure| uint16_t { SW_CAMERA_LENS_COVER } |]
encodeSw KeypadSlideSw = [C.pure| uint16_t { SW_KEYPAD_SLIDE } |]
encodeSw FrontProximitySw = [C.pure| uint16_t { SW_FRONT_PROXIMITY } |]
encodeSw RotateLockSw = [C.pure| uint16_t { SW_ROTATE_LOCK } |]
encodeSw LineinInsertSw = [C.pure| uint16_t { SW_LINEIN_INSERT } |]
encodeSw MuteDeviceSw = [C.pure| uint16_t { SW_MUTE_DEVICE } |]
encodeSw PenInsertedSw = [C.pure| uint16_t { SW_PEN_INSERTED } |]

data Msc
  = SerialMsc
  | PulseledMsc
  | GestureMsc
  | RawMsc
  | ScanMsc
  | TimestampMsc

data Led
  = NumlLed
  | CapslLed
  | ScrolllLed
  | ComposeLed
  | KanaLed
  | SleepLed
  | SuspendLed
  | MuteLed
  | MiscLed
  | MailLed
  | ChargingLed

data Rep
  = DelayRep
  | PeriodRep

data Snd
  = ClickSnd
  | BellSnd
  | ToneSnd
