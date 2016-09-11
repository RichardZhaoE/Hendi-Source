object frmWvsFake: TfrmWvsFake
  Left = 0
  Top = 0
  Caption = 'CLB'
  ClientHeight = 330
  ClientWidth = 442
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblMap: TLabel
    Left = 0
    Top = 0
    Width = 442
    Height = 13
    Align = alTop
    OnDblClick = lblMapDblClick
    ExplicitWidth = 3
  end
  object mmLog: TMemo
    Left = 0
    Top = 13
    Width = 442
    Height = 139
    Align = alClient
    PopupMenu = pmRight
    ReadOnly = True
    TabOrder = 0
    OnDblClick = mmLogDblClick
  end
  object pnlChat: TPanel
    Left = 0
    Top = 237
    Width = 442
    Height = 22
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      442
      22)
    object edtChat: TEdit
      Left = 0
      Top = 0
      Width = 381
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnChat: TButton
      Left = 378
      Top = -1
      Width = 64
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Send'
      TabOrder = 1
      OnClick = btnChatClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 259
    Width = 442
    Height = 35
    Align = alBottom
    Color = clWindow
    ParentBackground = False
    TabOrder = 2
    object lblLv: TLabel
      Left = 15
      Top = 4
      Width = 38
      Height = 13
      AutoSize = False
      Caption = 'LV. '
    end
    object lblName: TLabel
      Left = 15
      Top = 18
      Width = 3
      Height = 13
    end
    object lblExp: TLabel
      Left = 290
      Top = 4
      Width = 6
      Height = 13
      Caption = '0'
    end
    object lblMesos: TLabel
      Left = 290
      Top = 18
      Width = 6
      Height = 13
      Caption = '0'
    end
    object lblMP: TLabel
      Left = 156
      Top = 18
      Width = 14
      Height = 13
      Caption = 'MP'
    end
    object lblHP: TLabel
      Left = 156
      Top = 4
      Width = 13
      Height = 13
      Caption = 'HP'
    end
    object cbExit: TButton
      Left = 400
      Top = 6
      Width = 35
      Height = 23
      Caption = 'Exit'
      TabOrder = 0
      OnClick = cbExitClick
    end
  end
  object mmLog2: TMemo
    Left = 0
    Top = 152
    Width = 442
    Height = 85
    Align = alBottom
    ReadOnly = True
    TabOrder = 3
    OnDblClick = mmLog2DblClick
  end
  object gbSettings: TGroupBox
    Left = 0
    Top = 294
    Width = 442
    Height = 36
    Align = alBottom
    Caption = 'Settings'
    TabOrder = 4
    object cbServerCtrl: TCheckBox
      Left = 362
      Top = 6
      Width = 79
      Height = 17
      Caption = 'SyncServer'
      TabOrder = 0
      OnClick = cbServerCtrlClick
    end
    object btnConnect: TButton
      Left = 2
      Top = 13
      Width = 63
      Height = 22
      Caption = 'Connect'
      TabOrder = 1
      OnClick = btnConnectClick
    end
    object cbCapCC: TCheckBox
      Left = 270
      Top = 12
      Width = 86
      Height = 17
      Caption = '13 Channels'
      TabOrder = 2
    end
    object btnBot: TButton
      Left = 66
      Top = 13
      Width = 63
      Height = 22
      Caption = 'Start Bot'
      TabOrder = 3
      OnClick = mmLogDblClick
    end
    object cbMDRush: TCheckBox
      Left = 144
      Top = 12
      Width = 119
      Height = 17
      Caption = 'Mini Dungeon Rusher'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object cbRestart: TButton
      Left = 362
      Top = 6
      Width = 75
      Height = 25
      Caption = 'Restart'
      TabOrder = 5
      Visible = False
      OnClick = cbRestartClick
    end
  end
  object Client: TIdTCPClient
    ConnectTimeout = 0
    Host = '63.251.217.2'
    IPVersion = Id_IPv4
    Port = 8484
    ReadTimeout = -1
    Left = 87
    Top = 60
  end
  object pmRight: TPopupMenu
    Left = 295
    Top = 86
    object EvolvingRingFirst1: TMenuItem
      Caption = 'Evolving Ring (First)'
      OnClick = EvolvingRingFirst1Click
    end
    object EvolvingRing1: TMenuItem
      Caption = 'Evolving Ring'
      OnClick = EvolvingRing1Click
    end
    object Debugmovement1: TMenuItem
      Caption = 'Debug movement'
      OnClick = Debugmovement1Click
    end
    object invtest1: TMenuItem
      Caption = 'inv test'
      OnClick = invtest1Click
    end
    object gui1: TMenuItem
      Caption = 'gui'
      OnClick = gui1Click
    end
    object coinexploit1: TMenuItem
      Caption = 'coin exploit'
      OnClick = coinexploit1Click
    end
    object StopExploit1: TMenuItem
      Caption = 'Stop Exploit'
      OnClick = StopExploit1Click
    end
    object MobList1: TMenuItem
      Caption = 'MobList'
      OnClick = MobList1Click
    end
    object Savechecksumstofile1: TMenuItem
      Caption = 'Save checksums to file'
      OnClick = Savechecksumstofile1Click
    end
  end
end
