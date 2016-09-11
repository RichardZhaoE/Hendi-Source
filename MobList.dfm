object frmMobs: TfrmMobs
  Left = 0
  Top = 0
  Caption = 'MobList'
  ClientHeight = 352
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LVMobs: TListView
    Left = 0
    Top = 0
    Width = 295
    Height = 352
    Align = alClient
    Columns = <
      item
        Caption = 'Object ID'
        Width = 70
      end
      item
        Caption = 'ID'
        Width = 60
      end
      item
        Caption = 'HP'
        Width = 70
      end
      item
        Caption = 'ServerHP'
        Width = 60
      end>
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 208
  end
  object TmrUpdate: TTimer
    Enabled = False
    OnTimer = TmrUpdateTimer
    Left = 82
    Top = 125
  end
end
