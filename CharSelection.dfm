object frmCharSel: TfrmCharSel
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Character Selection'
  ClientHeight = 141
  ClientWidth = 293
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbChars: TListBox
    Left = 0
    Top = 0
    Width = 293
    Height = 141
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = lbCharsDblClick
  end
end
