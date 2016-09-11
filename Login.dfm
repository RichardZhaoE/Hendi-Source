object frmLogin: TfrmLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Login'
  ClientHeight = 102
  ClientWidth = 255
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
  PixelsPerInch = 96
  TextHeight = 13
  object btnLogin: TButton
    Left = 160
    Top = 23
    Width = 86
    Height = 39
    Caption = 'Login'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    OnClick = btnLoginClick
  end
  object cbCharSel: TCheckBox
    Left = 8
    Top = 79
    Width = 153
    Height = 17
    Caption = 'Show Character Selection'
    TabOrder = 1
    OnClick = cbCharSelClick
  end
  object cbAccs: TComboBox
    Left = 8
    Top = 17
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbAccsChange
  end
  object btnAdd: TButton
    Left = 8
    Top = 44
    Width = 70
    Height = 25
    Caption = 'Add'
    TabOrder = 3
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 84
    Top = 44
    Width = 70
    Height = 25
    Caption = 'Remove'
    TabOrder = 4
    OnClick = btnRemoveClick
  end
end
