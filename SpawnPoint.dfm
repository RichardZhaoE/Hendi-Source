object frmSpawnPoint: TfrmSpawnPoint
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Set Spawn-Point'
  ClientHeight = 60
  ClientWidth = 376
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 24
    Height = 13
    Caption = 'Map:'
  end
  object cbMap: TComboBox
    Left = 38
    Top = 5
    Width = 210
    Height = 21
    Style = csDropDownList
    Sorted = True
    TabOrder = 0
    OnSelect = cbMapSelect
  end
  object btnAdd: TButton
    Left = 254
    Top = 5
    Width = 57
    Height = 22
    Caption = 'Add'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 312
    Top = 5
    Width = 57
    Height = 22
    Caption = 'Remove'
    TabOrder = 2
    OnClick = btnRemoveClick
  end
  object edtX: TLabeledEdit
    Left = 52
    Top = 32
    Width = 50
    Height = 21
    EditLabel.Width = 10
    EditLabel.Height = 13
    EditLabel.Caption = 'X:'
    Enabled = False
    LabelPosition = lpLeft
    TabOrder = 3
    OnExit = edtXExit
  end
  object edtY: TLabeledEdit
    Left = 131
    Top = 32
    Width = 50
    Height = 21
    EditLabel.Width = 10
    EditLabel.Height = 13
    EditLabel.Caption = 'Y:'
    Enabled = False
    LabelPosition = lpLeft
    TabOrder = 4
    OnExit = edtYExit
  end
  object btnSave: TButton
    Left = 208
    Top = 32
    Width = 75
    Height = 21
    Caption = 'Save'
    TabOrder = 5
    OnClick = btnSaveClick
  end
end
