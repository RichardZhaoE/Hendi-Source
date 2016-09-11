object frmWorlds: TfrmWorlds
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = '%s - World Selection'
  ClientHeight = 86
  ClientWidth = 190
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cbWorlds: TComboBoxEx
    Left = 21
    Top = 8
    Width = 145
    Height = 22
    ItemsEx = <
      item
        Caption = 'Scania'
      end
      item
        Caption = 'Bera'
      end
      item
        Caption = 'Broa'
      end
      item
        Caption = 'Windia'
      end
      item
        Caption = 'Khaini'
      end
      item
        Caption = 'Bellocan'
      end
      item
        Caption = 'Mardia'
      end
      item
        Caption = 'Kradia'
      end
      item
        Caption = 'Yellonde'
      end
      item
        Caption = 'Demethos'
      end
      item
        Caption = 'Galicia'
      end
      item
        Caption = 'El Nido'
      end
      item
        Caption = 'Zenith'
      end
      item
        Caption = 'Arcania'
      end>
    Style = csExDropDownList
    TabOrder = 0
  end
  object cbSave: TCheckBox
    Left = 21
    Top = 31
    Width = 97
    Height = 17
    Caption = 'Remember'
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 57
    Top = 53
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
end
