object frmItems: TfrmItems
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Items'
  ClientHeight = 235
  ClientWidth = 254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LVItems: TListView
    Left = 0
    Top = 0
    Width = 254
    Height = 235
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        Caption = 'Loot'
      end
      item
        Caption = 'Name'
        Width = 165
      end>
    ReadOnly = True
    RowSelect = True
    PopupMenu = pmRight
    TabOrder = 0
    ViewStyle = vsReport
    OnItemChecked = LVItemsItemChecked
  end
  object pmRight: TPopupMenu
    Left = 92
    Top = 51
    object Reload1: TMenuItem
      Caption = 'Reload'
      OnClick = Reload1Click
    end
  end
end
