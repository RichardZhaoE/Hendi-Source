unit Items;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Generics.Collections, ComCtrls, Menus;

type
  TItem = record
    ID: Integer;
    Name: string;
    Loot: Boolean;
    Checksum: Cardinal;

    procedure SetLoot(Value: Boolean);
  end;
  PItem = ^TItem;

  TfrmItems = class(TForm)
    LVItems: TListView;
    pmRight: TPopupMenu;
    Reload1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure LVItemsItemChecked(Sender: TObject; Item: TListItem);
    procedure Reload1Click(Sender: TObject);
  private
    FLoading: Boolean;
    FItems: TDictionary<Integer, PItem>;
  public
    function GetItem(ID: Integer): PItem;
  end;

var
  frmItems: TfrmItems;

implementation

uses Tools;

{$R *.dfm}

{ TfrmItems }

procedure TfrmItems.FormCreate(Sender: TObject);
var
  FN, I: string;
  Items: TStringList;
  SData: TStringArray;
  IID: Integer;
  Item: PItem;
begin
  FItems := TDictionary<Integer, PItem>.Create;

  FN := ExtractFilePath(ParamStr(0)) + 'Items.txt';

  if not FileExists(FN) then
    raise Exception.Create('Items.txt not found');

  Items := TStringList.Create;
  FLoading := True;
  try
    Items.LoadFromFile(FN);
    for I in Items do
    begin
      SData := Explode('|', I);
      IID := StrToInt(SData[0]);

      New(Item);
      Item.ID := IID;
      Item.Name := SData[1];
      try
        Item.Loot := Boolean(StrToInt(SData[2]));
      except
        Item.Loot := False;
        ShowMessage('Error: ' + I);
      end;
      if Length(SData) > 3 then
        Item.Checksum := StrToInt(SData[3]);

      with LVItems.Items.Add do
      begin
        SubItems.Add(Item.Name);
        Checked := Item.Loot;
        Data := Item;
      end;
      FItems.Add(IID, Item);
      SData := nil;
    end;
  finally
    Items.Free;
    FLoading := False;
  end;
end;

function TfrmItems.GetItem(ID: Integer): PItem;
begin
  if (not FItems.TryGetValue(ID, Result)) or (not Result.Loot) then
    Result := nil;
end;

procedure TfrmItems.LVItemsItemChecked(Sender: TObject; Item: TListItem);
begin
  if not FLoading then
    PItem(Item.Data)^.SetLoot(Item.Checked);
end;

procedure TfrmItems.Reload1Click(Sender: TObject);
var
  P: PItem;
begin
  for P in FItems.Values do
    Dispose(P);
  FItems.Free;
  LVItems.Clear;
  FormCreate(nil);
end;

{ TItem }

procedure TItem.SetLoot(Value: Boolean);
var
  Items: TStringList;
  I: string;
begin
  Loot := Value;

  // Update file
  Items := TStringList.Create;
  try
    Items.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Items.txt');
    for I in Items do
    begin
      if StrToInt(Copy(I, 1, 7)) <> ID then
        Continue;

      Items[Items.IndexOf(I)] := Format('%d|%s|%d|$%x', [ID, Name, Ord(Loot), Checksum]);
      Break;
    end;
    Items.SaveToFile(ExtractFilePath(ParamStr(0)) + 'Items.txt');
  finally
    Items.Free;
  end;
end;

end.
