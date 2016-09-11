unit SpawnPoint;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Generics.Collections;

type
  TSpawnPoint = class
  public
    Map: Integer;
    X, Y: SmallInt;

    constructor Create(AMap: Integer);
  end;

  TfrmSpawnPoint = class(TForm)
    cbMap: TComboBox;
    Label1: TLabel;
    btnAdd: TButton;
    btnRemove: TButton;
    edtX: TLabeledEdit;
    edtY: TLabeledEdit;
    btnSave: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cbMapSelect(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure edtXExit(Sender: TObject);
    procedure edtYExit(Sender: TObject);
  private
    FMaps: TDictionary<Integer, TSpawnPoint>;

    procedure Load;
    procedure Save;
  public
    property Maps: TDictionary<Integer, TSpawnPoint> read FMaps;
  end;

var
  frmSpawnPoint: TfrmSpawnPoint;

implementation

uses DataCenter;

{$R *.dfm}

{ TSpawnPoint }

constructor TSpawnPoint.Create(AMap: Integer);
begin
  Map := AMap;
  X := 0;
  Y := 0;
end;

{ TfrmSpawnPoint }

procedure TfrmSpawnPoint.btnAddClick(Sender: TObject);
var
  SMap, MapName: string;
  Map: Integer;
  SP: TSpawnPoint;
begin
  SMap := InputBox('Add Map', 'Map ID:', '');
  if (SMap = '') or (not TryStrToInt(SMap, Map)) then
    Exit;

  if FMaps.ContainsKey(Map) then
    raise Exception.Create('That map is already in the list!');

  MapName := DataCenter.StringData.GetMapName(Map);
  if MapName = '' then
    raise Exception.Create('That map doesn''t seem to exist');

  SP := TSpawnPoint.Create(Map);
  cbMap.ItemIndex := cbMap.Items.AddObject(Format('[%s] %s', [SMap, MapName]), SP);
  FMaps.Add(Map, SP);

  cbMap.OnSelect(nil);

  Save;
end;

procedure TfrmSpawnPoint.btnRemoveClick(Sender: TObject);
var
  SP: TSpawnPoint;
begin
  if cbMap.ItemIndex < 0 then
    Exit;

  SP := TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]);
  FMaps.Remove(SP.Map);
  SP.Free;
  cbMap.DeleteSelected;
  cbMap.Repaint;
  cbMap.OnSelect(nil);
end;

procedure TfrmSpawnPoint.btnSaveClick(Sender: TObject);
begin
  if cbMap.ItemIndex > -1 then
  begin
    TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]).X := StrToInt(edtX.Text);
    TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]).Y := StrToInt(edtY.Text);
  end;

  Save;
  Close;
end;

procedure TfrmSpawnPoint.cbMapSelect(Sender: TObject);
var
  SP: TSpawnPoint;
begin
  if cbMap.ItemIndex < 0 then
  begin
    edtX.Enabled := False;
    edtY.Enabled := False;
    Exit;
  end;

  SP := TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]);
  edtX.Enabled := True;
  edtY.Enabled := True;
  edtX.Text := IntToStr(SP.X);
  edtY.Text := IntToStr(SP.Y);
end;

procedure TfrmSpawnPoint.edtXExit(Sender: TObject);
begin
  TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]).X := StrToInt(edtX.Text);
end;

procedure TfrmSpawnPoint.edtYExit(Sender: TObject);
begin
  TSpawnPoint(cbMap.Items.Objects[cbMap.ItemIndex]).Y := StrToInt(edtY.Text);
end;

procedure TfrmSpawnPoint.FormCreate(Sender: TObject);
begin
  FMaps := TDictionary<Integer, TSpawnPoint>.Create;
  Load;
end;

procedure TfrmSpawnPoint.FormDestroy(Sender: TObject);
var
  SP: TSpawnPoint;
begin
  for SP in FMaps.Values do
    SP.Free;

  FMaps.Free;
end;

procedure TfrmSpawnPoint.Load;
var
  FS: TFileStream;
  Cur, Count, i: Integer;
  MapName: string;
  SP: TSpawnPoint;
begin
  if not FileExists(ExtractFilePath(ParamStr(0)) + 'SpawnPoints.dat') then
    Exit;

  FS := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'SpawnPoints.dat', fmOpenRead);
  try
    with TReader.Create(FS, 1024) do
    begin
      Count := ReadInteger;
      Cur := ReadInteger;

      for i := 0 to Count - 1 do
      begin
        SP := TSpawnPoint.Create(ReadInteger);
        SP.X := ReadInteger;
        SP.Y := ReadInteger;

        FMaps.Add(SP.Map, SP);
        MapName := StringData.GetMapName(SP.Map);
        cbMap.AddItem(Format('[%d] %s', [SP.Map, MapName]), SP);
      end;
      Free;
    end;
  finally
    FS.Free;
  end;

  if cbMap.Items.Count > Cur then
  begin
    cbMap.ItemIndex := Cur;
    cbMap.OnSelect(nil);
  end;
end;

procedure TfrmSpawnPoint.Save;
var
  FS: TFileStream;
  SP: TSpawnPoint;
begin
  FS := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'SpawnPoints.dat', fmCreate);
  try
    with TWriter.Create(FS, 1024) do
    begin
      WriteInteger(FMaps.Count);
      WriteInteger(cbMap.ItemIndex);

      for SP in FMaps.Values do
      begin
        WriteInteger(SP.Map);
        WriteInteger(SP.X);
        WriteInteger(SP.Y);
      end;
      Free;
    end;
  finally
    FS.Free;
  end;
end;

end.
