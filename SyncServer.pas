// DataSnap Client Class

unit SyncServer;

interface
{$IFDEF DATASNAP}
uses DBXCommon, Classes, SysUtils, DB, SqlExpr, DBXDBReaders;

type
  TSyncClient = class
  private
    FDBXConnection: TDBXConnection;
    FInstanceOwner: Boolean;
    FAddItem: TDBXCommand;
    FGetChannel: TDBXCommand;

    procedure Init;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;

    procedure AddItem(Mob, Item: Integer; const Name: string);
    function GetChannel(DoChange: Boolean; Old: Integer): Integer;
  end;
{$ENDIF}
implementation
{$IFDEF DATASNAP}
procedure TSyncClient.Init;
begin
  FAddItem := FDBXConnection.CreateCommand;
  FAddItem.CommandType := TDBXCommandTypes.DSServerMethod;
  FAddItem.Text := 'TSyncServerModule.AddItem';
  FAddItem.Prepare;

  FGetChannel := FDBXConnection.CreateCommand;
  FGetChannel.CommandType := TDBXCommandTypes.DSServerMethod;
  FGetChannel.Text := 'TSyncServerModule.GetChannel';
  FGetChannel.Prepare;
end;

procedure TSyncClient.AddItem(Mob, Item: Integer;
  const Name: string);
begin
  FAddItem.Parameters[0].Value.SetInt32(Mob);
  FAddItem.Parameters[1].Value.SetInt32(Item);
  FAddItem.Parameters[2].Value.SetString(Name);
  FAddItem.ExecuteUpdate;
end;

function TSyncClient.GetChannel(DoChange: Boolean; Old: Integer): Integer;
begin
  FGetChannel.Parameters[0].Value.SetBoolean(DoChange);
  FGetChannel.Parameters[1].Value.SetInt32(Old);
  FGetChannel.ExecuteUpdate;
  Result := FGetChannel.Parameters[2].Value.GetInt32;
end;

constructor TSyncClient.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create;
  if ADBXConnection = nil then
    raise EInvalidOperation.Create('TSyncClient: No connection.');
  FDBXConnection := ADBXConnection;
  FInstanceOwner := True;

  Init;
end;

constructor TSyncClient.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create;
  if ADBXConnection = nil then
    raise EInvalidOperation.Create('TSyncClient: No connection.');
  FDBXConnection := ADBXConnection;
  FInstanceOwner := AInstanceOwner;

  Init;
end;

destructor TSyncClient.Destroy;
begin
  FAddItem.Free;
  FGetChannel.Free;

  inherited;
end;

{$ENDIF}
end.
