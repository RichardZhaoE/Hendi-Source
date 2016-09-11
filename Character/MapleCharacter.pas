unit MapleCharacter;

interface

uses Windows, SysUtils, Math, MapleMap, DataCenter, Inventory, GameLogic;

type
  TExtendedSP = array of Byte;
  TInventories = array[TMapleInventoryType] of TInventory;

  TOnNPCTalk = reference to procedure(Mode: Byte);

  TMapleCharacter = class
  private
    FID: Integer;
    FName: string;
    FGender, FSkinColor, FLevel: Byte;
    FFace, FHair: Integer;
    FJob: Word;
    FStr, FDex, FInt, FLuk, FHP, FMaxHP, FMP, FMaxMP, FAP, FSP: Integer;
    FExtendedSP: TExtendedSP;
    FExp, FMesos, FMapID: Integer;
    FFame: SmallInt;
    FSpawnpoint, FUsedPortals: Byte;
    FPosition: TPoint;
    FMap: TMapleMap;
    FInventory: TInventories;
    FOnNPCTalk: TOnNPCTalk;

    procedure SetExp(Value: Integer);
    procedure SetLevel(Value: Byte);
    procedure SetMesos(Value: Integer);
    procedure SetHP(Value: Integer);
    procedure SetMP(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    function IsExtendedSPJob: Boolean;

    procedure InitInventory(T: TMapleInventoryType; Limit: Byte);
    procedure LoadMap; overload;
    procedure LoadMap(ID: Integer); overload;
    procedure Spawn;

    function ToString: string; override;
    procedure UpdateGUI;

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Gender: Byte read FGender write FGender;
    property SkinColor: Byte read FSkinColor write FSkinColor;
    property Level: Byte read FLevel write SetLevel;
    property Face: Integer read FFace write FFace;
    property Hair: Integer read FHair write FHair;
    property Job: Word read FJob write FJob;
    property Str: Integer read FStr write FStr;
    property Dex: Integer read FDex write FDex;
    property Int: Integer read FInt write FInt;
    property Luk: Integer read FLuk write FLuk;
    property HP: Integer read FHP write SetHP;
    property MP: Integer read FMP write SetMP;
    property MaxHP: Integer read FMaxHP write FMaxHP;
    property MaxMP: Integer read FMaxMP write FMaxMP;
    property AP: Integer read FAP write FAP;
    property SP: Integer read FSP write FSP;
    property ExtendedSP: TExtendedSP read FExtendedSP write FExtendedSP;
    property Exp: Integer read FExp write SetExp;
    property Map: TMapleMap read FMap write FMap;
    property MapID: Integer read FMapID write FMapID;
    property Mesos: Integer read FMesos write SetMesos;
    property Fame: SmallInt read FFame write FFame;
    property Spawnpoint: Byte read FSpawnpoint write FSpawnpoint;
    property Position: TPoint read FPosition write FPosition;
    property Inventory: TInventories read FInventory;
    property OnNPCTalk: TOnNPCTalk read FOnNPCTalk write FOnNPCTalk;
    property UsedPortals: Byte read FUsedPortals write FUsedPortals;
  end;

implementation

uses Main, PacketIO, DeathLoader, InvGUI, Movement;

{ TMapleCharacter }

constructor TMapleCharacter.Create;
begin
  FMap := nil;
  FUsedPortals := 1;
end;

destructor TMapleCharacter.Destroy;
begin
  if Assigned(FMap) then
    FMap.Free;

  inherited;
end;

procedure TMapleCharacter.InitInventory(T: TMapleInventoryType; Limit: Byte);
begin
  FInventory[T] := TInventory.Create(T, Limit);
end;

function TMapleCharacter.IsExtendedSPJob: Boolean;
begin
  Result := ((Job >= 2200) and (Job <= 2218)) or (Job div 1000 = 3);
end;

procedure TMapleCharacter.LoadMap;
begin
  if Assigned(FMap) then
    FMap.Free;

  FMap := DataCenter.LoadMap(FMapID);
  frmWvsFake.lblMap.Caption := Format('[%d] %s', [FMap.ID, StringData.GetMapName(FMap.ID)]);
end;

procedure TMapleCharacter.LoadMap(ID: Integer);
begin
  Inc(FUsedPortals);
  FMapID := ID;
  LoadMap;
end;

procedure TMapleCharacter.SetExp(Value: Integer);
begin
  FExp := Value;
  if Character <> nil then
    frmWvsFake.lblExp.Caption := Format('%.2f%% (%d)', [(Value / EXP_TABLE[FLevel]) * 100, Value]);
end;

procedure TMapleCharacter.SetLevel(Value: Byte);
begin
  FLevel := Value;
  if Character <> nil then
    frmWvsFake.lblLV.Caption := Format('LV. %d', [Value]);
end;

procedure TMapleCharacter.SetMesos(Value: Integer);
var
  s: string;
begin
  FMesos := Value;

  s := IntToStr(Value);
  if Length(s) >= 4 then
    Insert(',', s, Length(s) - 2);
  if Length(s) >= 7 then
    Insert(',', s, Length(s) - 6);
  frmWvsFake.lblMesos.Caption := s;

  if frmInvGUI.Showing then
    frmInvGUI.lblMesos.Caption := s;
end;

procedure TMapleCharacter.SetHP(Value: Integer);
begin
  FHP := Value;
  if Character <> nil then
    frmWvsFake.lblHP.Caption := Format('HP: %d / %d', [Value, FMaxHP]);

  if (FHP <= 30) and (Attacker <> nil) then
    UseItem(6, 2001001);
end;

procedure TMapleCharacter.SetMP(Value: Integer);
begin
  FMP := Value;
  if Character <> nil then
    frmWvsFake.lblMP.Caption := Format('MP: %d / %d', [Value, FMaxMP]);

  if (FMP < 100) and (Attacker <> nil) then
// Pure Water    UseItem(5, 2022000);
                 UseItem(5, 2002024);
end;

procedure TMapleCharacter.Spawn;
begin
  frmWvsFake.Write(Movement.SpawnCharacter(FMap.Portals[FSpawnpoint]));
end;

function TMapleCharacter.ToString: string;
begin
  Result := Format('%s - Lv. %d - %d', [FName, FLevel, FJob]);
end;

procedure TMapleCharacter.UpdateGUI;
begin
  frmWvsFake.lblLV.Caption := Format('LV. %d', [FLevel]);
  frmWvsFake.lblMP.Caption := Format('MP: %d / %d', [FMP, FMaxMP]);
  frmWvsFake.lblExp.Caption := Format('%.2f%% (%d)', [(FExp / EXP_TABLE[FLevel]) * 100, FExp]);
end;

end.
