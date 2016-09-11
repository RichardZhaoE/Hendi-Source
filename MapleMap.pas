unit MapleMap;

interface

uses Windows, SysUtils, Generics.Collections, SyncObjs, Scheduler, Footholds;

const
  NO_MOB = 9999999;

type
  TPortal = record
    ID: Integer;
    Name: string;
    X, Y: SmallInt;
  end;
  PPortal = ^TPortal;

  TMonsterStats = record
    HP, PDRate: Integer;
  end;
  PMonsterStats = ^TMonsterStats;

  TLoadedLife = class
  protected
    FID, FObjectID: Integer;
  public
    property ID: Integer read FID;
    property ObjectID: Integer read FObjectID write FObjectID;
  end;

  TMonster = class(TLoadedLife)
  private
    FHP, FServerHP: Integer;
    FServerDead: Boolean;
    FStats: PMonsterStats;

    procedure SetServerDead(Value: Boolean);
  public
    constructor Create(AID: Integer; AStats: PMonsterStats);

    procedure Damage(Dmg: Integer);

    property HP: Integer read FHP write FHP;
    property ServerHP: Integer read FServerHP write FServerHP;
    property ServerDead: Boolean read FServerDead write SetServerDead;
    property Stats: PMonsterStats read FStats;
  end;

  TNPC = class(TLoadedLife)
  public
    constructor Create(AID: Integer);

    procedure InitTalk;
  end;

  TMapleMap = class
  private
    FID: Integer;
    FCharacters: TDictionary<Integer, string>;
    FFootholds: TFootholdTree;
    FMobLock: TCriticalSection;
    FMobs: TDictionary<Integer, TMonster>;
    FNPCs: TDictionary<Integer, TNPC>;
    FPortals: TDictionary<Integer, PPortal>;
    FObjectHistory: TDictionary<Integer, Integer>;
  public
    constructor Create(AID: Integer);
    destructor Destroy; override;

    procedure ClearObjects;

    procedure AddMonster(Mob: TMonster);
    procedure AddNPC(NPC: TNPC);
    procedure AddPortal(P: PPortal);
    function GetMonster(OID: Integer): TMonster;
    function GetPortal(const Name: string): PPortal;
    procedure RemoveMonster(OID: Integer);
    function NextMonster: TMonster;

    function GetAllMonsters: TList<TMonster>;
    function GetAttackableMonsters: TList<TMonster>;

    function ObjectToRealID(OID: Integer): Integer;

    property ID: Integer read FID;
    property Characters: TDictionary<Integer, string> read FCharacters;
    property Footholds: TFootholdTree read FFootholds write FFootholds;
    property NPCs: TDictionary<Integer, TNPC> read FNPCs;
    property Portals: TDictionary<Integer, PPortal> read FPortals;
  end;

implementation

uses DeathLoader, PacketIO;

{ TMapleMap }

constructor TMapleMap.Create(AID: Integer);
begin
  FID := AID;
  FMobLock := TCriticalSection.Create;
  FMobs := TDictionary<Integer, TMonster>.Create;
  FNPCs := TDictionary<Integer, TNPC>.Create;
  FPortals := TDictionary<Integer, PPortal>.Create;
  FCharacters := TDictionary<Integer, string>.Create;
  FObjectHistory := TDictionary<Integer, Integer>.Create;
end;

destructor TMapleMap.Destroy;
begin
  FCharacters.Free;
  FMobs.Free;
  FNPCs.Free;
  FPortals.Free;
  FMobLock.Free;
  FObjectHistory.Free;

  inherited;
end;

function TMapleMap.GetAllMonsters: TList<TMonster>;
var
  M: TMonster;
begin
  Result := TList<TMonster>.Create;
  FMobLock.Enter;
  try
    for M in FMobs.Values do
      Result.Add(M);
  finally
    FMobLock.Leave;
  end;
end;

function TMapleMap.GetAttackableMonsters: TList<TMonster>;
var
  M: TMonster;
begin
  Result := TList<TMonster>.Create;
  FMobLock.Enter;
  try
    for M in FMobs.Values do
      if (M.ID <> NO_MOB) and (M.ID <> AttackerID) and (not M.ServerDead) then
        Result.Add(M);
  finally
    FMobLock.Leave;
  end;
end;

function TMapleMap.GetMonster(OID: Integer): TMonster;
begin
  if not FMobs.TryGetValue(OID, Result) then
    Result := nil;
end;

function TMapleMap.GetPortal(const Name: string): PPortal;
begin
  for Result in FPortals.Values do
    if SameText(Result.Name, Name) then
      Exit;
  Result := nil;
end;
 function TMapleMap.NextMonster: TMonster;
var
  M: TMonster;
begin
  FMobLock.Enter;
  try
    for M in FMobs.Values do
      if (M.ID <> NO_MOB) and (M.ID <> AttackerID) and (not M.ServerDead) then
        Exit(M);

    Result := nil;
  finally
    FMobLock.Leave;
  end;
end;
procedure TMapleMap.ClearObjects;
begin
  FCharacters.Clear;
  FMobs.Clear;
  FNPCs.Clear;
end;

function TMapleMap.ObjectToRealID(OID: Integer): Integer;
begin
  if not FObjectHistory.TryGetValue(OID, Result) then
    Result := -1;
end;

procedure TMapleMap.AddMonster(Mob: TMonster);
begin
  if Mob.ID = AttackerID then
    AttackerOID := Mob.ObjectID;

  FMobLock.Enter;
  try
    FMobs.Add(Mob.ObjectID, Mob);
  finally
    FMobLock.Leave;
  end;
end;

procedure TMapleMap.AddNPC(NPC: TNPC);
begin
  FNPCs.Add(NPC.ID, NPC);
end;

procedure TMapleMap.AddPortal(P: PPortal);
begin
  FPortals.Add(P.ID, P);
end;

procedure TMapleMap.RemoveMonster(OID: Integer);
begin
  if OID = AttackerOID then
    AttackerOID := 0;

  FMobLock.Enter;
  try
    if FMobs.ContainsKey(OID) then
    begin
      FObjectHistory.AddOrSetValue(OID, TMonster(FMobs[OID]).ID);
      TMonster(FMobs[OID]).ServerDead := True;
      FMobs.Remove(OID);
    end;
  finally
    FMobLock.Leave;
  end;
end;

{ TMapleMonster }

constructor TMonster.Create(AID: Integer; AStats: PMonsterStats);
begin
  FID := AID;
  FStats := AStats;
  FHP := FStats.HP;
  FServerDead := False;
end;

procedure TMonster.Damage(Dmg: Integer);
begin
  Dec(FHP, Dmg);
end;

procedure TMonster.SetServerDead(Value: Boolean);
begin
  FServerDead := Value;
  if Value then
    Sched.AddSchedule(5000, procedure begin Free end);
end;

{ TNPC }

constructor TNPC.Create(AID: Integer);
begin
  FID := AID;
end;

procedure TNPC.InitTalk;
begin
  NPCTalk(FObjectID);
end;

end.
