unit Movement;

interface

uses Types, Generics.Collections, MapleMap, MapleStream, Opcodes, Footholds, Math;

type
  TMovementType = (mtMove, mt9 = 9, mtSit = 11);
  TStance = (stWalkRight, stWalkLeft, stSlowWalkRight, stSlowWalkLeft,
             stStandRight, stStandLeft, stAirRight, stAirLeft,
             stBreathRight, stBreathLeft, stLieRight, stLieLeft, stSwimRight, stSwimLeft,
             stDeadRight = 18, stDeadLeft, stSitRight, stSitLeft);

function SpawnCharacter(Portal: PPortal; DoLog: Boolean = False): TMapleStream;

implementation

uses Main, PacketIO, SpawnPoint;

function MoveStd: TMapleStream;
begin
  Result := TMapleStream.Create;
  with Result do
  begin
    WriteShort(S_MOVE);
    WriteInt64(-1);
    WriteByte(Character.UsedPortals);
    WriteInt64(-1);
    WriteInt64(0);
    WriteInt(0);
  end;
end;

function CalcV(Old, New: SmallInt): SmallInt;
begin
  Result := Min(670, Round(Sqrt(Abs(Old - New))) * 60);  // 60 = FPS?
end;

procedure AddMovement(var P: TMapleStream; AType: TMovementType; AParams: array of const);

  function Param(i: Integer): Integer;
  begin
    Result := TVarRec(AParams[i]).VInteger;
  end;

begin
  P.WriteByte(AType);
  case AType of
    mtMove:
    begin
      P.WriteShort(Param(0));
      P.WriteShort(Param(1));
      P.WriteShort(Param(2));
      P.WriteShort(Param(3));
      P.WriteShort(Param(4));
      P.WriteInt(0);
      P.WriteByte(Param(5));
      P.WriteShort(Param(6));
    end;

    mt9: P.WriteByte(Param(0));
  end;
end;

function SpawnCharacter(Portal: PPortal; DoLog: Boolean): TMapleStream;
var
  Plat: TFoothold;
  PY: Integer;
  SP: TSpawnPoint;
begin
  Result := MoveStd;

  if frmSpawnPoint.Maps.TryGetValue(Character.Map.ID, SP) then
  begin
    New(Portal);
    Portal.X := SP.X;
    Portal.Y := SP.Y;
  end
  else
    SP := nil;

  PY := Portal.Y - 10;
  Result.WriteShort(Portal.X);
  Result.WriteShort(PY);
  Result.WriteInt(0);  // two shorts actually
  Result.WriteByte(2); // Count
  AddMovement(Result, mt9, [1]);
  Plat := Character.Map.Footholds.FindBelow(Point(Portal.X, PY));
  AddMovement(Result, mtMove, [Portal.X, Plat.Y1, 0, CalcV(PY, Plat.Y1), Plat.ID, Byte(stStandRight), 500]);
  Result.WriteShort(11);
  Result.WriteInt64(0);
  Result.WriteShort(Portal.X);
  Result.WriteShort(PY);
  Character.Position := Point(Portal.X, Plat.Y1);
  Result.WritePos(Character.Position);

  if DOLog then
  begin
    Log('First: %d | %d', [Portal.X, PY]);
    Log('End: %d | %d', [Character.Position.X, Character.Position.Y]);
  end;

  if Assigned(SP) then
    Dispose(Portal);
end;

end.
