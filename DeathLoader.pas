unit DeathLoader;

interface

uses Windows, Classes, SysUtils, Math, MapleStream, PacketIO, Opcodes, MapleMap,
     Generics.Collections, Main;

procedure SpawnAttackMob;
procedure SendMob2Mob(Mob: TMonster);

type
  TAttacker = class(TThread)
  private
    FMap: TMapleMap;
  protected
    procedure Execute; override;
  public
    constructor Create(Map: TMapleMap);
  end;

var
  R1, R2, R3: Cardinal;
  PADamage: Integer = 22000;
  AttackerID: Integer = 8820018;
  AttackerOID: Integer;
  Attacker: TAttacker = nil;

implementation

function MSRand: Cardinal;
begin
  R1 := ((R1 and $FFFFFFFE) shl 12) xor (((R1 shl 13) xor R1) shr 19);
  R2 := ((R2 and $FFFFFFF8) shl 4) xor (((R2 shl 2) xor R2) shr 25);
  R3 := ((R3 and $FFFFFFF0) shl 17) xor (((R3 shl 3) xor R3) shr 11);

  Result := R1 xor R2 xor R3;
end;

function CalcMobBaseDamage(Att, Seed: Integer): Double;
var
  v4, v5: Double;
begin
  v5 := Att * 0.85;
  v4 := Att - v5;
  Result := v4 * (Seed mod 10000000) * 0.000000100000010000001 + v5;
end;

function PDamage(Mob: TMonster): Integer;
var
  v4, Rate: Cardinal;
begin
  v4 := MSRand;
  Rate := Mob.Stats.PDRate;
  Result := Floor((100.0 - Rate) * CalcMobBaseDamage(PADamage, v4) * 0.01);
  //Log('%d (%d) : %d', [Mob.ObjectID, Mob.ID, Result]);
end;

procedure SpawnAttackMob;
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_SKILL);
    WriteInt(GetTickCount);
    WriteInt(30001062);   // Citizen.CallOfTheHunter
    WriteByte(1);
    WriteInt(AttackerID);
    WritePos(Character.Position);
    WriteShort(0);
  end;
  frmWvsFake.Write(Packet);
end;

procedure SendMob2Mob(Mob: TMonster);
var
  Packet: TMapleStream;
  Dmg: Integer;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_MOB_DAMAGE_MOB);
    WriteInt(AttackerOID);
    WriteInt(Character.ID);
    WriteInt(Mob.ObjectID);
    WriteByte(3);
    Dmg := PDamage(Mob);
    Mob.Damage(Dmg);
    WriteInt(Dmg);
    WriteByte(1);
    WritePos(Character.Position);
  end;
  frmWvsFake.Write(Packet);
end;

{ TAttacker }

constructor TAttacker.Create(Map: TMapleMap);
begin
  FMap := Map;

  inherited Create(False);
end;
{procedure TAttacker.Execute;
var
  M: TMonster;
begin
  FreeOnTerminate := True;
  SpawnAttackMob;
  Sleep(2000);
  Log('Attacker spawned. OID: %d', [AttackerOID]);
  while not Terminated do
  begin
    try
      while (AttackerOID > 0) and (not Terminated) do
      begin
        M := FMap.NextMonster;
        while (M <> nil) and (AttackerOID > 0) and (not Terminated) and (not M.ServerDead) do
        begin
          SendMob2Mob(M);
          Sleep(20);
        end;
      end;

      if Terminated then
        Exit;

      Sleep(100);

      SpawnAttackMob;
      Sleep(2000);
    except
      Log('TAttacker: ' + Exception(ExceptObject).Message);
    end;
  end;
end;
}


procedure TAttacker.Execute;
var
  M: TMonster;
  L: TList<TMonster>;
begin
  FreeOnTerminate := True;
  SpawnAttackMob;
  Sleep(2000);
  Log('Attacker spawned. OID: %d', [AttackerOID]);
  while not Terminated do
  begin
    try
      while (AttackerOID > 0) and (not Terminated) do
      begin
        L := FMap.GetAttackableMonsters;
        try
          for M in L do
            if (AttackerOID > 0) and (not Terminated) and (not M.ServerDead) then
            begin
              SendMob2Mob(M);
              Sleep(20);
            end;
        finally
          L.Free;
        end;
      end;

      if Terminated then
        Exit;

      Sleep(100);

      SpawnAttackMob;
      Sleep(2000);
    except
      Log('TAttacker: ' + Exception(ExceptObject).Message);
    end;
  end;
end;

end.
