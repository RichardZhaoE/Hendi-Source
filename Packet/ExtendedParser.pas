unit ExtendedParser;

interface

uses SysUtils, MapleStream, MapleCharacter;

function ReadCharEntry(P: TMapleStream): TMapleCharacter;
procedure ReadCharStats(P: TMapleStream; C: TMapleCharacter);
procedure ReadCharLook(P: TMapleStream);

procedure ReadInventory(P: TMapleStream; C: TMapleCharacter);

implementation

uses Main, DataCenter, GameLogic;

function ReadCharEntry(P: TMapleStream): TMapleCharacter;
begin
  Result := TMapleCharacter.Create;
  ReadCharStats(P, Result);
  ReadCharLook(P);

  P.Skip(1);
  if P.ReadByte > 0 then
    P.Skip(16);
end;

function ReadExtendedSP(P: TMapleStream): TExtendedSP;
var
  i, Count, Adv: Byte;
begin
  Result := nil;
  Count := P.ReadByte;
  if Count = 0 then
    Exit;
  for i := 0 to Count - 1 do
  begin
    Adv := P.ReadByte;
    if Adv > High(Result) then
      SetLength(Result, Adv + 1);
    Result[Adv] := P.ReadByte;
  end;
end;

procedure ReadCharStats(P: TMapleStream; C: TMapleCharacter);
begin
  with C, P do
  begin
    ID := ReadInt;
    Name := ReadNullTerminatedString(13);
    Gender := ReadByte;
    SkinColor := ReadByte;
    Face := ReadInt;
    Hair := ReadInt;
    Skip(24);
    Level := ReadByte;
    Job := ReadShort;
    Str := ReadShort;
    Dex := ReadShort;
    Int := ReadShort;
    Luk := ReadShort;
    HP := ReadInt;
    MaxHP := ReadInt;
    MP := ReadInt;
    MaxMP := ReadInt;
    AP := ReadShort;
    if not IsExtendedSPJob then
      SP := ReadShort
    else
      ExtendedSP := ReadExtendedSP(P);
    Exp := ReadInt;
    Fame := ReadShort;
    Skip(4);
    MapID := ReadInt;
    Spawnpoint := ReadByte;
    Skip(6);
  end;
end;

procedure ReadCharLook(P: TMapleStream);
begin
  P.Skip(11);

  // Equips
  while P.ReadByte <> $FF do
    P.Skip(4);

  // Cash Equips
  while P.ReadByte <> $FF do
    P.Skip(4);

  P.Skip(16);
end;

procedure ReadInventory(P: TMapleStream; C: TMapleCharacter);
var
  T: TMapleInventoryType;
  Pos: Byte;

  procedure DoInv(i: TMapleInventoryType; Modifier: Byte = 0);
  begin
    Pos := P.ReadByte;
    if Pos = 0 then
      Exit;
    repeat
      C.Inventory[i].AddFromPacket(P, Pos + Modifier, False);
      Pos := P.ReadByte;
    until Pos = 0;
  end;

begin
  with P, C do
  begin
    C.Mesos := ReadInt;
    C.InitInventory(miEquipped, 100);
    for T := miEquip to miCash do
      C.InitInventory(T, ReadByte);

    Skip(8);  // Strange Magic

    DoInv(miEquipped);
    ReadByte;
    DoInv(miEquipped, 100);
    ReadByte;
    DoInv(miEquip);
    Skip(5);
    DoInv(miUse);
    DoInv(miSetup);
    DoInv(miEtc);
    DoInv(miCash);

    Skip(4);
  end;
end;

end.
