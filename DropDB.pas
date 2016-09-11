unit DropDB;

interface

uses Classes, SysUtils, Generics.Collections, Tools;

procedure Add(Mob, Item: Integer; ToFile: Boolean = True);
function Contains(Mob, Item: Integer): Boolean;
procedure Load;

var
  Drops: TDictionary<Integer, TList<Integer>>;
  DBFile: TextFile;

implementation

uses Login;

procedure Add(Mob, Item: Integer; ToFile: Boolean = True);
begin
  if not Drops.ContainsKey(Mob) then
    Drops.Add(Mob, TList<Integer>.Create);

  TList<Integer>(Drops[Mob]).Add(Item);

  if ToFile then
  begin
    WriteLn(DBFile, Format('%d, %d', [Mob, Item]));
    Flush(DBFile);
  end;
end;

function Contains(Mob, Item: Integer): Boolean;
begin
  if (Item = 2430026) or (Item = 2049301) or (Item = 2049401) or (Item div 10000 = 399) or
     (Mob = 9300340) or (Item = 4001126) or (Item = 4001165) or (Item = 4280001) or
     (Item = 4032658) or (Item = 4032659) then
    Exit(True);  // block

  Result := Drops.ContainsKey(Mob) and TList<Integer>(Drops[Mob]).Contains(Item);
end;

procedure Load;
var
  F: string;
  SL: TStringList;
  i: Integer;
  S: TStringArray;
begin
  F := ExtractFilePath(ParamStr(0)) + Format('DropDB_%s.txt', [frmLogin.Account.ID]);
  AssignFile(DBFile, F);
  if FileExists(F) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(F);
      for i := 0 to SL.Count - 1 do
      begin
        if Length(SL[i]) < 4 then
          Continue;

        S := Explode(', ', SL[i]);
        Add(StrToInt(S[0]), StrToInt(S[1]), False);
        S := nil;
      end;
    finally
      SL.Free;
    end;

    Append(DBFile);
  end
  else
    Rewrite(DBFile);
end;

procedure FreeDrops; inline;
var
  L: TList<Integer>;
begin
  for L in Drops.Values do
    L.Free;

  Drops.Free;
end;

initialization
  Drops := TDictionary<Integer, TList<Integer>>.Create;

finalization
  FreeDrops;

end.
