unit DataCenter;

interface

uses SysUtils, Generics.Collections, MapleMap, WZArchive, WZDirectory, WZIMGFile,
     Footholds, Types, GameLogic, MapleItem, PNGImage, PNGMapleCanvas, Classes,
     CRC32, Forms;

type
  TItemData = class
  private
    FArchive, FCharWZ: TWZArchive;
    FConsumeCache: TDictionary<Integer, PConsumeInfo>;

    function GetEquipCRC(Info: TWZIMGEntry; c: Cardinal): Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    function CalcCRC(ID: Integer): Cardinal; overload;
    function CalcCRC(ID: Integer; Info: TWZIMGEntry): Cardinal; overload;

    function GetConsumeData(ID: Integer): PConsumeInfo;
    function GetImage(ID: Integer): TPNGImage;
    function GetInfoRoot(ID: Integer): TWZIMGEntry;
    function GetCanvas(ID: Integer; const Name: string): TPNGMapleCanvas;

    procedure SaveAllChecksums;
  end;

  TStringData = class
  private
    FArchive: TWZArchive;
    FMaps: TWZIMGFile;
  public
    constructor Create;
    destructor Destroy; override;

    function GetItemProp(ID: Integer; const PropName: string): string;
    function GetMapName(ID: Integer): string;
    procedure InitializeItems;
  end;

function LoadMap(ID: Integer): TMapleMap;
function GetMonster(ID: Integer): TMonster;

var
  ItemData: TItemData;
  StringData: TStringData;

implementation

uses Main, Items;

var
  MobCache: TDictionary<Integer, PMonsterStats>;

{ TItemData }

constructor TItemData.Create;
begin
  FArchive := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'Item.wz', nil);
  FCharWZ := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'Character.wz', nil);
  FConsumeCache := TDictionary<Integer, PConsumeInfo>.Create;
end;

destructor TItemData.Destroy;
var
  CI: PConsumeInfo;
begin
  FArchive.Free;
  FCharWZ.Free;
  for CI in FConsumeCache.Values do
    Dispose(CI);

  FConsumeCache.Free;

  inherited;
end;

function TItemData.GetConsumeData(ID: Integer): PConsumeInfo;
var
  Dir: TWZIMGEntry;
begin
  if FConsumeCache.TryGetValue(ID, Result) then
    Exit;

  with FArchive.GetImgFile(Format('Consume/%.4d.img', [ID div 10000])) do
  begin
    Dir := Root.Child[Format('%.8d', [ID])].Child['spec'];
    if Dir = nil then
    begin
      FConsumeCache.Add(ID, nil);
      Exit(nil);
    end;

    New(Result);
    Result.HP := Dir.Get('hp', 0);
    Result.MP := Dir.Get('mp', 0);
    Result.HPRate := Dir.Get('hpR', 0);
    Result.MPRate := Dir.Get('mpR', 0);
    FConsumeCache.Add(ID, Result);
  end;
end;

function TItemData.CalcCRC(ID: Integer): Cardinal;
var
  Info: TWZIMGEntry;
begin
  Info := ItemData.GetInfoRoot(ID);
  if Info = nil then
    Exit(0);

  Result := CalcCRC(ID, Info);
end;

function TItemData.CalcCRC(ID: Integer; Info: TWZIMGEntry): Cardinal;
var
  i, c, IconCRC, j, Flags: Cardinal;
  Img: TPNGMapleCanvas;
  B: TBytes;
  d: Double;
  s: string;

  procedure GetIconCRC;
  var
    W, H: Integer;
    LineSum: Cardinal;
    Data: TMemoryStream;
  begin
    Data := Img.Decompress;
    try
      W := Img.Width * 2;
      H := Img.Height;
      SetLength(B, W);
      while H > 0 do
      begin
        Dec(H);
        Data.Read(B[0], W);
        LineSum := CalcCRC32(@B[0], W);
        IconCRC := IconCRC xor LineSum;
      end;
    finally
      Data.Free;
    end;
  end;

begin
  i := MSVer;
  c := CalcCRC32(@i, 4);
  c := CalcCRC32(@c, 4);
  c := CalcCRC32(@ID, 4) xor c;

  IconCRC := 0;
  Img := ItemData.GetCanvas(ID, 'iconRaw');
  GetIconCRC();
  Img := ItemData.GetCanvas(ID, 'icon');
  GetIconCRC();
  c := IconCRC xor c;

  s := StringData.GetItemProp(ID, 'name') + StringData.GetItemProp(ID, 'desc');
  B := TEncoding.ASCII.GetBytes(s);
  if Length(B) > 0 then
  begin
    for i := 0 to High(B) do
      if s[i + 1] = '…' then   // Fix for Unicode->ASCII
        B[i] := $85;
    c := c xor CalcCRC32(@B[0], Length(B));
  end;
  //Log('%x', [c]);

  if GetInventory(ID) = miEquip then
    c := GetEquipCRC(Info, c)
  else
  begin
    i := Info.Get('price', 0);
    j := Info.Get('reqLevel', 0);
    d := Info.Get('unitPrice', 0);
    c := (CalcCRC32(@j, 4) or CalcCRC32(@i, 4) or CalcCRC32(@d, 8)) xor c;

    i := Info.Get('slotMax', 100);
    j := Info.Get('max', 0);  // 2270007
    c := (CalcCRC32(@i, 4) or CalcCRC32(@j, 4)) xor c;

    Flags := (Ord(Info.Get('noCancelMouse', 0) <> 0)
        + 2 * (Ord(Info.Get('expireOnLogout', 0) <> 0)
        + 2 * (Ord(Info.Get('notSale', 0) <> 0)
        + 2 * (Ord(Info.Get('tradeAvailable', 0) <> 0)
        + 2 * (Ord(Info.Get('tradeBlock', 0) <> 0)
        + 2 * (Ord(Info.Get('timeLimited', 0) <> 0)
        + 2 * (Ord(Info.Get('pquest', 0) <> 0)
        + 2 * Ord(Info.Get('quest', 0) <> 0))))))));
    c := CalcCRC32(@Flags, 4) xor c;
  end;

  //Log('%d: %x', [ID, c]);
  Result := c;
end;

function TItemData.GetEquipCRC(Info: TWZIMGEntry; c: Cardinal): Cardinal;
var
  i, j, k, Flags: Cardinal;
  d: Double;
begin
  i := Info.Get('reqSTR', 0);
  j := Info.Get('reqDEX', 0);
  k := Info.Get('reqINT', 0);
  c := (CalcCRC32(@i, 4) or CalcCRC32(@j, 4) or CalcCRC32(@k, 4)) xor c;

  j := Info.Get('reqLUK', 0);
  i := Info.Get('reqJob', 0);
  k := Info.Get('reqPOP', 0);
  c := (CalcCRC32(@j, 4) or CalcCRC32(@k, 4) or CalcCRC32(@i, 4)) xor c;
  //Log('%x', [c]);

  i := Info.Get('price', 0);
  j := Info.Get('reqLevel', 0);
  c := (CalcCRC32(@j, 4) or 0 or CalcCRC32(@i, 4)) xor c;
  //Log('%x', [c]);

  d := Info.Get('recovery', 1.0);
  i := CalcCRC32(@d, 8);
  d := Info.Get('fs', 1.0);
  j := CalcCRC32(@d, 8);
  c := (i or j) xor c;

  //Log('%x', [c]);

  i := Info.Get('knockback', 0);
  j := 100;
  c := (CalcCRC32(@i, 4) or CalcCRC32(@j, 4)) xor c;

  //Log('%x', [c]);

  Flags := Ord(Info.Get('epicItem', 0) <> 0)  // 218
       + 2 * (Ord(Info.Get('notExtend', 0) <> 0)          // 1D4
       + 2 * (Ord(Info.Get('expireOnLogout', 0) <> 0)     // 1BC
       + 2 * (Ord(Info.Get('notSale', 0) <> 0)            // 1B4
       + 2 * (Ord(Info.Get('accountSharable', 0) <> 0)    // 1D8
       + 2 * (Ord(Info.Get('tradeAvailable', 0) <> 0)     // 1B0
       + 2 * (Ord(Info.Get('tradeBlock', 0) <> 0)         // 1AC
       + 2 * (Ord(Info.Get('onlyEquip', 0) <> 0)          // 1A8
       + 2 * (Ord(Info.Get('only', 0) <> 0)               // 1A4
       + 2 * Ord(Info.Get('timeLimited', 0) <> 0))))))))); // 0C

  //Log('Flags: %d', [Flags]);

  Result := CalcCRC32(@Flags, 4) xor c;
end;

function TItemData.GetImage(ID: Integer): TPNGImage;
begin
  Result := GetCanvas(ID, 'icon').Dump;
end;

function TItemData.GetInfoRoot(ID: Integer): TWZIMGEntry;
var
  Iter: TWZDirectory;
  s: string;
begin
  case GetInventory(ID) of
    miEquip:
    begin
      s := Format('%.8d.img', [ID]);
      for Iter in FCharWZ.Root.SubDirs do
        if Iter.Entry[s] <> nil then
        begin
          with FCharWZ.ParseFile(TWZFile(Iter.Entry[s])) do
            Exit(Root.Child['info']);
        end;
      Exit(nil);  // wtf.
    end;

    miUse: s := 'Consume';
    miSetup: s := 'Install';
    miEtc: s := 'Etc';
    miCash: s := 'Cash';
  end;

  with FArchive.GetImgFile(Format('%s/%.4d.img', [s, ID div 10000])) do
    Result := Root.Get(Format('%.8d/info', [ID]));
end;

function NoIMG(const Name: string): Integer; inline;
begin
  Result := StrToInt(ChangeFileExt(Name, ''));
end;

procedure TItemData.SaveAllChecksums;
var
  Iter: TWZDirectory;
  FIter: TWZFile;
  EIter: TWZIMGEntry;
  ID: Integer;
  Sums: TDictionary<Integer, Cardinal>;
  Sort: TList<Integer>;
  s: string;
  F: TextFile;
label l;
begin
  Sums := TDictionary<Integer, Cardinal>.Create;

  for Iter in FCharWZ.Root.SubDirs do
    if (Iter.Name <> 'Afterimage') and (Iter.Name <> 'Hair') and (Iter.Name <> 'Face') then
    begin
      Log(Iter.Name);
      Application.ProcessMessages;
      for FIter in Iter.Files do
        with FCharWZ.ParseFile(FIter) do
        begin
          if Root.Get('info/cash', 0) > 0 then
            Continue;

          ID := NoIMG(FIter.Name);
          Sums.AddOrSetValue(ID, CalcCRC(ID, Root.Child['info']));
        end;
    end;

  s := 'Consume';
l:
  for FIter in TWZDirectory(FArchive.Root.Entry[s]).Files do
  begin
    Log(FIter.Name);
    Application.ProcessMessages;
    with FArchive.ParseFile(FIter) do
      for EIter in Root.Children do
      begin
        ID := NoIMG(EIter.Name);
        Sums.AddOrSetValue(ID, CalcCRC(ID, EIter.Child['info']));
      end;
  end;

  if s = 'Consume' then
  begin
    s := 'Install';
    goto l;
  end
  else if s = 'Install' then
  begin
    s := 'Etc';
    goto l;
  end;

  Sort := TList<Integer>.Create;
  for ID in Sums.Keys do
    Sort.Add(ID);
  Sort.Sort;

  AssignFile(F, ExtractFilePath(ParamStr(0)) + 'Items.txt');
  Rewrite(F);
  for ID in Sort do
    WriteLn(F, ID, '|',
      StringReplace(StringData.GetItemProp(ID, 'name'), #13#10, '', [rfReplaceAll]),
      '|', Ord(Assigned(frmItems.GetItem(ID))), Format('|$%x', [Sums[ID]]));
  CloseFile(F);
  Sums.Free;
  Sort.Free;
end;

function TItemData.GetCanvas(ID: Integer; const Name: string): TPNGMapleCanvas;
var
  Dir: TWZIMGEntry;
begin
  Dir := GetInfoRoot(ID);
  if (Dir = nil) or (Dir.Child[Name] = nil) then
    Exit(nil);

  Dir := Dir.Child[Name];

  if Dir.DataType = mdtUOL then
    Dir := TWZIMGEntry(Dir.Parent).Get(Dir.Data);

  if Dir.Canvas = nil then
  begin
    Log('Image Fail %d', [ID]);
    Exit(nil);
  end;

  Result := Dir.Canvas;
end;

{ TStringData }

constructor TStringData.Create;
begin
  FArchive := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'String.wz', nil);
  FMaps := FArchive.ParseFile(TWZFile(FArchive.Root.Entry['Map.img']));
end;

destructor TStringData.Destroy;
begin
  FMaps.Free;
  FArchive.Free;

  inherited;
end;

function TStringData.GetItemProp(ID: Integer; const PropName: string): string;
var
  DirName: string;
  SubDir, Dir: TWZIMGEntry;
begin
  Result := '';

  case GetInventory(ID) of
    miEquip: DirName := 'Eqp';
    miUse: DirName := 'Consume';
    miSetup: DirName := 'Ins';
    miEtc: DirName := 'Etc';
    miCash: DirName := 'Cash';
  end;

  with FArchive.ParseFile(TWZFile(FArchive.Root.Entry[DirName + '.img'])) do
  begin
    SubDir := Root;
    if (DirName = 'Eqp') or (DirName = 'Etc') then
      SubDir := Root.Child[DirName];

    if DirName = 'Eqp' then
    begin
      for Dir in SubDir.Children do
      begin
        Result := Dir.Get(IntToStr(ID) + '/' + PropName, '');
        if Result <> '' then
          Exit;
      end;
    end
    else
    begin
      Dir := SubDir.Child[IntToStr(ID)];
      if Dir <> nil then
      begin
        Dir := Dir.Child[PropName];
        // Nexon fucked up some property-names and their client does NOT ignore case (Child[] uses SameText())
        if (Dir <> nil) and (Dir.Name = PropName) then
          Result := Dir.Data;
      end;
    end;
  end;
end;

function TStringData.GetMapName(ID: Integer): string;
var
  Dir: TWZIMGEntry;
  SID: string;
begin
  Result := '';
  SID := IntToStr(ID);

  for Dir in FMaps.Root.Children do
  begin
    Result := Dir.Get(SID + '/mapName', '');
    if Result <> '' then
      Exit;
  end;
end;

procedure TStringData.InitializeItems;
begin
  DidInit := True;
 (* Log('Loading string data...');
  FArchive.ParseFile(TWZFile(FArchive.Root.Entry['Consume.img']));
  FArchive.ParseFile(TWZFile(FArchive.Root.Entry['Etc.img']));
  FArchive.ParseFile(TWZFile(FArchive.Root.Entry['Eqp.img']));  *)
end;

function LoadFootholds(FHRoot: TWZIMGEntry): TFootholdTree;
var
  FH: TFoothold;
  FHs: TList<TFoothold>;
  LBound, UBound: TPoint;
  Iter1, Iter2, Iter3: TWZIMGEntry;
  X1, X2, Y1, Y2: Integer;
begin
  FHs := TList<TFoothold>.Create;
  LBound := Point(10000, 10000);
  UBound := Point(-10000, -10000);
  if FHRoot <> nil then
    for Iter1 in FHRoot.Children do
      for Iter2 in Iter1.Children do
        for Iter3 in Iter2.Children do
        begin
          X1 := Iter3.Get('x1', 0);
          X2 := Iter3.Get('x2', 0);
          Y1 := Iter3.Get('y1', 0);
          Y2 := Iter3.Get('y2', 0);

          FH := TFoothold.Create(Point(X1, Y1), Point(X2, Y2), StrToInt(Iter3.Name));
          FH.Next := Iter3.Get('nextid', 0);
          FH.Prev := Iter3.Get('previousid', 0);

          if X1 < LBound.X then
            LBound.X := X1;
          if X2 > UBound.X then
            UBound.X := X2;
          if Y1 < LBound.Y then
            LBound.Y := Y1;
          if Y2 > UBound.Y then
            LBound.Y := Y2;

          FHs.Add(FH);
        end;

  Result := TFootholdTree.Create(LBound, UBound);
  for FH in FHs do
    Result.Insert(FH);

  FHs.Free;
end;

function LoadMap(ID: Integer): TMapleMap;
var
  Maps: TWZArchive;
  Continent: TWZDirectory;
  Iter: TWZImgEntry;
  P: PPortal;
begin
  Result := TMapleMap.Create(ID);

  Maps := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'Map.wz', nil);
  Continent := TWZDirectory(TWZDirectory(Maps.Root.Entry['Map']).Entry['Map' + IntToStr(ID div 100000000)]);
  with Maps.ParseFile(TWZFile(Continent.Entry[IntToStr(ID) + '.img'])) do
  begin
    if Root.Child['portal'] <> nil then
      for Iter in Root.Child['portal'].Children do
      begin
        New(P);
        P.ID := StrToInt(Iter.Name);
        P.Name := Iter.Get('pn', '');
        P.X := Iter.Get('x', 0);
        P.Y := Iter.Get('y', 0);
       (* P.ParamByName('dest').AsInteger := Iter.Get('tm', 0);
        P.ParamByName('destname').AsString := Iter.Get('tn', '');
        P.ParamByName('script').AsString := Iter.Get('script', '');    *)
        Result.AddPortal(P);
      end;

    Result.Footholds := LoadFootholds(Root.Child['foothold']);
  end;

  Maps.Free;
end;

function GetMonster(ID: Integer): TMonster;
var
  Mobs: TWZArchive;
  S: PMonsterStats;
begin
  if not MobCache.TryGetValue(ID, S) then
  begin
    Mobs := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'Mob.wz', nil);
    with Mobs.ParseFile(TWZFile(Mobs.Root.Entry[IntToStr(ID) + '.img'])) do
    begin
      New(S);
      S.HP := Root.Get('info/maxHP', 0);
      S.PDRate := Root.Get('info/PDRate', 0);

      Free;
    end;

    Mobs.Free;
    MobCache.Add(ID, S);
  end;

  Result := TMonster.Create(ID, S);
end;

initialization
  MobCache := TDictionary<Integer, PMonsterStats>.Create;
  StringData := TStringData.Create;
  ItemData := TItemData.Create;

finalization
  MobCache.Free;
  StringData.Free;
  ItemData.Free;

end.
