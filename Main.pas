unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, StdCtrls,
  ReadThread, MapleCrypt, MapleStream, DataCenter, ExtCtrls, FuckShield,
  SyncObjs, Scheduler, ShellAPI, Menus, DropDB, MapleItem, GameLogic,
  {$IFDEF DATASNAP}DBXDataSnap, DBXCommon, DB, SqlExpr, WideStrings, {$ENDIF}SyncServer;

type
  TMode = (UNDEFINED, CHARSEL, HACK, EVENT);

  TfrmWvsFake = class(TForm)
    mmLog: TMemo;
    Client: TIdTCPClient;
    pnlChat: TPanel;
    edtChat: TEdit;
    btnChat: TButton;
    Panel1: TPanel;
    lblLv: TLabel;
    lblName: TLabel;
    lblExp: TLabel;
    lblMesos: TLabel;
    mmLog2: TMemo;
    lblMP: TLabel;
    pmRight: TPopupMenu;
    EvolvingRing1: TMenuItem;
    EvolvingRingFirst1: TMenuItem;
    Debugmovement1: TMenuItem;
    lblMap: TLabel;
    invtest1: TMenuItem;
    gui1: TMenuItem;
    lblHP: TLabel;
    coinexploit1: TMenuItem;
    StopExploit1: TMenuItem;
    gbSettings: TGroupBox;
    cbServerCtrl: TCheckBox;
    btnConnect: TButton;
    cbCapCC: TCheckBox;
    btnBot: TButton;
    cbMDRush: TCheckBox;
    MobList1: TMenuItem;
    Savechecksumstofile1: TMenuItem;
    cbExit: TButton;
    cbRestart: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnChatClick(Sender: TObject);
    procedure mmLogDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mmLog2DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EvolvingRing1Click(Sender: TObject);
    procedure EvolvingRingFirst1Click(Sender: TObject);
    procedure Debugmovement1Click(Sender: TObject);
    procedure lblMapDblClick(Sender: TObject);
    procedure invtest1Click(Sender: TObject);
    procedure gui1Click(Sender: TObject);
    procedure cbServerCtrlClick(Sender: TObject);
    procedure coinexploit1Click(Sender: TObject);
    procedure StopExploit1Click(Sender: TObject);
    procedure MobList1Click(Sender: TObject);
    procedure Savechecksumstofile1Click(Sender: TObject);
    procedure cbExitClick(Sender: TObject);
    procedure cbRestartClick(Sender: TObject);
  private
    FReader: TReadThread;
    FServerCrypto, FClientCrypto: TMapleDecoder;
    FCache: TBytes;
    FDidLogin: Boolean;
    FFShield: TFuckShield;
    {$IFDEF DATASNAP}
    FSQLCon: TSQLConnection;
    {$ENDIF}

    procedure Cache(Data: TBytes; Len: Integer);
    procedure Process(var VBuffer: TBytes);
  public
    procedure Write(const Data: TMapleStream);
    procedure OnPacket(const Data: TMapleStream);
    procedure OnDisconnected;
    procedure Reconnect(NewIP: string; NewPort: Word);

    procedure SyncItemLog(Mob, Item: Integer);
    function SyncGetChannel(DoChange: Boolean): Integer;
  end;

procedure Log(const Msg: string); overload;
procedure Log(const Msg: string; Args: array of const); overload;
procedure Log2(const Msg: string; Args: array of const);

var
  frmWvsFake: TfrmWvsFake;
  LastReconnect: Cardinal;
  CLock: TCriticalSection;
  Mode: TMode = UNDEFINED;
  MSVer: Word = 0;
  DidInit: Boolean = False;

implementation

uses Login, PacketIO, Items, DeathLoader, MapleMap, MapleCharacter, Movement,
     SpawnPoint, InvGUI, MobList;

{$R *.dfm}

procedure TfrmWvsFake.btnChatClick(Sender: TObject);
begin
  SendChat(edtChat.Text);
  edtChat.Clear;
end;

procedure TfrmWvsFake.btnConnectClick(Sender: TObject);
begin
  Client.Connect;
  FReader := TReadThread.Create(Client, OnPacket);
end;

procedure Log(const Msg: string); overload;
begin
  frmWvsFake.mmLog.Lines.Add(TimeToStr(Now) + '  ' + Msg);
end;

procedure Log(const Msg: string; Args: array of const); overload;
begin
  frmWvsFake.mmLog.Lines.Add(TimeToStr(Now) + '  ' + Format(Msg, Args));
end;

procedure Log2(const Msg: string; Args: array of const);
begin
  if frmWvsFake.mmLog2.Lines.Count = 6 then
    frmWvsFake.mmLog2.Lines.Delete(0);
  frmWvsFake.mmLog2.Lines.Add(Format(Msg, Args));
end;

procedure TfrmWvsFake.OnDisconnected;
var
  Param: string;
begin
  CurChannel := -1;
  FFShield.Terminate;
  FFShield := nil;
  if Assigned(Attacker) then
  begin
    Attacker.Terminate;
    Attacker := nil
  end;

  Sched.AddSchedule(3000, procedure begin
    CloseFile(DropDB.DBFile);
    try
      FReader.Free;
    except
    end;
    FReader := nil;
    // Once it gets disconnected it's fucked up beyond all recognition D:
    Param := Format('%s %d %d %d', [frmLogin.Account.ID, Ord(cbServerCtrl.Checked), Ord(cbCapCC.Checked), Ord(cbMDRush.Checked)]);
    ShellExecute(0, 'open', PChar(ParamStr(0)), PChar(Param), PChar(ExtractFilePath(ParamStr(0))), SW_SHOW);
    ExitProcess(0);
  end);
end;

procedure TfrmWvsFake.OnPacket(const Data: TMapleStream);
var
  Buf: TBytes;
  SIV, CIV: Integer;
begin
  if (Data.ReadShort = Data.Size - 2) and (Data.Size in [13..17]) then
  begin
    MSVer := Data.ReadShort;
    Log('Hello - Version 0.' + IntToStr(MSVer));
    Data.Skip(Data.ReadShort);
    Data.Read(CIV, 4);
    Data.Read(SIV, 4);
    FServerCrypto := TMapleDecoder.Create(SIV, $FFFF - MSVer);
    FClientCrypto := TMapleDecoder.Create(CIV, MSVer);
    if not FDidLogin then
    begin
      if (ParamCount = 0) or (frmLogin.Account = nil) then
        frmLogin.Show
      else
        frmLogin.btnLogin.Click;
    end
    else
    begin
      PacketIO.PlayerLoggedIn;
      if FFShield = nil then
        FFShield := TFuckShield.Create(False);
    end;
    Exit;
  end;
  Data.Position := 0;

  SetLength(Buf, Data.Size);
  Data.Read(Buf[0], Data.Size);
  Process(Buf);
end;

procedure TfrmWvsFake.Cache(Data: TBytes; Len: Integer);
var
  L1: Integer;
begin
  L1 := Length(FCache);
  SetLength(FCache, Length(FCache) + Len);
  Move(Data[0], FCache[L1], Len);
end;

procedure TfrmWvsFake.cbExitClick(Sender: TObject);
begin
ExitProcess(0);
end;

procedure TfrmWvsFake.cbRestartClick(Sender: TObject);
begin
LastDC := GetTickCount;
frmWvsFake.OnDisconnected;
cbRestart.Enabled := false;
end;

procedure TfrmWvsFake.cbServerCtrlClick(Sender: TObject);
begin
  {$IFDEF DATASNAP}
  FSQLCon.Connected := cbServerCtrl.Checked;
  {$ENDIF}
end;

var
  EventStop: Boolean = False;

procedure TfrmWvsFake.coinexploit1Click(Sender: TObject);
var
  Status: Integer;
begin
  if Sender <> nil then
    EventStop := False;

  if not Character.Map.NPCs.ContainsKey(9010010) then
  begin
    Log('Cassandra not found');
    Exit;
  end;

  Mode := EVENT;

  Status := -1;
  Character.OnNPCTalk := procedure(Mode: Byte)
  begin
    Inc(Status);
    if (Mode = 5) and (Status in [0, 1]) then
      NPCTalkMore(5, 1, 0)
    else if (Mode = 5) and (Status = 2) then
      NPCTalkMore(5, 1, 2060000)
    else if (Status = 3) and (Mode = 2) then
      NPCTalkMore(2, 1)
    else   // end conversation
    begin
      NPCTalkMore(Mode, $FF);
      if not EventStop then
        coinexploit1Click(nil);
    end;
  end;

  TNPC(Character.Map.NPCs[9010010]).InitTalk;
end;

procedure TfrmWvsFake.Debugmovement1Click(Sender: TObject);
var
  M, P: string;
begin
  M := InputBox('Movement Debug', 'Map ID:', '');
  if M = '' then
    Exit;

  P := InputBox('Movement Debug', 'Portal ID:', '');
  if P = '' then
    Exit;

  Character := TMapleCharacter.Create;
  Character.MapID := StrToInt(M);
  Character.LoadMap;
  Movement.SpawnCharacter(Character.Map.Portals[StrToInt(P)], True).Free;
end;

procedure TfrmWvsFake.EvolvingRing1Click(Sender: TObject);
begin
  if not Character.Map.NPCs.ContainsKey(9000021) then
  begin
    Log('FATAL: NPC not on map');
    Exit;
  end;

  Mode := EVENT;

  Character.OnNPCTalk := procedure(Mode: Byte)
  begin
    if Mode = 5 then
      NPCTalkMore(5, 1, 2)
    else if Mode = 0 then
      NPCTalkMore(0, 1);
  end;

  TNPC(Character.Map.NPCs[9000021]).InitTalk;
end;

procedure TfrmWvsFake.EvolvingRingFirst1Click(Sender: TObject);
begin
  if not Character.Map.NPCs.ContainsKey(9000021) then
  begin
    Log('FATAL: NPC not on map');
    Exit;
  end;

  Mode := EVENT;

  Character.OnNPCTalk := procedure(Mode: Byte)
  begin
    if Mode = 5 then
      NPCTalkMore(5, 1, 1)
    else if Mode = 0 then
      NPCTalkMore(0, 1)
    else if Mode = 2 then
      NPCTalkMore(2, 1);
  end;

  TNPC(Character.Map.NPCs[9000021]).InitTalk;
end;

procedure TfrmWvsFake.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Client.Connected then
    Client.Disconnect;

  if FReader <> nil then
    FReader.Terminate;

  if FFShield <> nil then
    FFShield.Terminate;
end;

procedure TfrmWvsFake.FormCreate(Sender: TObject);
begin
  CLock := TCriticalSection.Create;
  FCache := nil;
  FFShield := nil;
  FDidLogin := False;
  Randomize;

  {$IFDEF DATASNAP}
  FSQLCon := TSQLConnection.Create(Self);
  FSQLCon.DriverName := 'Datasnap';
  FSQLCon.ConnectionData.Properties.Values['HostName'] := 'localhost';
  FSQLCon.ConnectionData.Properties.Values['Port'] := '8485';
  FSQLCon.LoginPrompt := False;
  {$ELSE}
  cbServerCtrl.Visible := False;
  {$ENDIF}
end;

procedure TfrmWvsFake.FormDestroy(Sender: TObject);
begin
  CLock.Free;
  {$IFDEF DATASNAP}
  FSQLCon.Free;
  {$ENDIF}
end;

procedure TfrmWvsFake.FormShow(Sender: TObject);
begin
  if ParamCount >= 1 then
  begin
    if (ParamCount >= 2) and Boolean(StrToInt(ParamStr(2))) then
      cbServerCtrl.Checked := True;
    if (ParamCount >= 3) and Boolean(StrToInt(ParamStr(3))) then
      cbCapCC.Checked := True;
    if (ParamCount >= 4) and Boolean(StrToInt(ParamStr(4))) then
      cbMDRush.Checked := True;

    Mode := HACK;
    RestartAtt := True;
    StringData.InitializeItems;
    btnConnect.Click;
  end;
end;

procedure TfrmWvsFake.gui1Click(Sender: TObject);
begin
  frmInvGUI.Show;
end;

procedure TfrmWvsFake.invtest1Click(Sender: TObject);
var
  I: MapleItem.TItem;
  CI: PConsumeInfo;
begin
  for I in Character.Inventory[miUse] do
  begin
    CI := ItemData.GetConsumeData(I.ID);
    if CI = nil then
      Continue;

    if (CI.MP > 0) or (CI.MPRate > 0) then
      Log(IntTOStr(I.ID));
  end;
end;

procedure TfrmWvsFake.lblMapDblClick(Sender: TObject);
begin
  frmSpawnPoint.Show;
end;

procedure TfrmWvsFake.mmLog2DblClick(Sender: TObject);
begin
  frmItems.Show;
end;

procedure TfrmWvsFake.mmLogDblClick(Sender: TObject);
begin
  if Attacker = nil then
  begin
    Mode := HACK;
    if not DidInit then
      StringData.InitializeItems;
    Attacker := TAttacker.Create(Character.Map);
  end
  else
  begin
    Attacker.Terminate;
    Attacker := nil;
    Mode := UNDEFINED;
  end;
end;

procedure TfrmWvsFake.MobList1Click(Sender: TObject);
begin
  frmMobs.Show;
end;

procedure TfrmWvsFake.Process(var VBuffer: TBytes);
var
  Len, RealLen: Integer;
  Buf: TBytes;
  Data: TMapleStream;
begin
  Len := Length(VBuffer);

  if FCache <> nil then
  begin
    // Make room for cache in array
    SetLength(VBuffer, Length(VBuffer) + Length(FCache));
    Move(VBuffer[0], VBuffer[Length(FCache)], Length(VBuffer) - Length(FCache));
    // Copy cache before new data
    Move(FCache[0], VBuffer[0], Length(FCache));
    Inc(Len, Length(FCache));
    FCache := nil;
  end;

  if (Len = 0) or (not FServerCrypto.CheckPacket(VBuffer)) then
  begin
    if Len > 0 then
      Log('Packet check failed!');
    Exit;
  end;

  // More than 1 packet can be received with one call, so handle them all
  while Len > 0 do
  begin
    if not Assigned(FServerCrypto) then
      Exit;

    RealLen := TMapleDecoder.GetPacketLength(VBuffer);
    if Len - 4 < RealLen then
    begin
      Cache(VBuffer, Len);
      Break;
    end;

    // Decrypt incoming data and add it to Res
    Buf := Copy(VBuffer, 4, RealLen);   // packet data without the 4 byte header
    CLock.Enter;
    try
      if not Assigned(FServerCrypto) then
        Exit;

      FServerCrypto.DecryptPacket(Buf);
    finally
      CLock.Leave;
    end;

    Data := TMapleStream.Create;
    try
      Data.Write(Buf[0], Length(Buf));
      Data.Position := 0;
      HandlePacket(Data);
    finally
      Data.Free;
    end;

    // Remove first packet from array
    Move(VBuffer[RealLen + 4], VBuffer[0], Length(VBuffer) - (RealLen + 4));
    SetLength(VBuffer, Length(VBuffer) - (RealLen + 4));
    Dec(Len, RealLen + 4);  // packet with it's 4 byte header was removed
  end;

  VBuffer := nil;
end;

procedure TfrmWvsFake.Reconnect(NewIP: string; NewPort: Word);
begin
  Log('RECONNECT');

  try
    FReader.Terminate;
  except
  end;

  try
    Client.Disconnect;
  except
  end;
  while Client.Connected do
    Application.ProcessMessages;

  LastReconnect := GetTickCount;

  if Assigned(Recon) then
  begin
    try
      Recon.Terminate;
    except
    end;
    Recon := nil;
  end;

  CLock.Enter;
  try
    FreeAndNil(FServerCrypto);
    FreeAndNil(FClientCrypto);
  finally
    CLock.Leave;
  end;
  FCache := nil;

  FDidLogin := True;
  Client.Host := NewIP;
  Client.Port := NewPort;
  Client.Connect;
  FReader := TReadThread.Create(Client, OnPacket);
end;

procedure TfrmWvsFake.Savechecksumstofile1Click(Sender: TObject);
var
  s: string;
begin
  if MSVer = 0 then
  begin
    s := InputBox('MapleVersion', 'MapleVersion:', '');
    if s = '' then
      Exit;
    MSVer := StrToInt(s);
  end;

  ItemData.SaveAllChecksums;
end;

procedure TfrmWvsFake.StopExploit1Click(Sender: TObject);
begin
  EventStop := True;
end;

function TfrmWvsFake.SyncGetChannel(DoChange: Boolean): Integer;
begin
{$IFDEF DATASNAP}
  with TSyncClient.Create(FSQLCon.DBXConnection) do
  begin
    Result := GetChannel(DoChange, CurChannel);
    Free;
  end;
{$ELSE}
  Result := -1;
{$ENDIF}
end;

procedure TfrmWvsFake.SyncItemLog(Mob, Item: Integer);
begin
{$IFDEF DATASNAP}
  with TSyncClient.Create(FSQLCon.DBXConnection) do
  begin
    AddItem(Mob, Item, Character.Name);
    Free;
  end;
{$ENDIF}
end;



procedure TfrmWvsFake.Write(const Data: TMapleStream);
var
  Buf: TBytes;
begin
  Data.Position := 0;
  SetLength(Buf, Data.Size);
  Data.Read(Buf[0], Data.Size);
  try
    CLock.Enter;
    try
      if not Assigned(FClientCrypto) then
        Exit;

      FClientCrypto.EncryptPacket(Buf);
    finally
      CLock.Leave;
    end;
    try
      Client.IOHandler.Write(Buf);
    except
    end;
  finally
    Buf := nil;
    Data.Free;
  end;
end;

end.
