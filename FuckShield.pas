unit FuckShield;

interface

uses Windows, Classes;

type
  TFuckShield = class(TThread)
  protected
    procedure Execute; override;
  end;

  TReconnector = class(TThread)
  protected
    procedure Execute; override;
  private
    FOtherChar: Integer;
    FNewChannel: Int8;
  public
    constructor Create(OtherCharID: Integer = -1);

    property Destination: Int8 read FNewChannel;
  end;

var
  Recon: TReconnector = nil;

implementation

uses Main, PacketIO, DeathLoader;

{ TFuckShield }

procedure TFuckShield.Execute;
begin
  FreeOnTerminate := True;

  while not Terminated do
  begin
    if LastReconnect <= GetTickCount - 200000 then
      if not Assigned(Recon) then
        Recon := TReconnector.Create;

    Sleep(3000);
  end;
end;

{ TReconnector }

constructor TReconnector.Create(OtherCharID: Integer = -1);
begin
  Log('TReconnector.Create');
  FOtherChar := OtherCharID;
  inherited Create(False);
end;

procedure TReconnector.Execute;
var
  TryCount: Integer;
begin
  FreeOnTerminate := True;

  if frmWvsFake.cbServerCtrl.Checked then
    FNewChannel := frmWvsFake.SyncGetChannel(True)
  else
    repeat
      if frmWvsFake.cbCapCC.Checked then
        FNewChannel := Random(13)
      else
        FNewChannel := Random(19);
    until CurChannel <> FNewChannel;

  if Assigned(Attacker) then
  begin
    RestartAtt := True;
    Attacker.Terminate;
    Attacker := nil;
  end;

  Log('TReconnector.Execute Cur[%d] New[%d] Other[%d]', [CurChannel, FNewChannel, FOtherChar]);

  TryCount := 0;
  while (not Terminated) and (CurChannel > -1) and (CurChannel <> FNewChannel) do
  begin
    if TryCount >= 10 then
    begin
      frmWvsFake.OnDisconnected;
      Exit;
    end;

    if (FOtherChar > -1) and (not Character.Map.Characters.ContainsKey(FOtherChar)) and (not frmWvsFake.cbServerCtrl.Checked) then
    begin
      Log('Did not change channel as other char is a botter.');

      if RestartAtt then
      begin
        RestartAtt := False;
        Attacker := TAttacker.Create(Character.Map);
      end;

      Recon := nil;
      Exit;
    end;

    Log('Requesting channel-change to %d', [FNewChannel + 1]);  // Server counts from 0, not 1

    PacketIO.RequestChannelChange(FNewChannel);
    Sleep(1000);

    Inc(TryCount);
  end;
end;

end.
