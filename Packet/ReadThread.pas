unit ReadThread;

interface

uses
  SysUtils, Classes, Windows, IdTCPClient, MapleStream, IdStack;

type
  TOnReceive = procedure(const Data: TMapleStream) of object;

  TReadThread = class(TThread)
  private
    FClient: TIdTCPClient;
    FOnReceive: TOnReceive;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient; AOnReceive: TOnReceive);
  end;

var
  LastDC: Cardinal = 0;

implementation

uses Main;

{ TReadThread }

constructor TReadThread.Create(AClient: TIdTCPClient; AOnReceive: TOnReceive);
begin
  inherited Create(False);

  FreeOnTerminate := False;
  FClient := AClient;
  FOnReceive := AOnReceive;
end;

procedure TReadThread.Execute;
var
  Data: TMapleStream;
begin
  try
    while (not Terminated) and (FClient.Connected) do
    begin
      if not FClient.IOHandler.InputBufferIsEmpty then
      begin
        Data := TMapleStream.Create;
        try
          FClient.IOHandler.InputBufferToStream(Data);
          Data.Position := 0;
          Synchronize( procedure begin FOnReceive(Data); end );
        finally
          Data.Free;
        end;
      end;
      SleepEx(1, True);    // avoid high cpu usage
    end;
  except
    if ExceptObject is EIdSocketError then
      try
        Terminate;
      except
      end;
    frmWvsFake.mmLog.Lines.Add('Exception while reading ' + Exception(ExceptObject).Message);
    if (Pos('10054', Exception(ExceptObject).Message) > 0) and (LastDC < GetTickCount - 15000) then
    begin
      LastDC := GetTickCount;
      Synchronize(procedure begin frmWvsFake.OnDisconnected end);
    end
    else if (Pos('10053', Exception(ExceptObject).Message) > 0) and (LastDC < GetTickCount - 15000) then
    begin
      LastDC := GetTickCount;
      Synchronize(procedure begin frmWvsFake.OnDisconnected end);
    end;
  end;
end;

end.
