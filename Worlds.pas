unit Worlds;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, IniFiles;

type
  TfrmWorlds = class(TForm)
    cbWorlds: TComboBoxEx;
    cbSave: TCheckBox;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
  public
    function GetWorld: ShortInt;
  end;

var
  frmWorlds: TfrmWorlds;

implementation

{$R *.dfm}

uses Login;

{ TfrmWorlds }

procedure TfrmWorlds.FormCreate(Sender: TObject);
begin
  cbWorlds.ItemIndex := 0;
end;

function TfrmWorlds.GetWorld: ShortInt;
var
  Ini: TIniFile;
begin
  if frmLogin.Account.World > -1 then
    Exit(frmLogin.Account.World);

  Caption := Format('%s - World Selection', [frmLogin.Account.ID]);

  if ShowModal <> mrOK then
    Exit(-1);

  Result := cbWorlds.ItemIndex;

  if not cbSave.Checked then
    Exit;

  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'WvsFake.ini');
  try
    Ini.WriteInteger(frmLogin.Account.ID, 'WorldID', Result);
  finally
    Ini.Free;
  end;
end;

end.
