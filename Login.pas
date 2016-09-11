unit Login;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PacketIO, Auth, IniFiles, DropDB;

type
  TAccount = class
  public
    ID, PW, PIC: string;
    World: Int8;

    constructor Create(const AID, APW, APIC: string);
  end;

  TfrmLogin = class(TForm)
    btnLogin: TButton;
    cbCharSel: TCheckBox;
    cbAccs: TComboBox;
    btnAdd: TButton;
    btnRemove: TButton;
    procedure btnLoginClick(Sender: TObject);
    procedure edtPWKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure cbCharSelClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure cbAccsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure LoadAccounts;
    procedure SaveAccounts;
  public
    Account: TAccount;
  end;

var
  frmLogin: TfrmLogin;

implementation

uses Main;

{$R *.dfm}

function TextGet(const Caption: string; out Res: string): Boolean;
begin
  Res := InputBox(Caption, Caption + ':', '');
  Result := Length(Res) >= 4;
end;

procedure TfrmLogin.btnAddClick(Sender: TObject);
var
  ID, PW, PIC: string;
  Acc: TAccount;
begin
  if TextGet('Login ID', ID) and TextGet('Password', PW) and TextGet('PIC', PIC) then
  begin
    Acc := TAccount.Create(ID, PW, PIC);
    cbAccs.Items.AddObject(ID, Acc);
  end;
end;

procedure TfrmLogin.btnLoginClick(Sender: TObject);
var
  Cok: string;
begin
  if Account = nil then
    raise Exception.Create('SCREW YOU!');

  PacketIO.AtLoginScreen;
  Cok := Auth.GetCookieFromNexon(Account.ID, Account.PW);
  if Cok = '' then
    raise Exception.Create('Username/Password wrong');
  PacketIO.Login(Account.PW, Cok);

  if Showing then
    Close;

  try
    DropDB.Load;
  except
    Log('Loading the drop database failed.');
  end;
end;

procedure TfrmLogin.btnRemoveClick(Sender: TObject);
begin
  cbAccs.DeleteSelected;
end;

procedure TfrmLogin.cbAccsChange(Sender: TObject);
begin
  if cbAccs.ItemIndex > -1 then
    Account := TAccount(cbAccs.Items.Objects[cbAccs.ItemIndex]);
end;

procedure TfrmLogin.cbCharSelClick(Sender: TObject);
begin
  if cbCharSel.Checked then
    Mode := CHARSEL
  else
    Mode := UNDEFINED;
end;

procedure TfrmLogin.edtPWKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Ord(Key) = VK_RETURN then
  begin
    btnLogin.Click;
    Key := 0;
  end;
end;

procedure TfrmLogin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveAccounts;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  LoadAccounts;
  if ParamCount > 0 then
  begin
    cbAccs.ItemIndex := cbAccs.Items.IndexOf(ParamStr(1));
    Account := TAccount(cbAccs.Items.Objects[cbAccs.ItemIndex]);
  end;
end;

procedure TfrmLogin.LoadAccounts;
var
  Ini: TIniFile;
  SL: TStringList;
  ID: string;
  Acc: TAccount;
  Act: Integer;
begin
  Act := -1;
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'WvsFake.ini');
  SL := TStringList.Create;
  try
    Ini.ReadSections(SL);
    for ID in SL do
    begin
      if ID = 'General' then
      begin
        Act := Ini.ReadInteger(ID, 'ActiveAcc', -1);
        Continue;
      end;

      Acc := TAccount.Create(ID, Ini.ReadString(ID, 'PW', ''), Ini.ReadString(ID, 'PIC', ''));
      Acc.World := Ini.ReadInteger(ID, 'WorldID', -1);
      cbAccs.AddItem(ID, Acc);
    end;
  finally
    Ini.Free;
    SL.Free;
  end;

  if Act <> -1 then
  begin
    cbAccs.ItemIndex := Act;
    Account := TAccount(cbAccs.Items.Objects[Act]);
  end
  else
    Account := nil;
end;

procedure TfrmLogin.SaveAccounts;
var
  Ini: TIniFile;
  SL: TStringList;
  ID: string;
  Acc: TAccount;
  i: Integer;
begin
  Ini := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'WvsFake.ini');
  SL := TStringList.Create;
  try
    Ini.ReadSections(SL);
    for ID in SL do
      if ID <> 'General' then
        Ini.EraseSection(ID);

    Ini.WriteInteger('General', 'ActiveAcc', cbAccs.ItemIndex);
    for i := 0 to cbAccs.Items.Count - 1 do
    begin
      Acc := TAccount(cbAccs.Items.Objects[i]);
      Ini.WriteString(Acc.ID, 'PW', Acc.PW);
      Ini.WriteString(Acc.ID, 'PIC', Acc.PIC);
      Ini.WriteInteger(Acc.ID, 'WorldID', Acc.World);
    end;
  finally
    Ini.Free;
    SL.Free;
  end;
end;

{ TAccount }

constructor TAccount.Create(const AID, APW, APIC: string);
begin
  ID := AID;
  PW := APW;
  PIC := APIC;
  World := -1;
end;

end.
