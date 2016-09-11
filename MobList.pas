unit MobList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Generics.Collections;

type
  TfrmMobs = class(TForm)
    LVMobs: TListView;
    TmrUpdate: TTimer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TmrUpdateTimer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmMobs: TfrmMobs;

implementation

uses MapleMap, PacketIO;

{$R *.dfm}

procedure TfrmMobs.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TmrUpdate.Enabled := False;
end;

procedure TfrmMobs.FormShow(Sender: TObject);
begin
  TmrUpdate.Enabled := True;
end;

procedure TfrmMobs.TmrUpdateTimer(Sender: TObject);
var
  L: TList<TMonster>;
  M: TMonster;
begin
  L := Character.Map.GetAllMonsters;
  LVMobs.Items.BeginUpdate;
  LVMobs.Items.Clear;
  try
    for M in L do
      with LVMobs.Items.Add do
      begin
        Caption := IntToStr(M.ObjectID);
        SubItems.Add(IntToStr(M.ID));
        SubItems.Add(IntToStr(M.HP));
        SubItems.Add(IntToStr(M.ServerHP));
      end;
  finally
    L.Free;
    LVMobs.Items.EndUpdate;
  end;
end;

end.
