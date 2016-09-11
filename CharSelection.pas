unit CharSelection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PacketIO, MapleCharacter;

type
  TfrmCharSel = class(TForm)
    lbChars: TListBox;
    procedure lbCharsDblClick(Sender: TObject);
  public
    procedure AddCharacter(C: TMapleCharacter);
  end;

var
  frmCharSel: TfrmCharSel;

implementation

{$R *.dfm}

{ TfrmCharSel }

procedure TfrmCharSel.AddCharacter(C: TMapleCharacter);
begin
  lbChars.AddItem(C.ToString, C);
end;

procedure TfrmCharSel.lbCharsDblClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to lbChars.Items.Count - 1 do
    if not lbChars.Selected[i] then
      lbChars.Items.Objects[i].Free
    else
      Character := TMapleCharacter(lbChars.Items.Objects[i]);

  lbChars.Clear;
  Close;
end;

end.
