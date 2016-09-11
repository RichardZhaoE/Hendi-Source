unit InvGUI;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, WZArchive, WZDirectory, WZIMGFile, ExtCtrls, PNGImage, PNGMapleCanvas,
  Generics.Collections, Math, MapleItem, GameLogic, Inventory, StdCtrls;

type
  TScrollDirection = (sdDown, sdUp);
  TOnScroll = procedure(Sender: TObject; Direction: TScrollDirection) of object;

  TWZControl = class
  protected
    FEntry: TWZIMGEntry;
  end;

  TWZButton = class(TWZControl)
  private
    FX, FY, FWidth, FHeight: SmallInt;
    FOnClick: TNotifyEvent;
  public
    constructor Create(AOnClick: TNotifyEvent; AEntry: TWZIMGEntry); overload;
    constructor Create(AOnClick: TNotifyEvent; AEntry: TWZIMGEntry; AX, AY: SmallInt); overload;
    destructor Destroy; override;

    function CheckClick(X, Y: Integer; Up: Boolean): Boolean;
    procedure Paint;
  end;

  TWZTabControl = class(TWZControl)
  private
    FIndex: Integer;
    FTabs: array of TWZButton;

    procedure TabClick(Sender: TObject);
  public
    constructor Create(AEntry: TWZIMGEntry);
    destructor Destroy; override;

    procedure Paint;

    property Index: Integer read FIndex;
  end;

  TWZScrollBar = class(TWZControl)
  private
    FOnScroll: TOnScroll;
    FBtnUp, FBtnDown: TWZButton;

    procedure UpClick(Sender: TObject);
    procedure DownClick(Sender: TObject);
  public
    constructor Create(AOnScroll: TOnScroll; AEntry: TWZIMGEntry; AX, AY, AHeight: SmallInt);
    destructor Destroy; override;

    procedure Paint;
  end;

  TfrmInvGUI = class(TForm)
    Window: TImage;
    lblItemName: TLabel;
    lblMesos: TLabel;
    procedure FormShow(Sender: TObject);
    procedure WindowMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure WindowMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure WindowMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    FUI: TWZArchive;
    btnClose: TWZButton;
    tcTabs: TWZTabControl;
    FScroller: TWZScrollBar;
    FButtons: TList<TWZButton>;
    FViewStart, FViewEnd: Int8;
    FDigits: array['0'..'9'] of TPNGImage;

    procedure CloseClick(Sender: TObject);
    procedure Scroll(Sender: TObject; Direction: TScrollDirection);

    function CheckClick(X, Y: Integer; Up: Boolean): Boolean;
    procedure DrawItem(I: TItem);
    procedure DrawMesos(M: Integer);

    function GetCurInv: TInventory;
  public
    procedure DoPaint;
    procedure Draw(X, Y: Integer; P: TPNGImage; AutoFree: Boolean = True);

    property Buttons: TList<TWZButton> read FButtons;
  end;

var
  frmInvGUI: TfrmInvGUI;

implementation

uses DataCenter, Main, PacketIO;

{$R *.dfm}

function TfrmInvGUI.CheckClick(X, Y: Integer; Up: Boolean): Boolean;
var
  B: TWZButton;
begin
  Result := False;
  for B in FButtons do
    if B.CheckClick(X, Y, Up) then
      Exit(True);
end;

procedure TfrmInvGUI.CloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmInvGUI.Draw(X, Y: Integer; P: TPNGImage; AutoFree: Boolean = True);
begin
  TPNGImage(Window.Picture.Graphic).Canvas.Draw(X, Y, P);
  if AutoFree then
    P.Free;
end;

procedure TfrmInvGUI.DrawItem(I: TItem);
var
  Row, X, Y, m: Int16;
  P: TPNGImage;
  Slot: Byte;
  Q: string;
  C: Char;
begin
  P := ItemData.GetImage(I.ID);
  if P = nil then
    Exit;

  Slot := I.Position;
  Dec(Slot, FViewStart);

  Row := Ceil(Slot / 4) - 1;

  m := Slot mod 4;
  if m > 0 then
    X := 12 + (m - 1) * 36
  else
    X := 120;  // 4th slot in row

  Y := 55 + Row * 35;

  Draw(X, Y, P);

  if I.ItemType <> itItem then
    Exit;

  Q := IntToStr(I.Quantity);
  for C in Q do
  begin
    Draw(X, Y + 18, FDigits[C], False);
    Inc(X, FDigits[C].Width);
  end;
end;

procedure TfrmInvGUI.DrawMesos(M: Integer);
var
  S: string;
begin
  S := IntToStr(M);
  if Length(S) >= 4 then
    Insert(',', S, Length(S) - 2);
  if Length(S) >= 7 then
    Insert(',', S, Length(S) - 6);

  lblMesos.Caption := S;
end;

procedure TfrmInvGUI.DoPaint;
var
  I: TItem;

  procedure Add(I: TWZIMGEntry);
  begin
    Draw(Abs(I.Child['origin'].Vector.X), Abs(I.Child['origin'].Vector.Y), I.Canvas.Dump);
  end;

begin
  with FUI.ParseFile(TWZFile(FUI.Root.Entry['UIWindow2.img'])) do
    with Root.Child['item'] do
    begin
      if Assigned(Window.Picture.Graphic) then
        Window.Picture.Graphic.Free;

      Window.Picture.Assign(Child['backgrnd'].Canvas.Dump);
      Add(Child['backgrnd2']);
      if not Assigned(tcTabs) then
        tcTabs := TWZTabControl.Create(Child['Tab'])
      else
        tcTabs.Paint;
      Add(Child['backgrnd3']);
    end;

  with FUI.ParseFile(TWZFile(FUI.Root.Entry['Basic.img'])) do
  begin
    if not Assigned(btnClose) then
      btnClose := TWZButton.Create(CloseClick, Root.Child['BtClose3'], Width - 22, 7)
    else
      btnClose.Paint;

    if not Assigned(FScroller) then
      FScroller := TWZScrollBar.Create(Scroll, Root.Child['VScr9'], Width - 20, 50, 197)
    else
      FScroller.Paint;
  end;

  if not Assigned(Character) then
    Exit;

  DrawMesos(Character.Mesos);

  for I in GetCurInv do
    if (I.Position > FViewStart) and (I.Position < FViewEnd) then
      DrawItem(I);
end;

procedure TfrmInvGUI.FormCreate(Sender: TObject);
var
  E: TWZIMGEntry;
  c: Char;
begin
  FButtons := TList<TWZButton>.Create;
  FUI := TWZArchive.Create(ExtractFilePath(ParamStr(0)) + 'UI.wz', nil);
  FViewStart := 0;
  FViewEnd := 25;

  E := FUI.ParseFile(TWZFile(FUI.Root.Entry['Basic.img'])).Root.Child['ItemNo'];
  for c := '0' to '9' do
    FDigits[c] := E.Child[c].Canvas.Dump;
end;

procedure TfrmInvGUI.FormDestroy(Sender: TObject);
var
  P: TPNGImage;
begin
  btnClose.Free;
  tcTabs.Free;
  FButtons.Free;
  FUI.Free;

  for P in FDigits do
    P.Free;
end;

procedure TfrmInvGUI.FormShow(Sender: TObject);
begin
  DoPaint;
end;

function TfrmInvGUI.GetCurInv: TInventory;
var
  T: TMapleInventoryType;
begin
  case tcTabs.Index of
    0: T := miEquip;
    1: T := miUse;
    2: T := miEtc;
    3: T := miSetup;
    4: T := miCash;
    else T := miEquipped; // well, wtf?
  end;

  Result := Character.Inventory[T];
end;

procedure TfrmInvGUI.Scroll(Sender: TObject; Direction: TScrollDirection);
begin
  if (Direction = sdUp) and (FViewStart = 0) then
    Exit;

  if (Direction = sdDown) and (FViewEnd = GetCurInv.SlotLimit + 1) then
    Exit;

  case Direction of
    sdDown:
    begin
      Inc(FViewStart, 4);
      Inc(FViewEnd, 4);
    end;
    sdUp:
    begin
      Dec(FViewStart, 4);
      Dec(FViewEnd, 4);
    end;
  end;

  DoPaint;
end;

procedure TfrmInvGUI.WindowMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button <> mbLeft) or (Y > 20) or CheckClick(X, Y, False) then
    Exit;
  ReleaseCapture;
  Perform(WM_SYSCOMMAND, $F012, 0);
end;

procedure TfrmInvGUI.WindowMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Slot: Byte;
  I: TItem;
  label empty;
begin
  // to find out where coordinates are
  //frmWvsFake.Caption := Format('%d | %d', [x, y]);

  if not Assigned(Character) then
    Exit;

  if (X > 10) and (X < 150) and (Y > 50) and (Y < 260) then
  begin     // Slot in Row      +    0th Index of Row
    Slot := (X - 10) div 36 + 1 + ((Y - 55) div 35) * 4;
    I := GetCurInv[Slot + FViewStart];
    if I <> nil then
      lblItemName.Caption := StringData.GetItemProp(I.ID, 'name')
    else goto empty;
  end
  else empty:
    lblItemName.Caption := '';
end;

procedure TfrmInvGUI.WindowMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    CheckClick(X, Y, True);
end;

{ TWZButton }

constructor TWZButton.Create(AOnClick: TNotifyEvent; AEntry: TWZIMGEntry);
begin
  Create(AOnClick, AEntry, Abs(AEntry.Child['origin'].Vector.X), Abs(AEntry.Child['origin'].Vector.Y));
end;

constructor TWZButton.Create(AOnClick: TNotifyEvent; AEntry: TWZIMGEntry; AX, AY: SmallInt);
begin
  FEntry := AEntry;
  FOnClick := AOnClick;
  FX := AX;
  FY := AY;

  Paint;

  frmInvGUI.Buttons.Add(Self);
end;

destructor TWZButton.Destroy;
begin
  frmInvGUI.Buttons.Remove(Self);

  inherited;
end;

procedure TWZButton.Paint;
var
  E: TWZIMGEntry;
begin
  E := FEntry.Get('normal/0');
  if E = nil then
    E := FEntry;

  with E.Canvas do
  begin
    FWidth := Width;
    FHeight := Height;
    frmInvGUI.Draw(FX, FY, Dump);
  end;
end;

function TWZButton.CheckClick(X, Y: Integer; Up: Boolean): Boolean;
begin
  Result := PtInRect(Rect(FX, FY, FX + FWidth, FY + FHeight), Point(X, Y));
  if Result and Up then
    FOnClick(Self);
end;

{ TWZTabControl }

constructor TWZTabControl.Create(AEntry: TWZIMGEntry);
var
  i: Integer;
begin
  FEntry := AEntry;

  with FEntry.Child['disabled'] do
  begin
    SetLength(FTabs, Children.Count);
    for i := 1 to High(FTabs) do
      FTabs[i] := TWZButton.Create(TabClick, Child[IntToStr(i)]);
  end;
  FTabs[0] := TWZButton.Create(TabClick, FEntry.Child['enabled'].Child['0']);
  FIndex := 0;
end;

destructor TWZTabControl.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FTabs) do
    FTabs[i].Free;

  inherited;
end;

procedure TWZTabControl.Paint;
var
  i: Integer;
begin
  for i := 0 to High(FTabs) do
    FTabs[i].Paint;
end;

procedure TWZTabControl.TabClick(Sender: TObject);
var
  i, CIdx: Integer;
begin
  CIdx := -1;
  for i := 0 to High(FTabs) do
    if FTabs[i] = Sender then
    begin
      CIdx := i;
      Break;
    end;

  if (CIdx = FIndex) or (CIdx = -1) then
    Exit;

  FTabs[FIndex].Free;
  FTabs[FIndex] := TWZButton.Create(TabClick, FEntry.Child['disabled'].Child[IntToStr(FIndex)]);
  FTabs[CIdx].Free;
  FTabs[CIdx] := TWZButton.Create(TabClick, FEntry.Child['enabled'].Child[IntToStr(CIdx)]);
  FIndex := CIdx;
  frmInvGUI.DoPaint;
end;

{ TWZScrollBar }

constructor TWZScrollBar.Create(AOnScroll: TOnScroll; AEntry: TWZIMGEntry; AX, AY, AHeight: SmallInt);
begin
  FEntry := AEntry;
  FOnScroll := AOnScroll;

  FBtnUp := TWZButton.Create(UpClick, FEntry.Child['enabled'].Child['prev0'], AX, AY);
  FBtnDown := TWZButton.Create(DownClick, FEntry.Child['enabled'].Child['next0'], AX, AY + AHeight);
end;

destructor TWZScrollBar.Destroy;
begin
  FBtnUp.Free;
  FBtnDown.Free;

  inherited;
end;

procedure TWZScrollBar.Paint;
begin
  FBtnUp.Paint;
  FBtnDown.Paint;
end;

procedure TWZScrollBar.DownClick(Sender: TObject);
begin
  FOnScroll(Self, sdDown);
end;

procedure TWZScrollBar.UpClick(Sender: TObject);
begin
  FOnScroll(Self, sdUp);
end;

end.
