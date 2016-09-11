unit Inventory;

interface

uses Windows, Classes, SysUtils, Generics.Collections, Generics.Defaults, GameLogic,
     MapleItem, MapleStream;

type
  TInventoryEnumerator = TDictionary<ShortInt, TItem>.TValueEnumerator;

  TInventory = class
  private
    FInventory: TDictionary<ShortInt, TItem>;
    FSlotLimit: Byte;
    FType: TMapleInventoryType;

    function Get(Slot: ShortInt): TItem;
    function GetE(Slot: TEquipSlot): TItem;
    procedure Swap(Source, Target: TItem);
  public
    constructor Create(InvType: TMapleInventoryType; SlotLimit: Byte);
    destructor Destroy; override;

    procedure AddFromPacket(P: TMapleStream; Pos: Byte; Update: Boolean);
    function ContainsID(ID: Integer): Boolean;

    function GetEnumerator: TInventoryEnumerator;

    function ListByID(const ItemID: Integer): TList<TItem>;

    procedure Move(Src, Dst: ShortInt; SlotMax: SmallInt);

    procedure RemoveItem(Slot: ShortInt); overload;
    procedure RemoveItem(Slot: ShortInt; Quantity: Word; AllowZero: Boolean); overload;
    procedure RemoveSlot(const Slot: ShortInt);

    property Items[Slot: ShortInt]: TItem read Get; default;  // This is very nice :D
    property Items[Slot: TEquipSlot]: TItem read GetE; default;
    property InvType: TMapleInventoryType read FType;
    property SlotLimit: Byte read FSlotLimit write FSlotLimit;
  end;

implementation

uses Main, ExtendedParser;

{ TInventory }

constructor TInventory.Create(InvType: TMapleInventoryType; SlotLimit: Byte);
begin
  FType := InvType;
  FSlotLimit := SlotLimit;
  FInventory := TDictionary<ShortInt, TItem>.Create;
end;

destructor TInventory.Destroy;
var
  Item: TItem;
begin
  for Item in FInventory.Values do
    Item.Free;

  FInventory.Free;

  inherited;
end;

procedure TInventory.AddFromPacket(P: TMapleStream; Pos: Byte; Update: Boolean);
var
  I: TItem;
  E: TEquip;
  Cash: Boolean;
begin
  if Update and FInventory.ContainsKey(Pos) then
  begin
    FInventory[Pos].Free;
    FInventory.Remove(Pos);
  end;

  with P do
  begin
    if (not Update) and (ShortInt(FType) < 2) then
      ReadByte;

    case ReadByte of
      itEquip: I := TEquip.Create(ReadInt, Pos);
      itItem: I := MapleItem.TItem.Create(ReadInt, Pos, 1);
      itPet: I := nil;
      else
      begin
        Log('Unknown Item Type!');
        Exit;
      end;
    end;

    Cash := ReadByte > 0;
    if Cash then
      Skip(8);
    Skip(8); // Expiration

    Skip(4);  // -1, since 0.96

    if I = nil then
    begin  // Pet
      Skip(35);
      Exit;
    end;

    FInventory.Add(I.Position, I);

    if I.ItemType <> itEquip then
    begin
      I.Quantity := ReadShort;
      I.Owner := ReadMapleAnsiString;
      ReadShort;  // Flags

      if IsRechargeable(I.ID) then
        Skip(8);

      Exit;
    end;

    E := TEquip(I);
    with E do
    begin
      UpgradeSlots := ReadByte;
      Level := ReadByte;
      STR := ReadShort;
      DEX := ReadShort;
      INT := ReadShort;
      LUK := ReadShort;
      HP := ReadShort;
      MP := ReadShort;
      WAtk := ReadShort;
      MAtk := ReadShort;
      WDef := ReadShort;
      MDef := ReadShort;
      Acc := ReadShort;
      Avoid := ReadShort;
      Hands := ReadShort;
      Speed := ReadShort;
      Jump := ReadShort;
      Owner := ReadMapleAnsiString;
      ReadShort;  // Flags
      Skip(26);
      if not Cash then
        Skip(8);
    end;
    Skip(12);
  end;
end;

function TInventory.ContainsID(ID: Integer): Boolean;
var
  Item: TItem;
begin
  for Item in Self do
    if Item.ID = ID then
      Exit(True);

  Result := False;
end;

function TInventory.Get(Slot: ShortInt): TItem;
begin
  if not FInventory.TryGetValue(Slot, Result) then
    Result := nil;
end;

function TInventory.GetE(Slot: TEquipSlot): TItem;
begin
  if not FInventory.TryGetValue(ShortInt(Slot), Result) then
    Result := nil;
end;

function TInventory.GetEnumerator: TInventoryEnumerator;
begin
  // FUCKING ERROR INSIGHT!!!!
  Result := FInventory.Values.GetEnumerator;
end;

function TInventory.ListByID(const ItemID: Integer): TList<TItem>;
var
  Item: TItem;
begin
  Result := TList<TItem>.Create(TComparer<TItem>.Construct(
    function(const Left, Right: TItem): Integer
    begin
      if Left.Position < Right.Position then
        Result := -1
      else
      if Left.Position > Right.Position then
        Result := 1
      else
        Result := 0;
    end));

  for Item in Self do
    if Item.ID = ItemID then
      Result.Add(Item);

  Result.Sort;   // Sort using Position
end;

procedure TInventory.Move(Src, Dst: ShortInt; SlotMax: SmallInt);
var
  Source, Target: TItem;
  Rest: SmallInt;
begin
  Source := Get(Src);
  Target := Get(Dst);

  if Source = nil then
    raise EArgumentException.Create('Trying to move empty slot!');

  if Target = nil then
  begin
    Source.Position := Dst;
    FInventory.Add(Dst, Source);
    FInventory.Remove(Src);
  end
  else
  if (Target.ID = Source.ID) and (not IsRechargeable(Source.ID)) then
  begin
    if FType = miEquip then
      Swap(Target, Source);

    if Source.Quantity + Target.Quantity > SlotMax then
    begin
      Rest := (Source.Quantity + Target.Quantity) - SlotMax;
      Source.Quantity := Rest;
      Target.Quantity := SlotMax;
    end
    else
    begin
      Target.Quantity := Source.Quantity + Target.Quantity;
      FInventory.Remove(Src);
    end;
  end
  else
    Swap(Target, Source);
end;

procedure TInventory.RemoveItem(Slot: ShortInt);
begin
  RemoveItem(Slot, 1, False);
end;

procedure TInventory.RemoveItem(Slot: ShortInt; Quantity: Word;
  AllowZero: Boolean);
var
  Item: TItem;
begin
  if not FInventory.TryGetValue(Slot, Item) then
    Exit;

  if Item.Quantity - Quantity < 0 then
    Item.Quantity := 0
  else
    Item.Quantity := Item.Quantity - Quantity;

  if (Item.Quantity = 0) and (not AllowZero) then
    RemoveSlot(Slot);
end;

procedure TInventory.RemoveSlot(const Slot: ShortInt);
begin
  if not FInventory.ContainsKey(Slot) then
    Exit;

  FInventory.Remove(Slot);
end;

procedure TInventory.Swap(Source, Target: TItem);
var
  SwapPos: ShortInt;
begin
  FInventory.Remove(Source.Position);
  FInventory.Remove(Target.Position);
  SwapPos := Source.Position;
  Source.Position := Target.Position;
  Target.Position := SwapPos;
  FInventory.Add(Source.Position, Source);
  FInventory.Add(Target.Position, Target);
end;

end.
