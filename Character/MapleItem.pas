(*
    This file is part of the Delphi MapleStory Server

    	Copyright (C) 2009-2010  Hendi

    The code contains portions of:

	    OdinMS
	    KryptoDEV Source
	    Vana

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License version 3
    as published by the Free Software Foundation. You may not use, modify
    or distribute this program under any other version of the
    GNU Affero General Public License.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License <http://www.gnu.org/licenses/>
    for more details.
*)

unit MapleItem;

interface

uses SysUtils;

const
  itEquip = 1;
  itItem = 2;
  itPet = 3;

type
  TConsumeInfo = record
    HP, MP, HPRate, MPRate: Word;
    WAtk, MAtk, WDef, MDef, Acc, Avoid, Speed, Jump: Word;

    // Return Scrolls
    MoveTo: Integer;
    IgnoreContinent: Boolean;
  end;
  PConsumeInfo = ^TConsumeInfo;

  TItem = class
  protected
    FID: Integer;
    FPosition: ShortInt;
    FQuantity: Word;
    FType: Byte;
    FOwner: string;
  public
    constructor Create(ID: Integer; Position: ShortInt; Quantity: Word);

    property ID: Integer read FID;
    property Position: ShortInt read FPosition write FPosition;
    property ItemType: Byte read FType;
    property Owner: string read FOwner write FOwner;
    property Quantity: Word read FQuantity write FQuantity;
  end;

  TEquip = class(TItem)
  private
    FLevel, FUpgradeSlots: Byte;
    FStr, FDex, FInt, FLuk, FHP, FMP, FWAtk, FMAtk, FWDef, FMDef, FAcc,
    FAvoid, FHands, FSpeed, FJump: Word;

    procedure SetQuantity(const Value: Word);
  public
    constructor Create(ID: Integer; Position: ShortInt);

    property Quantity: Word read FQuantity write SetQuantity;

    property Level: Byte read FLevel write FLevel;
    property UpgradeSlots: Byte read FUpgradeSlots write FUpgradeSlots;

    property Str: Word read FStr write FStr;
    property Dex: Word read FDex write FDex;
    property Int: Word read FInt write FInt;
    property Luk: Word read FLuk write FLuk;
    property HP: Word read FHP write FHP;
    property MP: Word read FMP write FMP;
    property WAtk: Word read FWAtk write FWAtk;
    property MAtk: Word read FMAtk write FMAtk;
    property WDef: Word read FWDef write FWDef;
    property MDef: Word read FMDef write FMDef;
    property Acc: Word read FAcc write FAcc;
    property Avoid: Word read FAvoid write FAvoid;
    property Hands: Word read FHands write FHands;
    property Speed: Word read FSpeed write FSpeed;
    property Jump: Word read FJump write FJump;
  end;

implementation

{ TItem }

constructor TItem.Create(ID: Integer; Position: ShortInt; Quantity: Word);
begin
  FID := ID;
  FPosition := Position;
  FQuantity := Quantity;
  FType := itItem;
end;

{ TEquip }

constructor TEquip.Create(ID: Integer; Position: ShortInt);
begin
  inherited Create(ID, Position, 1);
  FType := itEquip;
end;

procedure TEquip.SetQuantity(const Value: Word);
begin
  if Value > 1 then
    raise Exception.CreateFmt('Setting the quantity to %d on %d', [Value, ID]);

  FQuantity := Value;
end;

end.
