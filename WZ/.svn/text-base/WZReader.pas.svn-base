unit WZReader;

interface

uses SysUtils, Classes, KeyHandler;

type
  TWZReader = class
  private
    FFileName: string;
    FWZ: TStream;
    FKey: TBytes;

    function GetPosition: Int64;
  public
    constructor Create(AFileName: string; Cache: Boolean = False);
    destructor Destroy; override;

    function ReadByte: Byte;
    function ReadInt8: ShortInt;
    function ReadShort: SmallInt;
    function ReadInt: Integer;
    function ReadValue: Integer;
    function ReadUInt64: UInt64;
    function ReadFloat: Single;
    function ReadFloatValue: Single;
    function ReadDouble: Double;
    function ReadChar: AnsiChar;
    function ReadString(Length: Integer): string;
    function ReadNullTerminatedString: string;
    function ReadDecodedString: string;
    function ReadDecodedStringAtOffsetAndReset(Offset: Int64): string;

    procedure Seek(Offset: Int64; Origin: TSeekOrigin);

    property FileName: string read FFileName;
    property Key: TBytes read FKey;
    property Position: Int64 read GetPosition;
    property Stream: TStream read FWZ;
  end;

implementation

{ TWZReader }

constructor TWZReader.Create(AFileName: string; Cache: Boolean = False);
var
  FS: TFileStream;
begin
  inherited Create;

  FFileName := ExtractFileName(AFileName);
  FKey := nil;
  if not Cache then
    FWZ := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone)
  else
  begin
    FWZ := TMemoryStream.Create;
    FS := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      FS.Position := 0;
      FWZ.CopyFrom(FS, 0);
      FWZ.Position := 0;
    finally
      FS.Free;
    end;
  end;
end;

destructor TWZReader.Destroy;
begin
  FKey := nil;
  FreeAndNil(FWZ);

  inherited;
end;

function TWZReader.GetPosition: Int64;
begin
  Result := FWZ.Position;
end;

procedure TWZReader.Seek(Offset: Int64; Origin: TSeekOrigin);
begin
  FWZ.Seek(Offset, Origin);
end;

// ==================== Stream Reading functions ===============================

function TWZReader.ReadByte: Byte;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadInt8: ShortInt;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadShort: SmallInt;
begin
	FWZ.Read(Result, 2);
end;

function TWZReader.ReadInt: Integer;
begin
  FWZ.Read(Result, 4);
end;

function TWZReader.ReadFloat: Single;
begin
  FWZ.Read(Result, 4);
end;

function TWZReader.ReadUInt64: UInt64;
begin
  FWZ.Read(Result, 8);
end;

function TWZReader.ReadDouble: Double;
begin
  FWZ.Read(Result, 8);
end;

function TWZReader.ReadChar: AnsiChar;
begin
  FWZ.Read(Result, 1);
end;

function TWZReader.ReadString(Length: Integer): string;
var
  i, Limit: Integer;
  b: Byte;
begin
  Result := '';

  Limit := Length;
	if Limit = 0 then
		Limit := FWZ.Size - FWZ.Position;

	for i := 1 to Limit do
  begin
		b := ReadByte;
		if b <> 0 then
	  	Result := Result + Chr(b);
  end;
end;

function TWZReader.ReadNullTerminatedString: string;
var
  b: Byte;
  i: Integer;
begin
  b := 1;
  i := 0;

  while b <> 0 do
  begin
		b := ReadByte;
    Inc(i);
    SetLength(Result, i);
		Result[i] := Chr(b);
  end;
end;

function TWZReader.ReadDecodedString: string;
var
  StrLength, i: Integer;
  b, b2: Byte;
  UMask, Mask: Word;
  Str: string;
  Chr1: Word;
  MB1, MB2: Byte;
begin
	b := ReadByte;

  if b = 0 then
    Exit('');

  Str := '';

  if b <= $7F then   // unicode
  begin
		UMask := $AAAA;

    if b = $7F then        // 7F = 127
      StrLength := ReadInt
    else
      StrLength := b;

		if StrLength < 0 then
      raise Exception.Create('StrLength < 0');

		for i := 1 to StrLength do
    begin
      Chr1 := ReadShort;
			Chr1 := Chr1 xor (UMask and $FFFF);

      // decryption
      if FKey <> nil then
      begin
        MB1 := FKey[i * 2 - 2];
        MB2 := FKey[i * 2 - 1];
        Chr1 := Chr1 xor ((MB2 shl 8) + MB1);
      end;

			Inc(UMask);
			Str := Str + Char(Chr1);
    end;
  end
  else     // non-unicode
  begin
		Mask := $AA;

		if b = $80 then
      StrLength := ReadInt
    else
      StrLength := 256 - b;

		SetLength(Str, StrLength);
		for i := 1 to StrLength do
    begin
			b2 := ReadByte;

      if (b2 < $80) and (FKey = nil) then
        FKey := KeyCreator.GetKey(GMS_IV);

			b2 := b2 xor Mask;
      if (FKey <> nil) and (i < Length(FKey)) then
        b2 := b2 xor FKey[i - 1];

			Inc(Mask);
			Str[i] := Chr(b2);
		end;
  end;

  Result := Str;
end;

function TWZReader.ReadDecodedStringAtOffsetAndReset(Offset: Int64): string;
var
  Pos: Int64;
begin
  Pos := FWZ.Position;             // save old position
  FWZ.Seek(Offset, soBeginning);   // seek to wanted position
  Result := ReadDecodedString;     // read
  FWZ.Seek(Pos, soBeginning);      // reset to old position
end;

function TWZReader.ReadFloatValue: Single;
var
  b: Byte;
begin
  b := ReadByte;
  if b = 128 then
    Result := ReadFloat
  else
    Result := 0;
end;

function TWZReader.ReadValue: Integer;
var
  b: ShortInt;
begin
  b := ReadInt8;
 	if b = -128 then
    Result := ReadInt
  else
		Result := b;
end;

end.
