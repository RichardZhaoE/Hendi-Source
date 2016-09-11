unit PNGMapleCanvas;

interface

uses Windows, Classes, SysUtils, PNGImage, WZReader, ZLib, Graphics;

type
  TPNGMapleCanvas = class
  private
    FHeight: Integer;
    FWidth: Integer;
    FDataLength: Integer;
    FOffset: Int64;
    FFormat: Integer;
    FWZReader: TWZReader;

    function Parse1(Input: TMemoryStream): TPNGImage;
    function Parse513(Input: TMemoryStream): TPNGImage;
  public
    constructor Create(Width, Height, DataLength: Integer; Offset: Int64; Format: Integer; var WZReader: TWZReader);

    function Decompress: TMemoryStream;
    function Dump: TPNGImage;

    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

implementation

{ TPNGMapleCanvas }

constructor TPNGMapleCanvas.Create(Width, Height, DataLength: Integer; Offset: Int64; Format: Integer; var WZReader: TWZReader);
begin
  FHeight := Height;
  FWidth := Width;
  FDataLength := DataLength;
  FOffset := Offset;
  FFormat := Format;
  FWZReader := WZReader;
end;

procedure StreamDecompression(mInputStream: TStream; var mOutputStream: TMemoryStream);
var
  Stream: TZDecompressionStream;
begin
  if not Assigned(mInputStream) then
  begin
    if Assigned(mOutputStream) then
      FreeAndNil(mOutputStream);
    Exit;
  end;

  if not Assigned(mOutputStream) then
    Exit;

  Stream := TZDecompressionStream.Create(mInputStream);
  try
    mOutputStream.CopyFrom(Stream, 0);
  finally
    Stream.Free;
  end;
end;

procedure StreamDecryption(var Stream: TMemoryStream; Key: TBytes);
var
  SubLen, i: Integer;
  Data: TBytes;
  New: TMemoryStream;
begin
  New := TMemoryStream.Create;
  try
    while Stream.Position < Stream.Size do
    begin
      Stream.Read(SubLen, 4);
      SetLength(Data, SubLen);
      Stream.Read(Data[0], SubLen);

      for i := 0 to High(Data) do
        Data[i] := Data[i] xor Key[i];

      New.Write(Data[0], Length(Data));
    end;

    Stream.Clear;
    Stream.CopyFrom(New, 0);
  finally
    New.Free;
  end;
end;

function TPNGMapleCanvas.Decompress: TMemoryStream;
var
  vBuf: TMemoryStream;
begin
  if (FFormat <> 1) and (FFormat <> 513) then
    raise Exception.CreateFmt('Image format unsupported! [%d]', [FFormat]);

  Result := nil;

  vBuf := TMemoryStream.Create;

  FWZReader.Seek(FOffset, soBeginning);
  vBuf.CopyFrom(FWZReader.Stream, FDataLength);
  vBuf.Position := 0;

  Result := TMemoryStream.Create;
  try
    try
      StreamDecompression(vBuf, Result);
    except
      on EZDecompressionError do
      begin
        vBuf.Position := 0;
        StreamDecryption(vBuf, FWZReader.Key);
        StreamDecompression(vBuf, Result);
      end
      else
        FreeAndNil(Result);
    end;

    if not Assigned(Result) then
      Exit(nil);

    Result.Position := 0;
  finally
    vBuf.Free;
  end;
end;

function TPNGMapleCanvas.Dump: TPNGImage;
var
  Decompressed: TMemoryStream;
begin
  Result := nil;
  Decompressed := Decompress;
  try
    case FFormat of
        1: Result := Parse1(Decompressed);
      513: Result := Parse513(Decompressed);    // $201
    end;
  finally
    Decompressed.Free;
  end;
end;

function TPNGMapleCanvas.Parse1(Input: TMemoryStream): TPNGImage;
var
  x, y: Integer;
  b1, b2: Byte;
  a, r, g, b: Word;
  RGBLine: PRGBLine;
  AlphaLine: PByteArray;
begin
  Result := TPNGImage.CreateBlank(COLOR_RGBALPHA, 16, FWidth, FHeight);

  for y := 0 to FHeight - 1 do
  begin
    RGBLine := Result.Scanline[y];
    AlphaLine := Result.AlphaScanline[y];
    for x := 0 to FWidth - 1 do
    begin
      Input.Read(b1, 1);
      Input.Read(b2, 1);

      b := b1 and 15;
      b := b or (b shl 4);
      g := b1 and 240;
      g := g or (g shr 4);
      r := b2 and 15;
      r := r or (r shl 4);
      a := b2 and 240;
      a := a or (a shr 4);

      RGBLine[x].rgbtBlue := b;
      RGBLine[x].rgbtGreen := g;
      RGBLine[x].rgbtRed := r;
      AlphaLine[x] := a;
    end;
  end;
end;

function TPNGMapleCanvas.Parse513(Input: TMemoryStream): TPNGImage;
var
  x, y: Integer;
  b1, b2: Byte;
  a, r, g, b: Word;
begin
  Result := TPNGImage.CreateBlank(COLOR_RGB, 16, FWidth, FHeight);
  Result.CreateAlpha;

  x := 0;
  y := 0;
  while Input.Position < Input.Size do
  begin
    if x = FWidth then
    begin
      x := 0;
      Inc(y);
      if y = FHeight then
        Break;
    end;

    Input.Read(b1, 1);
    Input.Read(b2, 1);

    b := (b1 and $1F) shl 3;
    b := b or (b shr 5);
    g := ((b2 and 7) shl 5) or ((b1 and $E0) shr 3);
    g := g or (g shr 6);
    r := b2 and $F8;
    r := r or (r shr 5);
    a := $FF;

    Result.Pixels[x, y] := RGB(r, g, b);
    Result.AlphaScanline[y][x] := a;

    Inc(x);
  end;
end;

end.
