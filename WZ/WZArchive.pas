unit WZArchive;

interface

uses Classes, SysUtils, Dialogs, WZReader, WZDirectory, WZIMGFile, VirtualTrees,
     Tools;

type
  TWZArchive = class
  private
    FReader: TWZReader;
    FRoot: TWZDirectory;
    FVST: TVirtualStringTree;
    FFileSize, FHeaderSize, FVersion: Integer;
    FName, FPKG, FCopyright: string;

    procedure Load;
    procedure GetOffsets(Dir: TWZDirectory; var StartOffset: Int64);
    procedure ParseDirectory(Dir: TWZDirectory);

    function DecodeVersion(Encoded: Smallint): Integer;
  public
    constructor Create(Filename: string; VST: TVirtualStringTree; Cache: Boolean = False);
    destructor Destroy; override;

    function GetImgFile(Path: string): TWZIMGFile;
    function GetData(Path: string): TWZIMGFile;

    function ParseFile(F: TWZFile): TWZIMGFile;

    property Reader: TWZReader read FReader;
    property Root: TWZDirectory read FRoot;
    property VST: TVirtualStringTree read FVST;

    property Copyright: string read FCopyright;
    property FileSize: Integer read FFileSize;
    property HeaderSize: Integer read FHeaderSize;
    property Name: string read FName;
    property PKG: string read FPKG;
    property Version: Integer read FVersion;
  end;

implementation

constructor TWZArchive.Create(Filename: string; VST: TVirtualStringTree;
  Cache: Boolean = False);
begin
  FReader := TWZReader.Create(Filename, Cache);

  FName := ExtractFileName(Filename);

  FVST := VST;

  FRoot := TWZDirectory.Create(ExtractFileName(Filename), 0, 0, nil);
  FRoot.Archive := Self;

  if FVST <> nil then
  begin
    FRoot.Node := FVST.AddChild(nil, FRoot);
    FRoot.VST := FVST;
  end;

  Load;
end;

destructor TWZArchive.Destroy;
begin
  FRoot.Free;
  FReader.Free;

  inherited;
end;

procedure TWZArchive.Load;
var
  Off: Int64;
begin
  FPKG := FReader.ReadString(4);
  FFileSize := FReader.ReadUInt64;
  FHeaderSize := FReader.ReadInt;
  FCopyright := FReader.ReadNullTerminatedString;
  FVersion := DecodeVersion(FReader.ReadShort);

  ParseDirectory(FRoot);
  Off := FReader.Position;
  GetOffsets(FRoot, Off);
end;

function TWZArchive.DecodeVersion(Encoded: Smallint): Integer;
var
  Sum, i, j: Integer;
  Version: string;
  a, b, c, d: Integer;
  e: Smallint;
begin
  for i := 20 to 500 do       // Minimum has to be that high; otherwise v94 turns out as v13
  begin
    Sum := 0;

    Version := IntToStr(i);

    for j := 1 to Length(Version) do
      Sum := (Sum * 32) + Ord(Version[j]) + 1;

		a := (Sum shr 24) and $FF;
		b := (Sum shr 16) and $FF;
		c := (Sum shr 8) and $FF;
		d := Sum and $FF;
		e := $FF xor a xor b xor c xor d;

    if Encoded = e then
      Exit(i);
  end;

  Result := -1;
end;

procedure TWZArchive.ParseDirectory(Dir: TWZDirectory);
var
  EntryCount, i, Size, Checksum: Integer;
  Marker: Byte;
  Name: string;
  E: TWZEntry;
begin
  EntryCount := FReader.ReadValue;

  for i := 0 to EntryCount - 1 do
  begin
    Marker := FReader.ReadByte;

    case Marker of
      $01, $02:
      begin
        Name := FReader.ReadDecodedStringAtOffsetAndReset(FReader.ReadInt + FHeaderSize + 1);
        Size := FReader.ReadValue;
        Checksum := FReader.ReadValue;
        FReader.ReadInt;       // Dummy

        if Marker = 1 then
        begin
          E := TWZDirectory.Create(Name, Size, Checksum, Dir);
          Dir.AddDirectory(TWZDirectory(E));
        end
        else
        begin
          E := TWZFile.Create(Name, Size, Checksum, Dir);
          Dir.AddFile(TWZFile(E));
        end;

        if FVST <> nil then
        begin
          E.Node := FVST.AddChild(Dir.Node, E);
          E.VST := FVST;
        end;
      end;

      $03, $04:
      begin
        Name := FReader.ReadDecodedString;
        Size := FReader.ReadValue;
        Checksum := FReader.ReadValue;
        FReader.ReadInt;     // Dummy

        if Marker = 3 then
        begin
          E := TWZDirectory.Create(Name, Size, Checksum, Dir);
          Dir.AddDirectory(TWZDirectory(E));
        end
        else
        begin
          E := TWZFile.Create(Name, Size, Checksum, Dir);
          Dir.AddFile(TWZFile(E));
        end;

        if FVST <> nil then
        begin
          E.Node := FVST.AddChild(Dir.Node, E);
          E.VST := FVST;
        end;
      end;

      //else Showmessage('Unknown Marker at ParseDirectory(' + Dir.Name + '): ' + sLineBreak +
//      'i = ' + IntToStr(i) + ': ' + IntToStr(Marker));
    end;
  end;

  for i := 0 to Dir.SubDirs.Count - 1 do
    ParseDirectory(Dir.SubDirs[i]);
end;

function TWZArchive.GetData(Path: string): TWZIMGFile;
begin
  Result := GetImgFile(Path);
end;

function TWZArchive.GetImgFile(Path: string): TWZIMGFile;
var
  Segments: TStringArray;
  i: Integer;
  Dir: TWZDirectory;
  Entry: TWZFile;
begin
  Segments := Explode('/', Path);

  Dir := FRoot;
  for i := 0 to High(Segments) - 1 do
  begin
    Dir := TWZDirectory(Dir.Entry[Segments[i]]);

    if Dir = nil then
    begin
      ShowMessageFmt('File %s not found in %s!', [Path, FRoot.Name]);
      Exit(nil);
    end;
  end;

  Entry := TWZFile(Dir.Entry[Segments[High(Segments)]]);
  if Entry = nil then
  begin
    ShowMessage('Entry = nil!');
    Exit(nil);
  end;

  Segments := nil;

  Result := ParseFile(Entry);
end;

procedure TWZArchive.GetOffsets(Dir: TWZDirectory; var StartOffset: Int64);
var
  Entry: TWZEntry;
begin
  for Entry in Dir.Files do
  begin
    Entry.Offset := StartOffset;
    Inc(StartOffset, Entry.Size);
  end;

  for Entry in Dir.SubDirs do
    GetOffsets(TWZDirectory(Entry), StartOffset);
end;

function TWZArchive.ParseFile(F: TWZFile): TWZIMGFile;
begin
  if F.Parsed and Assigned(F.IMGFile) then
    Exit(TWZIMGFile(F.IMGFile));

  Result := TWZIMGFile.Create(FReader, F);
  F.Parsed := True;
  F.IMGFile := Result;
end;

end.
