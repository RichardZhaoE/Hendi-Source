unit WZDirectory;

interface

uses SysUtils, Generics.Collections, VirtualTrees, Dialogs;

type
  TWZEntry = class
  private
    FName: string;
    FSize: Integer;
    FChecksum: Integer;
    FOffset: Integer;
    FParent: TWZEntry;
    FNode: PVirtualNode;
    FVST: TVirtualStringTree;
  public
    constructor Create(Name: string; Size, Checksum: Integer; Parent: TWZEntry);
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Size: Integer read FSize write FSize;
    property Checksum: Integer read FChecksum write FChecksum;
    property Offset: Integer read FOffset write FOffset;
    property Parent: TWZEntry read FParent write FParent;
    property Node: PVirtualNode read FNode write FNode;
    property VST: TVirtualStringTree read FVST write FVST;
  end;

  TWZFile = class(TWZEntry)
  private
    FIMGFile: TObject;
    FParsed: Boolean;
  public
    constructor Create(Name: string; Size, Checksum: Integer; Parent: TWZEntry);
    destructor Destroy; override;

    property IMGFile: TObject read FIMGFile write FIMGFile;
    property Parsed: Boolean read FParsed write FParsed;
  end;

  TWZDirectory = class(TWZEntry)
  private
    FArchive: TObject; { TWZArchive }
    FFiles: TList<TWZFile>;
    FSubDirs: TList<TWZDirectory>;
    FEntries: TList<TWZEntry>;    // Files and SubDirs together

    function GetEntry(Name: string): TWZEntry;
  public
    constructor Create(Name: string; Size, Checksum: Integer; Parent: TWZEntry);
    destructor Destroy; override;

    procedure AddFile(FileEntry: TWZFile);
    procedure AddDirectory(Dir: TWZDirectory);

    property Archive: TObject { TWZArchive } read FArchive write FArchive;
    property Entry[Name: string]: TWZEntry read GetEntry;
    property Entries: TList<TWZEntry> read FEntries;
    property Files: TList<TWZFile> read FFiles;
    property SubDirs: TList<TWZDirectory> read FSubDirs;
  end;

implementation

uses WZArchive;

{ TWZEntry }

constructor TWZEntry.Create(Name: string; Size, Checksum: Integer; Parent: TWZEntry);
begin
  FName := Name;
  FSize := Size;
  FChecksum := Checksum;
  FParent := Parent;
  FNode := nil;
end;

destructor TWZEntry.Destroy;
begin
  if FNode <> nil then
    FVST.DeleteNode(FNode);

  inherited;
end;

{ TWZDirectory }

constructor TWZDirectory.Create(Name: string; Size, Checksum: Integer; Parent: TWZEntry);
begin
  inherited Create(Name, Size, Checksum, Parent);

  FFiles := TList<TWZFile>.Create;
  FSubDirs := TList<TWZDirectory>.Create;
  FEntries := TList<TWZEntry>.Create;
end;

destructor TWZDirectory.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count - 1 do
    FEntries[i].Free;

  FFiles.Free;
  FSubDirs.Free;
  FEntries.Free;

  inherited;
end;

function TWZDirectory.GetEntry(Name: string): TWZEntry;
begin
  for Result in FEntries do
    if Result.Name = Name then
      Exit;

  Result := nil;
end;

procedure TWZDirectory.AddFile(FileEntry: TWZFile);
begin
  FFiles.Add(FileEntry);
  FEntries.Add(FileEntry);
end;

procedure TWZDirectory.AddDirectory(Dir: TWZDirectory);
begin
  FSubDirs.Add(Dir);
  FEntries.Add(Dir);
end;

{ TWZFile }

constructor TWZFile.Create(Name: string; Size, Checksum: Integer;
  Parent: TWZEntry);
begin
  inherited Create(Name, Size, Checksum, Parent);

  FParsed := False;
  FIMGFile := nil;
end;

destructor TWZFile.Destroy;
begin
  if FIMGFile <> nil then
    FIMGFile.Free;

  inherited;
end;

end.
