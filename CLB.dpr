program CLB;

{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}

uses
  Forms,
  Main in 'Main.pas' {frmWvsFake},
  Login in 'Login.pas' {frmLogin},
  MP3MapleSound in 'WZ\MP3MapleSound.pas',
  PNGMapleCanvas in 'WZ\PNGMapleCanvas.pas',
  WZArchive in 'WZ\WZArchive.pas',
  WZDirectory in 'WZ\WZDirectory.pas',
  WZIMGFile in 'WZ\WZIMGFile.pas',
  KeyHandler in 'WZ\KeyHandler.pas',
  WZReader in 'WZ\WZReader.pas',
  DataCenter in 'DataCenter.pas',
  MapleMap in 'MapleMap.pas',
  Tools in 'Tools.pas',
  DeathLoader in 'DeathLoader.pas',
  Scheduler in 'Scheduler.pas',
  FuckShield in 'FuckShield.pas',
  Items in 'Items.pas' {frmItems},
  CharSelection in 'CharSelection.pas' {frmCharSel},
  DropDB in 'DropDB.pas',
  Footholds in 'Footholds.pas',
  Worlds in 'Worlds.pas' {frmWorlds},
  SpawnPoint in 'SpawnPoint.pas' {frmSpawnPoint},
  MapleCharacter in 'Character\MapleCharacter.pas',
  Inventory in 'Character\Inventory.pas',
  MapleItem in 'Character\MapleItem.pas',
  Movement in 'Character\Movement.pas',
  Auth in 'Packet\Auth.pas',
  ExtendedParser in 'Packet\ExtendedParser.pas',
  MapleCrypt in 'Packet\MapleCrypt.pas',
  MapleStream in 'Packet\MapleStream.pas',
  Opcodes in 'Packet\Opcodes.pas',
  PacketIO in 'Packet\PacketIO.pas',
  ReadThread in 'Packet\ReadThread.pas',
  GameLogic in 'GameLogic.pas',
  InvGUI in 'InvGUI.pas' {frmInvGUI},
  SyncServer in 'SyncServer.pas',
  CRC32 in 'Utils\CRC32.pas',
  MobList in 'MobList.pas' {frmMobs};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmWvsFake, frmWvsFake);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.CreateForm(TfrmItems, frmItems);
  Application.CreateForm(TfrmCharSel, frmCharSel);
  Application.CreateForm(TfrmWorlds, frmWorlds);
  Application.CreateForm(TfrmSpawnPoint, frmSpawnPoint);
  Application.CreateForm(TfrmInvGUI, frmInvGUI);
  Application.CreateForm(TfrmMobs, frmMobs);
  Application.Run;
end.
