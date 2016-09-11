unit PacketIO;

interface

uses Windows, Types, Main, MapleStream, SysUtils, Generics.Collections, Opcodes,
     MapleCharacter, Scheduler, DropDB, Movement, FuckShield, GameLogic, Classes;

procedure AtLoginScreen;
procedure Pong;
procedure Login(const Password, Cookie: string);
procedure SelectCharacter;
procedure PlayerLoggedIn;
procedure CancelBuff(ID: Integer);
procedure SendChat(S: string);
procedure FacialExpression(ID: Integer);
procedure NPCTalk(ObjID: Integer);
procedure NPCTalkMore(B1, B2: Byte; Selection: Integer = -1);
procedure RequestChannelChange(ToChannel: Byte);
procedure UseItem(Slot: SmallInt; ID: Integer);

procedure HandlePacket(Packet: TMapleStream);

var
  Character: TMapleCharacter;
  ConAuth: Int64;
  HWID1, HWID2: Integer;
  RestartAtt: Boolean = False;
  CurChannel: Int8 = -1;

implementation

uses ExtendedParser, DeathLoader, MapleMap, DataCenter, Items, Login, CharSelection,
     Worlds, InvGUI;

procedure AtLoginScreen;
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  Packet.WriteShort(S_CLIENT_START);
  frmWvsFake.Write(Packet);
end;

procedure Pong;
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  Packet.WriteShort(S_PONG);
  frmWvsFake.Write(Packet);
end;

procedure Login(const Password, Cookie: string);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_LOGIN);
    WriteMapleAnsiString(Password);
    WriteMapleAnsiString(Cookie);
    // 16 Bytes   CSystemInfo::GetMachineId
    // 4 Bytes    CSystemInfo::GetGameRoomClient
    WriteInt(0);
    WriteShort(0);
    HWID1 := Random(MAXINT);
    HWID2 := Random(High(SmallInt));
    WriteInt(HWID1);
    WriteInt(0);
    WriteInt(HWID2);
    WriteShort(0);
    WriteInt(2);
    WriteShort(0);
    WriteByte(0);
  end;
  frmWvsFake.Write(Packet);
end;

procedure RequestCharList(WorldID: ShortInt);
var
  Packet: TMapleStream;
  x: Int8;
begin
  if WorldID < 0 then
    Exit;

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_CHARLIST);
    WriteByte(2);  // Nexon Crap

    WriteByte(WorldID);

    if not frmWvsFake.cbServerCtrl.Checked then
    begin
      repeat
        x := Random(19)
      until x <> CurChannel;
      CurChannel := x;
      WriteByte(CurChannel);
    end
    else
    begin
      CurChannel := frmWvsFake.SyncGetChannel(False);
      WriteByte(CurChannel);
    end;
    WriteInt(1291954368);
  end;
  frmWvsFake.Write(Packet);
end;

procedure SelectCharacter;
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_SELECT_CHARACTER_PIC);
    WriteMapleAnsiString(frmLogin.Account.PIC);
    WriteInt(Character.ID);
    WriteMapleAnsiString('00-DE-AD-BE-EF-00');
    WriteMapleAnsiString('');
  end;
  frmWvsFake.Write(Packet);
end;

procedure PlayerLoggedIn;
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_PLAYER_LOGGEDIN);
    WriteInt(Character.ID);
    WriteInt(0);
    WriteShort(0);
    WriteInt(HWID1);
    WriteInt(0);
    WriteInt(HWID2);
    WriteInt64(ConAuth);
  end;
  frmWvsFake.Write(Packet);
  frmWvsfake.Caption:='CLB:'+frmLogin.Account.ID;
end;

procedure SMapRush(const PN: string; PortalX, PortalY: SmallInt);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;

  with Packet do
  begin
    WriteShort(S_CHANGE_MAP_SPECIAL);
    WriteByte(Character.UsedPortals);
    WriteMapleAnsiString(PN);
    WriteShort(PortalX);
    WriteShort(PortalY);
    WriteByte($FF);
  end;
  frmWvsFake.Write(Packet);
  Log('SMapRush pn:' + pn);
end;

procedure CancelBuff(ID: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_CANCEL_BUFF);
    WriteInt(ID);
  end;
  frmWvsFake.Write(Packet);
end;

procedure SendChat(S: string);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_CHAT);
    WriteInt(GetTickCount);
    WriteMapleAnsiString(S);
    WriteByte(0);
  end;
  frmWvsFake.Write(Packet);
end;

procedure FacialExpression(ID: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_FACE);
    WriteInt(ID);
  end;
  frmWvsFake.Write(Packet);
end;

procedure NPCTalk(ObjID: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_NPC_TALK);
    WriteInt(ObjID);
    WritePos(Character.Position);
  end;
  frmWvsFake.Write(Packet);
end;

procedure NPCTalkMore(B1, B2: Byte; Selection: Integer = -1);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_NPC_TALK_MORE);
    WriteByte(B1);
    WriteByte(B2);
    if Selection > -1 then
      WriteInt(Selection);
  end;
  frmWvsFake.Write(Packet);
end;

procedure RequestChannelChange(ToChannel: Byte);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_CHANGE_CHANNEL);
    WriteByte(ToChannel);
   (* if frmWvsFake.cbParty.Checked then
    begin
      if CurChannel < 18 then
        WantedCh := CurChannel + 1
      else
        WantedCh := 1;
      WriteByte(WantedCh);
    end
    else
      WriteByte(Random(19));     *)
    WriteInt(GetTickCount);
  end;
  frmWvsFake.Write(Packet);
end;

(*procedure ChangeChannel(OtherChar: Boolean = False);
begin
  if LastReconnect > GetTickCount - 2000 then
  begin
    Sched.AddSchedule(1000, procedure begin ChangeChannel(OtherChar) end);
    Exit;
  end;

  SendChangeChannel;
  // Crappy server is crappy
  Sched.AddSchedule(3000, procedure
  begin
    if LastReconnect < GetTickCount - 4000 then
      if (OtherChar and (Character.Map.Characters.Count > 0)) or (not OtherChar) then
        ChangeChannel(OtherChar)
      else if OtherChar then
        Log('Did not change channel as other char is a botter.');
  end);
end;       *)

procedure TakeDrop(ObjectID, ID: Integer; IsMesos: Boolean);
var
  I: PItem;
  Packet: TMapleStream;
  Sum: Cardinal;
begin
  if IsMesos then
    Sum := 0
  else
  begin
    I := frmItems.GetItem(ID);
    if I = nil then
      Exit
    else
      Sum := I.Checksum;
    Log2('You have gained an item (%s)', [I.Name]);
  end;

  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_TAKE_DROP);
    WriteByte(Character.UsedPortals);
    WriteInt(GetTickCount);
    WritePos(Character.Position);
    WriteInt(ObjectID);
    WriteInt(Sum);
  end;
  frmWvsFake.Write(Packet);
end;

procedure UseItem(Slot: SmallInt; ID: Integer);
var
  Packet: TMapleStream;
begin
  Packet := TMapleStream.Create;
  with Packet do
  begin
    WriteShort(S_USE_ITEM);
    WriteInt(GetTickCount);
    WriteShort(Slot);
    WriteInt(ID);
  end;
  frmWvsFake.Write(Packet);
end;

procedure HandlePacket(Packet: TMapleStream);
var
  Opcode: Word;
  v1, v2, i: Integer;
  B: Boolean;
  s: string;
  Char: TMapleCharacter;
  Mob: TMonster;
  NPC: TNPC;
  Pos: TPoint;
  T: TMapleInventoryType;
  Portal: PPortal;
begin
  Opcode := Packet.ReadShort;
  case Opcode of
    R_LOGINSTATUS:
    begin
      v1 := Packet.ReadInt;

      if v1 = 0 then
      begin
        Log('Login successful.');
        RequestCharList(frmWorlds.GetWorld);
      end
      else
        Log('Login failed! ' + IntToStr(v1));
    end;

    R_CONNECT_AUTH:
    begin
      Packet.Skip(10);
      Packet.Skip(Packet.ReadShort);
      Packet.Skip(22);
      Packet.Read(ConAuth, 8);
    end;

    R_CHARACTER_LIST:
    begin
      Packet.Skip(1);
      v1 := Packet.ReadByte;
      Log('Account has %d character(s).', [v1]);
      if v1 = 0 then
        Exit;

      Character := nil;
      for i := 0 to v1 - 1 do
      begin
        Char := ReadCharEntry(Packet);
        Log(Char.ToString);
        if Mode <> CHARSEL then
        begin
          if Char.Job div 100 = 33 then
            Character := Char
          else
            Char.Free;
        end
        else
          frmCharSel.AddCharacter(Char);
      end;

      if Mode = CHARSEL then
        frmCharSel.ShowModal;

      if Character <> nil then
      begin
        Character.LoadMap;
        Character.UpdateGUI;
        frmWvsFake.lblName.Caption := Character.Name;
        Log('Using %s, map %d', [Character.Name, Character.Map.ID]);
        SelectCharacter;
      end;
    end;

    R_SERVER_IP, R_CHANGE_CHANNEL:
    begin
      Log('Got ServerIP packet');
      if Opcode = R_SERVER_IP then
        Packet.Skip(2)
      else
      begin
        if Assigned(Recon) then
          CurChannel := Recon.Destination
        else
          Log('EVERYTHING SUCKS!');
        Packet.Skip(1);
      end;
      s := '';
      for v1 := 0 to 3 do
        s := s + IntToStr(Packet.ReadByte) + '.';
      Delete(s, Length(s), 1);  // Last dot

      v2 := Packet.ReadShort;
      Log('Connecting to channel-server: %s:%d', [s, v2]);
      frmWvsFake.Reconnect(s, v2);
    end;

    R_PING: Pong;

    R_MODIFY_INVENTORY:
    begin
      Packet.Skip(1);  // EnableActions
      v1 := Packet.ReadShort;
      if v1 = 0 then
        Exit;

      T := TMapleInventoryType(Packet.ReadByte);
      i := Packet.ReadByte;
      v2 := Packet.ReadByte;

      if v2 > 0 then
        Log('MODIFY_INVENTORY v2: %d', [v2]);

      case v1 of
        $0001: // Add
          Character.Inventory[T].AddFromPacket(Packet, i, True);

        $0101: // Update (01 01)
          Character.Inventory[T][i].Quantity := Packet.ReadShort;

        $0301: // Remove (01 03)
          Character.Inventory[T].RemoveSlot(i);

        else
        try
        Log('Unknown MODIFY_INVENTORY mode: %d', [v1]);
        except

        end;
      end;

      if frmInvGUI.Showing then
        frmInvGUI.DoPaint;
    end;

    R_UPDATE_STATS:
    begin
      Packet.Skip(1);
      v1 := Packet.ReadInt;

      if v1 = 0 then
        Exit;

      i := 1;
      while (i <= $40000) and (i <= v1) do
      begin
        if (v1 and i) = i then
        begin
          case i of
            $01: Character.SkinColor := Packet.ReadShort;
            $02: Character.Face := Packet.ReadInt;
            $04: Character.Hair := Packet.ReadInt;
            $08: if v1 <> 1572872 then Log('Unknown stat 8, %s', [Packet.ToString]);
            $10: Character.Level := Packet.ReadByte;
            $20: Character.Job := Packet.ReadShort;
            $40: Character.Str := Packet.ReadShort;
            $80: Character.Dex := Packet.ReadShort;
            $100: Character.Int := Packet.ReadShort;
            $200: Character.Luk := Packet.ReadShort;
            $400: Character.HP := Packet.ReadInt;
            $800: Character.MaxHP := Packet.ReadInt;
            $1000: Character.MP := Packet.ReadInt;
            $2000: Character.MaxMP := Packet.ReadInt;
            $4000: Character.AP := Packet.ReadShort;
            $8000: Log('SP table');
            $10000: Character.Exp := Packet.ReadInt;
            $20000: Character.Fame := Packet.ReadInt;
            $40000: Character.Mesos := Packet.ReadInt;
          end;
        end;
        i := i * 2;
      end;
    end;

    R_GIVE_BUFF:
    begin
      Log('Got buff');
    end;

    R_CANCEL_BUFF:
    begin
      //Log('HIDE cancelled');
    end;

    R_REMOVE_TEMPORARY_STATS: ;
    R_UPDATE_SKILLS: ;

    R_SHOW_INFO:
    begin
      case Packet.ReadByte of
        0:
        begin
          if Packet.ReadByte <> 1 then
            Exit;

          Packet.Skip(1);
          Log2('You have gained mesos (+%d)', [Packet.ReadInt]);
        end;

        3:
        begin
          Packet.Skip(1);
          Log2('You have gained experience (+%d)', [Packet.ReadInt]);
        end;
      end;
    end;

    R_NOTES: ;
    R_ENABLE_REPORT: ;
    R_UPDATE_MOUNT: ;  // Wild Huter mount update
    R_GENDER: ;
    R_PARTY_OPERATION: ;

    R_BUDDY_LIST:
    begin
      if RestartAtt then
      begin
        RestartAtt := False;
        Attacker := TAttacker.Create(Character.Map);
      end;
    end;

    R_GUILD_OPERATION: ;
    R_SERVER_MESSAGE: ;
    R_EVENT_STAT: ; // Server always sends it when entering a channel, 'kill_count' o.o
    R_OPEN_FAMILY: ;

    R_WARP_TO_MAP:
    begin
      AttackerOID := 0;
      Character.Map.ClearObjects;
      Packet.Skip(34);

      if Packet.Size < 200 then
      begin
        Packet.Skip(1);
        Character.LoadMap(Packet.ReadInt);
        Character.Spawnpoint := Packet.ReadByte;
        Character.Spawn;
        Exit;
      end
      else
      begin
        Character.UsedPortals := 1;
        Character.Spawn;
      end;

      R1 := Packet.ReadInt or $100000;
      R2 := Packet.ReadInt or $1000;
      R3 := Packet.ReadInt or $10;

      Packet.Skip(10);
      ReadCharStats(Packet, Character);
      Packet.ReadByte;  // BuddyList Capacity
      if Packet.ReadByte = 1 then  // Link
        Packet.Skip(Packet.ReadShort);
      if Packet.ReadByte = 1 then  // Link 2
        Packet.Skip(Packet.ReadShort);
      Packet.ReadByte; // ?

      ExtendedParser.ReadInventory(Packet, Character);

      Portal := Character.Map.GetPortal('MD00');
      if (Portal <> nil) and (frmWvsFake.cbMDRush.Checked) then
        SMapRush('MD00', Portal.X, Portal.Y);
    end;

    R_SPAWN_PLAYER:
    begin
      v1 := Packet.ReadInt;
      Packet.Skip(1);
      s := Packet.ReadMapleAnsiString;
      Log('*** ' + s + ' spawned');       // paladin
      if v1 <> Character.ID then
      begin
        Character.Map.Characters.AddOrSetValue(v1, s);
        if (Mode = HACK) and (v1 <> 5358243) then//(v1 <> 1631625 sader) then
          if not Assigned(Recon) then
            Recon := TReconnector.Create(v1)
          else
            Log('Did not change channel: Already reconnecting?');
      end;
    end;

    R_REMOVE_PLAYER:
    begin
      v1 := Packet.ReadInt;
      if Character.Map.Characters.ContainsKey(v1) then
      begin
        Log('*** ' + Character.Map.Characters[v1] + ' left');
        Character.Map.Characters.Remove(v1);
      end;
    end;

    R_CHAT:
    begin
      v1 := Packet.ReadInt;
      Packet.Skip(1);
      s := Packet.ReadMapleAnsiString;
      Log(Format('%s : %s', [Character.Map.Characters[v1], s]));
    end;

    R_SHOW_ITEM_GAIN_INCHAT:
    begin
      if (Packet.ReadByte = 5) and (Packet.ReadByte = 1) then
      begin
        Log2('You have gained an item (%s)', [StringData.GetItemProp(Packet.ReadInt, 'name')]);
      end;
    end;

    R_SPAWN_MONSTER:
    begin
      v1 := Packet.ReadInt;
      Packet.Skip(1);
      v2 := Packet.ReadInt;
      Mob := GetMonster(v2);
      Mob.ObjectID := v1;
      Character.Map.AddMonster(Mob);
    end;

    R_REMOVE_MONSTER:
    begin
      v1 := Packet.ReadInt;
      Character.Map.RemoveMonster(v1);
    end;

    R_MONSTER_HP:
    begin
      v1 := Packet.ReadInt;
      Mob := Character.Map.GetMonster(v1);
      if Mob <> nil then
        Mob.ServerHP := Packet.ReadByte;
    end;

    R_SPAWN_NPC:
    begin
      v1 := Packet.ReadInt;
      NPC := TNPC.Create(Packet.ReadInt);
      NPC.ObjectID := v1;
      Character.Map.AddNPC(NPC);
    end;

    R_SPAWN_DROP:
    begin
      if (Packet.ReadByte <> 0) then
        Exit;

      v1 := Packet.ReadInt;
      B := Packet.ReadByte = 1;
      i := Packet.ReadInt;
      Packet.Skip(5);
      Pos := Packet.ReadPos;
      v2 := Character.Map.ObjectToRealID(Packet.ReadInt);

      if (v2 <> -1) and (not B) and (not DropDB.Contains(v2, i)) then
      begin
        DropDB.Add(v2, i);

        if frmWvsFake.cbServerCtrl.Checked then
          frmWvsFake.SyncItemLog(v2, i);
      end;

      if PtInRect(Rect(Pos.X - 25, Pos.Y - 10, Pos.X + 25, Pos.Y + 10), Character.Position) then
      begin
        Sched.AddSchedule(1000 + Random(1000), procedure begin TakeDrop(v1, i, B) end);
      end;
    end;

    R_NPC_TALK:
    begin
      Packet.Skip(5);
      if Assigned(Character.OnNPCTalk) then
        Character.OnNPCTalk(Packet.ReadByte);
    end;

    else if Opcode < 100 then  Log('! Received ' + IntToStr(Opcode));
  end;
end;

end.
