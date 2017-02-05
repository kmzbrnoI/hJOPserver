unit DataMTB;

// TMTBTableData - trida starajici se o vyplnovani tabulky MTB

interface

uses ComCtrls, SysUtils;

type
  TMTBTableData=class
    private
      LV:TListView;

    public

      procedure LoadToTable();
      procedure UpdateLine(Board:integer);
      procedure UpdateTable();

      constructor Create(LV:TListView);
  end;//TMTBData

var
   MTBTableData:TMTBTableData;


implementation

uses TechnologieMTB, RCS, Prevody;

////////////////////////////////////////////////////////////////////////////////

constructor TMTBTableData.Create(LV:TListView);
begin
 inherited Create();
 Self.LV := LV;
end;//ctor

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.LoadToTable();
var i, j:integer;
    LI:TListItem;
 begin
  Self.LV.Clear();

  for i := 0 to TMTB._MAX_MTB-1 do
   begin
    LI := Self.LV.Items.Add();
    LI.Caption := IntToStr(i);
    for j := 0 to Self.LV.Columns.Count-1 do
      LI.SubItems.Add('');

    Self.UpdateLine(i);
   end;//for i
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.UpdateTable();
var i:Integer;
begin
 for i := 0 to TMTB._MAX_MTB-1 do
  Self.UpdateLine(i);
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TMTBTableData.UpdateLine(board:integer);
var j:integer;
    output:Integer;
    LI:TListItem;
 begin
  LI := Self.LV.Items[board];

  Self.LV.Items.Item[board].Caption := IntToStr(board);

  if (not MTB.ready) then
   begin
    LI.SubItems.Strings[0] := '';
    LI.SubItems.Strings[1] := '';
    LI.SubItems.Strings[2] := '';
    LI.SubItems.Strings[3] := '-------- --------';
    LI.SubItems.Strings[4] := '-------- --------';
    LI.SubItems.Strings[5] := '-';
    LI.SubItems.Strings[6] := '-';

    Exit();
   end;

  try
    if (MTB.IsModule(board)) then
     begin
      case (MTB.GetModuleType(board)) of
       _RCS_MOD_MTB_UNI_ID    : LI.ImageIndex := 0;
       _RCS_MOD_MTB_TTL_ID    : LI.ImageIndex := 1;
       _RCS_MOD_MTB_TTLOUT_ID : LI.ImageIndex := 2;
       _RCS_MOD_MTB_REGP_ID   : LI.ImageIndex := 3;
       _RCS_MOD_MTB_POT_ID    : LI.ImageIndex := 4;
       _RCS_MOD_MTB_UNIOUT_ID : LI.ImageIndex := 5;
      else
       Self.LV.Items.Item[board].ImageIndex := -1;
      end;
     end else
       Self.LV.Items.Item[board].ImageIndex := -1;
  except
    Self.LV.Items.Item[board].ImageIndex := -1;
  end;

  if (MTB.GetNeeded(board)) then
    LI.SubItems.Strings[0] := 'X'
  else
    LI.SubItems.Strings[0] := '';

  try
    LI.SubItems.Strings[1] := MTB.GetModuleName(board);
  except
    on E:Exception do
      LI.SubItems.Strings[1] := E.Message;
  end;

  try
    if (MTB.IsModule(board)) then
      LI.SubItems.Strings[2] := MTB.ModuleTypeToStr(MTB.GetModuleType(board))
    else
      LI.SubItems.Strings[2] := '-';
  except
    on E:Exception do
      LI.SubItems.Strings[2] := E.Message;
  end;

  try
    if (MTB.Opened) then
     begin
      if (MTB.IsModule(board)) then
       begin
        if (MTB.Started) then
         begin
          LI.SubItems.Strings[3] := '';
          LI.SubItems.Strings[4] := '';

          for j := 0 to 15 do
           begin
            case (MTB.GetInput(board, j)) of
              isOn          : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '1';
              isOff         : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '0';
              failure       : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + 'X';
              notYetScanned : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '?';
              unavailable   : LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + '-';
            end;

            output := MTB.GetOutput(board, j);
            if (output > 1) then
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4]+'S'
            else
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4]+IntToStr(output);

            if (j = 7) then
             begin
              LI.SubItems.Strings[3] := LI.SubItems.Strings[3] + ' ';
              LI.SubItems.Strings[4] := LI.SubItems.Strings[4] + ' ';
             end;//if
           end;//for
         end else begin
          LI.SubItems.Strings[3] := '-------- --------';
          LI.SubItems.Strings[4] := '-------- --------';
         end;

        LI.SubItems.Strings[5] := 'Ano';
        LI.SubItems.Strings[6] := MTB.GetModuleFW(board);
       end else begin
        // neexistuje
        LI.SubItems.Strings[3] := '-------- --------';
        LI.SubItems.Strings[4] := '-------- --------';
        LI.SubItems.Strings[5] := 'Ne';
        LI.SubItems.Strings[6] := '-';
       end;
     end else begin
      // mtb closed
      LI.SubItems.Strings[3] := '-------- --------';
      LI.SubItems.Strings[4] := '-------- --------';
      LI.SubItems.Strings[5] := '-';
      LI.SubItems.Strings[6] := '-';
     end;
  except
    on E:Exception do
     begin
      LI.SubItems.Strings[3] := 'Exception';
      LI.SubItems.Strings[4] := E.Message;
      LI.SubItems.Strings[5] := 'Ex';
      LI.SubItems.Strings[6] := 'Ex';
     end;
  end;
 end;//procedure

////////////////////////////////////////////////////////////////////////////////

initialization

finalization
 MTBTableData.Free();

end.//unit
