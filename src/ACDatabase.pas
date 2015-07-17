unit ACDatabase;

interface

uses Generics.Collections, AC, IniFiles, SysUtils, Classes;

type
  //AC database
  TACDb = class
    private
      ffilename:string;

      procedure SaveStat();

    public

      ACs:TList<TAC>;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadFromFile(filename:string);
      procedure SaveToFile(filename:string);

      function AddAC():TAC;
      function RemoveAC(index:Cardinal):Integer;

      procedure Update();

      procedure StopAllACs();

      property filename:string read ffilename;
  end;

var
  ACDb   : TACDb;

implementation

uses Logging, DataAC, fMain;

////////////////////////////////////////////////////////////////////////////////

constructor TACDb.Create();
begin
 inherited Create();

 Self.ACs := TList<TAC>.Create;
end;//ctor

destructor TACDb.Destroy();
var i:Integer;
begin
 Self.SaveStat();
 for i := 0 to Self.ACs.Count-1 do Self.ACs[i].Free();
 Self.ACs.Free();
 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

//load ACs db from ini file
procedure TACDb.LoadFromFile(filename:string);
var ini:TMemIniFile;
    i:Integer;
    sections:TStrings;
    AC:TAC;
    krk_file:string;
begin
 writelog('Nacitam AC - '+filename, WR_DATA);
 Self.ffilename := filename;
 Self.ACs.Clear();

 try
   ini := TMemIniFile.Create(filename);
 except
   writelog('Nelze nacist databazi AC - nelze otevrit soubor - '+filename, WR_ERROR);
   Exit();
 end;

 sections := TStringList.Create();
 ini.ReadSections(sections);

 for i := 0 to sections.Count-1 do
  begin
   AC := nil;
   try
     krk_file := ini.ReadString(sections[i], 'file', '');
     AC := TAC.Create(krk_file);
   except
     on E:Exception do
      begin
       writelog('Chyba pri nacitani souboru AC - '+krk_file+' : '+E.Message, WR_ERROR);
       if (Assigned(AC)) then AC.Free();
       continue;
      end;
   end;

   AC.stat_run  := ini.ReadInteger(sections[i], 'stat_run', 0);
   AC.stat_end  := ini.ReadInteger(sections[i], 'stat_end', 0);

   Self.ACs.Add(AC);
  end;//for i

 ini.Free();
 writelog('Nacteno '+IntToStr(Self.ACs.Count)+' AC', WR_DATA);

 ACTableData.LoadToTable();
end;//function

//save ACs db to inifile
procedure TACDb.SaveToFile(filename:string);
var ini:TMemIniFile;
    i:Integer;
begin
 try
   ini := TMemIniFile.Create(filename);
 except
   writelog('Nelze ulozit databazi AC - nelze otevrit soubor - '+filename, WR_ERROR);
   Exit();
 end;

 ini.Clear();

 for i := 0 to Self.ACs.Count-1 do
  begin
   ini.WriteInteger(IntToStr(i), 'stat_run', Self.ACs[i].stat_run);
   ini.WriteInteger(IntToStr(i), 'stat_end', Self.ACs[i].stat_end);
   ini.WriteString (IntToStr(i), 'file',     Self.ACs[i].krk_filename);
  end;//for i

 ini.UpdateFile();
 ini.Free();
end;//procedure

procedure TACDb.SaveStat();
var ini:TMemIniFile;
    i:Integer;
begin
 //save stat data
 try
   ini := TMemIniFile.Create(Self.filename);
 except
   writelog('Nelze ulozit statistiky AC - nelze otevrit soubor - '+filename, WR_ERROR);
   Exit();
 end;

 for i := 0 to Self.ACs.Count-1 do
  begin
   ini.WriteInteger(IntToStr(i), 'stat_run', Self.ACs[i].stat_run);
   ini.WriteInteger(IntToStr(i), 'stat_end', Self.ACs[i].stat_end);
  end;
end;//procedure

////////////////////////////////////////////////////////////////////////////////

function TACDb.AddAC():TAC;
var AC:TAC;
begin
 AC := TAC.Create();
 Self.ACs.Add(AC);
 ACTableData.AddAC();
 Result := AC;
end;//function

function TACDb.RemoveAC(index:Cardinal):Integer;
begin
 if (Integer(index) >= Self.ACs.Count) then Exit(1);
 Self.ACs[index].Free();
 Self.ACs.Delete(index);
 ACTableData.RemoveAC(index);
 Result := 0;
end;//function

////////////////////////////////////////////////////////////////////////////////

procedure TACDb.Update();
var i:Integer;
begin
 for i := 0 to Self.ACs.Count-1 do Self.ACs[i].Update();   
end;//procedure

////////////////////////////////////////////////////////////////////////////////

procedure TACDb.StopAllACs();
var i:Integer;
begin
 for i := 0 to Self.ACs.Count-1 do
  if (Self.ACs[i].running) then Self.ACs[i].Stop();
 F_Main.LV_AC_Db.ItemIndex := -1;
end;//procedure

initialization
  ACDb := TACDb.Create();

finalization
  //save data
  FreeAndNil(ACDb);

end.//unit

