unit ACDatabase;

interface

uses Generics.Collections, AC, IniFiles, SysUtils, Classes;

const
 _FILE_SUFFIX = '.krk';

type
  //AC database
  TACDb = class
    private
      fstatfilename:string;
      fdirname:string;

      function GetACRunning():boolean;

    public

      ACs:TObjectList<TAC>;

      constructor Create();
      destructor Destroy(); override;

      procedure LoadFromDir(const dirname:string);
      procedure LoadStatFromFile(const filename:string);
      procedure SaveStatToFile(const filename:string);

      function AddAC():TAC;
      procedure RemoveAC(index:Cardinal);

      procedure Update();

      procedure StopAllACs();

      property statfilename:string read fstatfilename;
      property dirname:string read fdirname;
      property acRunning:boolean read GetACRunning;
  end;

var
  ACDb   : TACDb;

implementation

uses Logging, DataAC, fMain, appEv;

////////////////////////////////////////////////////////////////////////////////

constructor TACDb.Create();
begin
 inherited Create();

 Self.ACs := TObjectList<TAC>.Create;
end;//ctor

destructor TACDb.Destroy();
begin
 try
   if (Self.fstatfilename <> '') then
     Self.SaveStatToFile(Self.fstatfilename);
 except

 end;
 Self.ACs.Free();
 inherited;
end;//dtor

////////////////////////////////////////////////////////////////////////////////

//load ACs db from ini file
procedure TACDb.LoadFromDir(const dirname:string);
var AC:TAC;
    SR:TSearchRec;
begin
  writelog('Nacitam AC - ' + dirname, WR_DATA);
  Self.fdirname := dirname;

  Self.ACs.Clear();

  // prohledavani adresare a nacitani soubor *.2lok
  // najdeme prvni soubor
  if (FindFirst(dirname+'\*'+_FILE_SUFFIX, faAnyFile, SR) = 0) then
   begin
    if ((SR.Attr AND faDirectory) = 0) then
     begin
      try
        AC := TAC.Create(dirname+'\'+SR.Name);
        Self.ACs.Add(AC);
      except
        on E:Exception do
          writelog('Nelze nacist AC ' + SR.Name + ' : ' + E.Message, WR_ERROR);
      end;
     end;

    // hledame dalsi soubory
    while (FindNext(SR) = 0) do
     begin
      if ((SR.Attr AND faDirectory) = 0) then
       begin
        try
          AC := TAC.Create(dirname+'\'+SR.Name);
          Self.ACs.Add(AC);
        except
          on E:Exception do
            writelog('Nelze nacist AC ' + SR.Name + ' : ' + E.Message, WR_ERROR);
        end;
       end;
     end;

    SysUtils.FindClose(SR);
    writelog('Naèteno '+IntToStr(Self.ACs.Count)+' AC', WR_DATA);
   end else begin
    writelog('Nenacteno zadne AC',WR_DATA);
   end;

  ACTableData.LoadToTable();
end;

//load ACs db from ini file
procedure TACDb.LoadStatFromFile(const filename:string);
var ini:TMemIniFile;
    AC:TAC;
    krk_filename:string;
begin
 writelog('Nacitam AC - '+filename, WR_DATA);
 Self.fstatfilename := filename;

 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Nelze nacist statistiku AC - nelze otevrit soubor - '+filename);
     Exit();
    end;
 end;

 for AC in Self.ACs do
  begin
   krk_filename := ExtractFileName(AC.krk_filename);
   AC.stat_run := ini.ReadInteger(krk_filename, 'stat_run', 0);
   AC.stat_end := ini.ReadInteger(krk_filename, 'stat_end', 0);
  end;//for i

 ini.Free();
 ACTableData.UpdateTable(true);
end;

procedure TACDb.SaveStatToFile(const filename:string);
var ini:TMemIniFile;
    AC:TAC;
    krk_filename:string;
begin
 //save stat data
 try
   ini := TMemIniFile.Create(filename, TEncoding.UTF8);
 except
   on E:Exception do
    begin
     AppEvents.LogException(E, 'Nelze ulozit statistiky AC - nelze otevrit soubor - '+filename);
     Exit();
    end;
 end;

 try
   for AC in Self.ACs do
    begin
     krk_filename := ExtractFileName(AC.krk_filename);
     ini.WriteInteger(krk_filename, 'stat_run', AC.stat_run);
     ini.WriteInteger(krk_filename, 'stat_end', AC.stat_end);
    end;

   ini.UpdateFile();
 finally
   ini.Free();
 end;
end;

////////////////////////////////////////////////////////////////////////////////

function TACDb.AddAC():TAC;
var AC:TAC;
begin
 AC := TAC.Create();
 Self.ACs.Add(AC);
 ACTableData.AddAC();
 Result := AC;
end;

procedure TACDb.RemoveAC(index:Cardinal);
begin
 Self.ACs.Delete(index);
 ACTableData.RemoveAC(index);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TACDb.Update();
var i:Integer;
begin
 for i := 0 to Self.ACs.Count-1 do Self.ACs[i].Update();   
end;

////////////////////////////////////////////////////////////////////////////////

procedure TACDb.StopAllACs();
var i:Integer;
begin
 for i := 0 to Self.ACs.Count-1 do
  if (Self.ACs[i].running) then Self.ACs[i].Stop();
 F_Main.LV_AC_Db.ItemIndex := -1;
end;

////////////////////////////////////////////////////////////////////////////////

function TACDb.GetACRunning():boolean;
var AC:TAC;
begin
 for AC in Self.ACs do
   if (AC.running) then
     Exit(true);
 Result := false;
end;

////////////////////////////////////////////////////////////////////////////////

initialization
  ACDb := TACDb.Create();

finalization
  //save data
  FreeAndNil(ACDb);

end.//unit

