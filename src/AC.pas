unit AC;

// Trida definujici AC
//   Stezejni casti teto tridy je parsing souboru .krk

interface

uses Classes, fMain, IniFiles, SysUtils, TBlok, TechnologieJC, TBloky,
     TBlokUsek, ComCtrls, Prevody, Windows, Generics.Collections,
     ownStrUtils, StrUtils, IdContext;

const
  _AC_CMDTYPE_END   = 0;
  _AC_CMDTYPE_JC    = 1;
  _AC_CMDTYPE_USEK  = 2;
  _AC_CMDTYPE_OSV   = 3;
  _AC_CMDTYPE_TRAT  = 4;
  _AC_CMDTYPE_DELAY = 5;
  _AC_CMDTYPE_NAV   = 6;

const
  _MAX_AC   = 32;
  _MAX_COND = 32;

type
  // parametry kroku
  TKrkParams = array [0..15] of Integer;

  //one AC step
  TKrok = record
   command:integer;
         //command:
         //0 = konec
         //1 = 'jc'
         //2 = 'usek'
         //3 = 'osv'
         //4 = 'trat'
         //5 = 'delay'
         //6 = 'nav'
   Params:TKrkParams;
  end;

  TACPodminka = record
    usekID:Integer;
    stav:Integer;
  end;

  //one AC
  TAC = class
    private
      fkrk_filename:string;
      krok:Integer;
      subkrok:Integer;
      frunning:boolean;
      wait:TDateTime;

      function GetReady():boolean;

      procedure UpdateKrok();
      procedure NextStep(); inline;
      function GetPaused():boolean;

    public
      name:string;
      opak_time:TTime;
      repeating:boolean;
      changed:boolean;

      stat_run:Integer;
      stat_end:Integer;

      kroky:TList<TKrok>;
      podm:TList<TACPodminka>;

      constructor Create(krk_filename:string); overload;
      constructor Create(); overload;
      destructor Destroy(); override;

      procedure LoadKrkFile(filename:string);
      procedure ClearStatistics();

      procedure Update();

      procedure Start();
      procedure Stop();
      procedure Pause();

      property krk_filename:string read fkrk_filename write fkrk_filename;
      property ready:boolean read GetReady;
      property running:boolean read frunning;
      property paused:boolean read getPaused;
      property ACKrok:Integer read krok;
  end;

implementation

uses fSettings, GetSystems, TJCDatabase, Logging,
      TOblRizeni, TOblsRizeni, TBlokNav, TBlokTrat, Zasobnik;

////////////////////////////////////////////////////////////////////////////////

constructor TAC.Create(krk_filename:string);
begin
 inherited Create();

 Self.changed  := false;
 Self.frunning := false;
 Self.krok     := -1;
 Self.kroky    := TList<TKrok>.Create();
 Self.podm     := TList<TACPodminka>.Create();
 Self.LoadKrkFile(krk_filename);
end;//ctor

constructor TAC.Create();
begin
 inherited Create();

 Self.kroky := TList<TKrok>.Create();
 Self.podm  := TList<TACPodminka>.Create();
end;//ctor

destructor TAC.Destroy();
begin
 Self.kroky.Free();
 Self.podm.Free();

 inherited Destroy();
end;//dtor

////////////////////////////////////////////////////////////////////////////////

// format souboru s kroky:
//  povinna hlavicka:
//    "AC Nedvedice" - nazev AC
//    "00:10"        - cas opakovani AC
//    "(15,1)(16,8)(...)..."    - podminky pro AC (blok-usek,stav)...
//    [prazdny_radek]
//    krok_1
//    krok_2
//    ...


// pozn.:
//    znakem "#" zacina komentar, ktery je ignorovany
//    veskere prazdne znaky jsou ignorovany

// nacitani souboru s kroky:
procedure TAC.LoadKrkFile(filename:string);
var Blk:TBlk;
    lines, data, data2:TStrings;
    i, j:Integer;
    tmp:string;
    ACpodm:TACPodminka;
    krok:TKrok;
 begin
  lines := nil;
  data  := nil;
  data2 := nil;

  try

    try
      Self.fkrk_filename := filename;

      Self.kroky.Clear();

      // nacist soubor do StringList
      lines := TStringList.Create();
      try
        lines.LoadFromFile(filename, TEncoding.UTF8);
      except
        raise Exception.Create('Nelze nacist soubor');
      end;

      for i := 0 to lines.Count-1 do
       begin
        // odstranit bile znaky
        if (i > 0) then
         begin
          tmp := RemoveWhiteSpace(lines[i]);
          lines[i] := tmp;
         end;

        // odstranit komentare
        for j := 1 to Length(lines[i]) do
         if (lines[i][j] = '#') then
          begin
           lines[i] := LeftStr(lines[i], j-1);
           break;
          end;
       end;

      // odstranit mezery na krajich stringu nazvu (zejmena kvuli komentari vpravo)
      lines[0] := Trim(lines[0]);

      // odtsranit prazdne radky
      i := 0;
      while (i < lines.Count) do
       begin
        if (lines[i] = '') then
         lines.Delete(i)
        else
         Inc(i);
       end;


      // kontrola velikosti souboru
      if (lines.Count < 4) then
        raise Exception.Create('Prilis malo radku - neosahuje hlavicku');

      data  := TStringList.Create();
      data2 := TStringList.Create();

      // nacist hlavicku:
      try
        Self.name      := lines[0];
        Self.opak_time := EncodeTime(0, StrToInt(LeftStr(lines[1], 2)), StrToInt(RightStr(lines[1], 2)), 0);

        data.Clear();
        ExtractStrings(['(', ')'], [], PChar(lines[2]), data);
        Self.podm.Clear();
        for i := 0 to data.Count-1 do
         begin
          data2.Clear();
          ExtractStrings([',', ';'], [], PChar(data[i]), data2);
          try
            ACpodm.usekID := StrToInt(data2[0]);
            ACpodm.stav   := StrToInt(data2[1]);
            Self.podm.Add(ACpodm);
          except
            raise Exception.Create('Nevalidni podminky');
          end;
         end;

      except
        raise Exception.Create('Nevalidni hlavicka');
      end;


      // nacist jednotlive kroky (prikazy):
      for i := 3 to lines.Count-1 do
       begin
        //zmenseni znaku na mala pismena
        lines[i] := LowerCase(lines[i]);

        //seznam validnich kroku:
        //validni oddelovace: ,;()
             //0 = konec
             //1 = 'jc' [index]                 stavi a ceka na postaveni cesty index
             //2 = 'usek' [index,stav]          ceka na stav useku index
             //3 = 'osv' [stanice,index,stav]   zmeni osvetleni index v dane stanici na stav
             //4 = 'trat' [index,smer]          nastavi danou trat do daneho smeru
             //5 = 'delay' [m,s,ms]             ceka cas m,s,ms
             //6 = 'nav' [jc,navest]           ceka na navest na navestidle jizdni cesty jc
                //navest muze nabyvat hodnot 0,1

        //parsing dat
        data.Clear();
        ExtractStrings([';','(',')',','],[], PChar(lines[i]), data);

        if (data[0] = 'jc') then
         begin
          if (data.Count < 2) then  raise Exception.Create('Prilis malo parametru - '+lines[i]);

          try
            krok.command   := _AC_CMDTYPE_JC;
            krok.Params[0] := StrToInt(data[1]);
          except
            raise Exception.Create('Nevalidni format parametru - '+lines[i]);
          end;

          if (JCDb.GetJCByID(krok.Params[0]) = nil) then  raise Exception.Create('JC neexistuje - '+lines[i]);

          Self.kroky.Add(krok);
         end else

        //////////////////////////////////////////////////

        if (data[0] = 'usek') then
         begin
          if (data.Count < 3) then  raise Exception.Create('Prilis malo parametru - '+lines[i]);

          try
            krok.command   := _AC_CMDTYPE_USEK;
            krok.Params[0] := StrToInt(data[1]);
            krok.Params[1] := StrToInt(data[2]);
          except
            raise Exception.Create('Nevalidni format parametru - '+lines[i]);
          end;

          Blky.GetBlkByID(krok.Params[0], Blk);
          if ((not Assigned(Blk)) or ((Blk.typ <> _BLK_USEK) and (Blk.typ <> _BLK_TU))) then
            raise Exception.Create('Nevalidni blok - '+lines[i]);

          Self.kroky.Add(krok);
         end else

        //////////////////////////////////////////////////

        if (data[0] = 'osv') then
         begin
          // to-do
         end;

        //////////////////////////////////////////////////

        if (data[0] = 'trat') then
         begin
          if (data.Count < 3) then  raise Exception.Create('Prilis malo parametru - '+lines[i]);

          try
            krok.command   := _AC_CMDTYPE_TRAT;
            krok.Params[0] := StrToInt(data[1]);
            krok.Params[1] := StrToInt(data[2]);
          except
            raise Exception.Create('Nevalidni format parametru - '+lines[i]);
          end;

          Blky.GetBlkByID(krok.Params[0], Blk);
          if ((blk = nil) or (blk.typ <> _BLK_TRAT)) then
            raise Exception.Create('Nevalidni blok - '+lines[i]);

          Self.kroky.Add(krok);
         end;

        //////////////////////////////////////////////////

        if (data[0] = 'delay') then
         begin
          if (data.Count < 2) then  raise Exception.Create('Prilis malo parametru - '+lines[i]);

          try
            krok.command   := _AC_CMDTYPE_DELAY;
            krok.Params[0] := StrToInt(data[1]);
          except
            raise Exception.Create('Nevalidni format parametru - '+lines[i]);
          end;

          Self.kroky.Add(krok);
         end;

        //////////////////////////////////////////////////

        if (data[0] = 'nav') then
         begin
          if (data.Count < 2) then  raise Exception.Create('Prilis malo parametru - '+lines[i]);

          try
            krok.command   := _AC_CMDTYPE_NAV;
            krok.Params[0] := StrToInt(data[1]);
          except
            raise Exception.Create('Nevalidni format parametru - '+lines[i]);
          end;

          Blky.GetBlkByID(krok.Params[0], Blk);
          if ((not Assigned(Blk)) or (Blk.typ <> _BLK_NAV)) then
            raise Exception.Create('Nevalidni blok - '+lines[i]);

          Self.kroky.Add(krok);
         end;

        //-----nav End-----

       end;//for

    except
      Self.kroky.Clear();
      Self.podm.Clear();
      raise;
    end;

  finally
   if (Assigned(lines)) then lines.Free();
   if (Assigned(data))  then data.Free();
   if (Assigned(data2)) then data2.Free();
  end;

end;

////////////////////////////////////////////////////////////////////////////////

function TAC.GetReady():boolean;
var i:Integer;
    Blk:TBlk;
begin
 for i := 0 to Self.podm.Count-1 do
  begin
   Blky.GetBlkByID(Self.podm[i].usekID, Blk);
   if ((Blk = nil) or (Integer((Blk as TBlkUsek).Stav.Stav) <> Self.podm[i].stav)) then Exit(false);
  end;
 Result := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAC.ClearStatistics();
begin
 Self.stat_run := 0;
 Self.stat_end := 0;
 Self.changed := true;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAC.Update();
begin
 if (Self.running) then Self.UpdateKrok();
end;

////////////////////////////////////////////////////////////////////////////////
// zapnout AC:

procedure TAC.Start();
begin
 if (Self.frunning) then Exit();

 if (Self.krok > -1) then
  begin
   Self.frunning := true;
   writelog('AC '+Self.name+ ' : RESUME', WR_AUTREZ);
  end else begin
   if (not Self.ready) then
    raise Exception.Create('Podmínky pro spuštìní nesplnìny!')
   else begin
     Self.krok := 0;
     Self.frunning  := true;
     Self.repeating := false;
     Inc(Self.stat_run);
     Self.changed := true;
     writelog('AC '+Self.name+ ' : START', WR_AUTREZ);
   end;
  end;

 Self.changed := true;
 F_Main.LV_AC_Kroky.Repaint();
end;

procedure TAC.Stop();
begin
 Self.frunning  := false;
 Self.krok      := -1;
 Self.repeating := false;
 writelog('AC '+Self.name+ ' : STOP', WR_AUTREZ);
 Inc(Self.stat_end);
 Self.changed := true;
 F_Main.LV_AC_Kroky.Repaint();
 F_Main.UpdateACButtons();
end;

procedure TAC.Pause();
begin
 if (not Self.running) then Exit();
 Self.frunning := false;
 Self.changed := true;
 writelog('AC '+Self.name+ ' : PAUSE', WR_AUTREZ);
end;

function TAC.GetPaused():boolean;
begin
 Result := ((not Self.running) and (Self.krok > -1));
end;

////////////////////////////////////////////////////////////////////////////////
// aktualizace prubehu AC:

procedure TAC.UpdateKrok();
var cmd:integer;
    params:TKrkParams;
    Blk:TBlk;
    JC:TJC;
    OblR:TOR;
    panel:TIDContext;
 begin

    //seznam validnich instrukci:
         //command:
         //0 = konec
         //1 = 'jc' [index]                 stavi a ceka na postaveni cesty index
         //2 = 'usek' [index,stav]          ceka na stav useku index
         //3 = 'osv' [stanice,index,stav]   zmeni osvetleni index v dane stanici na stav
         //4 = 'trat' [index,smer]          nastavi danou trat do daneho smeru
         //5 = 'delay' [m,s,ms]             ceka cas m,s,ms
         //6 = 'nav' [jc,navest]           ceka na navest na navestidle jizdni cesty jc

  if (Self.krok < 0) then Self.krok := 0;   // to be sure

  if (Self.krok = Self.kroky.Count) then
   begin
    if (Self.repeating) then
     begin
      // opakovani
      Self.wait := Now + Self.opak_time;
      Self.NextStep();
      Inc(Self.stat_end);
      writelog('AC '+Self.name + ' : cekam na opakovani...', WR_AUTREZ);
     end else
      Self.Stop();

    Self.changed := true;
    Exit;
   end;

  if (Self.krok = (Self.kroky.Count+1)) then
   begin
    if ((Self.wait < Now) and (Self.ready)) then
     begin
      Self.krok := 0;
      Self.changed := true;
      writelog('AC '+Self.name + ' : opakuji', WR_AUTREZ);
     end;
    Exit();
   end;

  cmd     := Self.kroky[Self.Krok].command;
  params  := Self.kroky[Self.Krok].Params;

  case (cmd) of
    0: begin
      Self.Stop();
    end;

    _AC_CMDTYPE_JC: begin
      if (Self.subkrok = 0) then
       begin
        JC := JCDb.GetJCByID(params[0]);
        writelog('AC '+Self.name+': krok : '+IntToStr(Self.Krok)+' == Staveni JC '+JC.Nazev, WR_AUTREZ);

        if (not JC.postaveno) then
         begin
          Blky.GetBlkByID(JC.data.NavestidloBlok, Blk);
          if ((Blk as TBlkNav).OblsRizeni.Count > 0) then
           OblR := (Blk as TBlkNav).OblsRizeni[0]
          else
           OblR := nil;

          if (OblR.Connected.Count > 0) then
           panel := OblR.Connected[0].Panel
          else
           panel := nil;

          // stavit JC
          if (OblR.stack.volba = TORStackVolba.VZ) then
            OblR.stack.AddJC(JC, panel, false)
          else
            JC.StavJC(panel, OblR);

         end;

        Self.subkrok := 1;
       end;

      if (Self.subkrok = 1) then
        if (JCDb.GetJCByID(params[0]).Postaveno) then Self.NextStep();
     end;//if _AC_CMDTYPE_JC

  ///////////////////////////////////////

    _AC_CMDTYPE_USEK: begin
      Blky.GetBlkByID(params[0], Blk);

      if (Self.subkrok = 0) then
       begin
        writelog('AC '+Self.name+': krok : '+IntToStr(Self.Krok)+' == Cekani na '+Blk.name,WR_AUTREZ);
        Self.subkrok := 1;
       end;

      if (Self.subkrok = 1) then
        if (Integer((Blk as TBlkUsek).Obsazeno) = params[1]) then Self.NextStep();
     end;

  ///////////////////////////////////////

    _AC_CMDTYPE_OSV: begin
//      Self.state := 'zmena osvetleni';
//      writelog('Krok : '+IntToStr(Self.Krok)+' == Stav : zmena osvetleni stanice '+ORs.GetORNameByIndex(params[0])+';index:'+IntToStr(params[1])+';new:'+IntToStr(params[2]), WR_AUTREZ);

//    StaniceFce.SetOsv(params.Data[0], params.Data[1], PrevodySoustav.IntToBool(params.Data[2]));

      Self.NextStep();
     end;

  ///////////////////////////////////////

    _AC_CMDTYPE_TRAT: begin
      Blky.GetBlkByID(params[0], Blk);

      if (Self.subkrok = 0) then
       begin
        writelog('AC '+Self.name+': krok : '+IntToStr(Self.Krok)+' == Zmena smeru trati '+Blk.name,WR_AUTREZ);
        Self.subkrok := 1;
       end;

      if (Self.subkrok = 1) and ((not (Blk as TBlkTrat).Obsazeno) and (not (Blk as TBlkTrat).Zaver) and (not (Blk as TBlkTrat).nouzZaver)) then
       begin
        (Blk as TBlkTrat).Smer := TTratSmer(params[1]);
        Self.NextStep();
       end;
     end;

  ///////////////////////////////////////

    _AC_CMDTYPE_DELAY: begin

      if (Self.subkrok = 0) then
       begin
        Self.wait := Now + EncodeTime(0, params[0] div 60, params[0] mod 60, 0);
        writelog('AC '+Self.name+': krok : '+IntToStr(Self.Krok)+' == : Cekani ('+FormatDateTime('nn:ss',Self.wait - Now)+')', WR_AUTREZ);
        Self.subkrok := 1;
       end;

      if (Self.subkrok = 1) then
        if (Now >= Self.wait) then
          Self.NextStep();
     end;

  ///////////////////////////////////////

    _AC_CMDTYPE_NAV: begin
      Blky.GetBlkByID(JCDb.GetJCByID(params[0]).data.NavestidloBlok , Blk);

      if (Self.subkrok = 0) then
       begin
        writelog('AC '+Self.name+': krok : '+IntToStr(Self.Krok)+' == Kontrola navesti JC '+JCDb.GetJCByID(params[0]).Nazev, WR_AUTREZ);
        Self.subkrok := 1;
       end;

      if (Self.subkrok = 1) then
       begin
        if (params[1] = 0) then
         begin
          //0
          if (((Blk as TBlkNav).Navest = 0) or ((Blk as TBlkNav).Navest = 13)) then
            Self.NextStep();
         end else begin
          //1
          if (((Blk as TBlkNav).Navest <> 0) and ((Blk as TBlkNav).Navest <> 13)) then
            Self.NextStep();
         end;
       end;//if AutRezimy[AC].Podkrok
     end;//'nav'
  else//case
   Inc(Self.krok);
  end;
 end;

////////////////////////////////////////////////////////////////////////////////

procedure TAC.NextStep();
begin
 Inc(Self.krok);
 Self.subkrok := 0;
 F_Main.LV_AC_Kroky.Repaint();
end;

////////////////////////////////////////////////////////////////////////////////

end.//unit

