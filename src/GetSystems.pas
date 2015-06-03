unit GetSystems;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IdGlobal, IdHash, IdHashMessageDigest;

type
 TGetFunctions=class
    function GetQueryClose:boolean;                           // zjisti, zda je mozno zavrit program
    function GetWidthLabel(Nazev:string;Font:String):integer; // zjisti Width Labelu po zadani prislusneho textu na defaultnim fontu
    function GetSystemStart:Boolean;

 end;

var
 GetFunctions:TGetFunctions;

implementation

uses Main, RPConst,
     AdminForm, TechnologieMTB, Settings, TBLoky, TOblsRizeni, Logging,
     TCPServerOR;


function TGetFunctions.GetQueryClose:boolean;
 begin
  Result := true;
  if (NUZClose) then Exit;

  if (SystemData.Status <> TSystemStatus.null) then
   begin
    writelog('Pokus o zavøení okna pøi zapínání nebo vypínání systémù',WR_ERROR);
    Application.MessageBox(PChar('Technologie právì zapíná nebo vypíná systémy, aplikaci nelze momentálnì zavøít.'+#13#10+'Nouzové ukonèení programu lze provést spuštìním pøíkazu "app-exit" v konzoli')
            , 'Nelze ukonèit program', MB_OK OR MB_ICONWARNING);
    Exit(false);
   end;

  if (GetFunctions.GetSystemStart) then                                            //pokud je spustena komunikace
   begin
    writelog('Pokus o zavøení okna bez ukonèení komunikace se systémy',WR_ERROR);
    if (Application.MessageBox('Program není odpojen od systémù, odpojit od systémù?', 'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
     F_Main.A_System_StopExecute(Self);
    Exit(false);
   end;

  if (TrkSystem.openned) then
   begin
    writelog('Pokus o zavøení okna bez odpojení od centrály',WR_ERROR);
    if (Application.MessageBox('Program není odpojen od centrály, odpojit?', 'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
      TrkSystem.Close();
    Exit(false);
   end;
  if (ORTCPServer.openned) then
   begin
    writelog('Pokus o zavøení okna bez vypnutí panel serveru',WR_ERROR);
    if (Application.MessageBox('PanelServer stále bìží, vypnout?', 'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
     ORTCPServer.Stop();
    Exit(false);
   end;
  if (MTB.Openned) then
   begin
    writelog('Pokus o zavøení okna bez uzavøení MTB',WR_ERROR);
    if (Application.MessageBox('Program není odpojen od MTB, odpojit?', 'Nelze ukonèit program', MB_YESNO OR MB_ICONWARNING) = mrYes) then
     begin
      if (MTB.Start) then MTB.Stop()
      else if (MTB.Openned) then MTB.Close();           
     end;
    Exit(false);
   end;
 end;

function TGetFunctions.GetWidthLabel(Nazev:string;Font:String):integer;
var L_Test:TLabel;
 begin
  L_Test            := TLabel.Create(F_Main);
  L_Test.Caption    := Nazev;
  L_Test.AutoSize   := true;
  L_Test.Visible    := false;
  L_Test.Font.Name  := Font;
  Result            := L_Test.Width;
  L_Test.Free;
 end;//function

function TGetFunctions.GetSystemStart:Boolean;
 begin
  Result := (((TrkSystem.openned) and (ORTCPServer.openned) and (MTB.Start)) or (MTB.Start and F_Admin.CHB_SystemStart.Checked));
 end;//function

end.//uses
