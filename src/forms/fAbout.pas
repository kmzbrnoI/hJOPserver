unit fAbout;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, fMain, ShellAPI, pngimage, Vcl.ComCtrls;

type
  TF_About = class(TForm)
    ST_about1: TStaticText;
    ST_about2: TStaticText;
    ST_email: TStaticText;
    ST_about4: TStaticText;
    ST_kmz_web: TStaticText;
    B_Close: TButton;
    GB_Info: TGroupBox;
    Label1: TLabel;
    L_VApp: TLabel;
    I_AppIcon: TImage;
    ST_hJOP_web: TStaticText;
    GB_RCS: TGroupBox;
    LV_RCS: TListView;
    Label2: TLabel;
    L_Trakce_Lib: TLabel;
    Label3: TLabel;
    L_Trakce_APIv: TLabel;
    Label4: TLabel;
    L_BuildApp: TLabel;
    Label5: TLabel;
    L_Trakce_Version: TLabel;
    Label6: TLabel;
    L_UpdatedDateTime: TLabel;
    procedure FormShow(Sender: TObject);
    procedure B_CloseClick(Sender: TObject);
    procedure ST_linkClick(Sender: TObject);
    procedure ST_emailClick(Sender: TObject);
  private
    procedure RefreshRCSTable();

  public
    { Public declarations }
  end;

var
  F_About: TF_About;

implementation

uses version, RCSc, RCSsc, Logging, appEv, Trakcec;

{$R *.dfm}

procedure TF_About.FormShow(Sender: TObject);
begin
  Self.L_VApp.Caption := version.VersionStr();
  Self.L_BuildApp.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', BuildDateTime());

  Self.L_Trakce_Lib.Caption := trakce.Lib;
  if (trakce.libLoaded) then
    Self.L_Trakce_APIv.Caption := trakce.apiVersionStr
  else
    Self.L_Trakce_APIv.Caption := '-';

  try
    Self.L_Trakce_Version.Caption := trakce.LibVersion();
  except
    Self.L_Trakce_Version.Caption := '?';
  end;

  Self.RefreshRCSTable();

  Self.L_UpdatedDateTime.Caption := FormatDateTime('dd.mm.yyyy hh:nn:ss', Now);
end;

procedure TF_About.RefreshRCSTable();
begin
  Self.LV_RCS.Clear();
  for var rcsi: Integer := 0 to RCSs._RCSS_MAX do
  begin
    var rcs: TRCS := RCSs[rcsi];
    var li: TListItem := Self.LV_RCS.Items.Add;
    li.Caption := IntToStr(rcsi);
    li.SubItems.Add(rcs.lib);

    begin
      var str: string := '-';
      if (rcs.lib <> '') then
      begin
        try
          str := rcs.GetDllVersion();
        except
          on E: Exception do
          begin
            str := 'nelze získat';
            AppEvents.LogException(E, 'rcs'+IntToStr(rcsi)+'.GetDllVersion');
          end;
        end;
      end;
      li.SubItems.Add(str);
    end;

    begin
      var str: string := '-';
      try
        if (rcs.lib <> '') then
        begin
          if (rcs.Opened()) then
            str := rcs.GetDeviceVersion()
          else
            str := 'zařízení uzavřeno';
        end;
      except
        on E: Exception do
        begin
          str := 'nelze získat';
          AppEvents.LogException(E, 'rcs'+IntToStr(rcsi)+'.GetDeviceVersion');
        end;
      end;
      li.SubItems.Add(str);
    end;

    if (rcs.lib <> '') then
      li.SubItems.Add(rcs.apiVersionStr())
    else
      li.SubItems.Add('-');
  end;
end;

procedure TF_About.B_CloseClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_About.ST_linkClick(Sender: TObject);
begin
  var stSender: TStaticText := (Sender as TStaticText);
  Screen.Cursor := crAppStart;
  ShellExecute(0, nil, PChar(stSender.Caption), nil, nil, 0);
  Screen.Cursor := crDefault;
end;

procedure TF_About.ST_emailClick(Sender: TObject);
begin
  Screen.Cursor := crAppStart;
  ShellExecute(0, nil, PChar('mailto:' + Self.ST_email.Caption), nil, nil, 0);
  Screen.Cursor := crDefault;
end;

end.// unit
