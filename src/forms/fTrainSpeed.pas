unit fTrainSpeed;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.Menus, TrainSpeed, Generics.Collections,
  StdCtrls;

type
  EInvalidData = class(Exception);

  TF_TrainSpeed = class(TForm)
    LV_Speeds: TListView;
    PM_Speeds: TPopupMenu;
    MI_NewRecord: TMenuItem;
    MI_Delete: TMenuItem;
    N1: TMenuItem;
    MI_Help: TMenuItem;
    procedure MI_NewRecordClick(Sender: TObject);
    procedure PM_SpeedsPopup(Sender: TObject);
    procedure MI_DeleteClick(Sender: TObject);
    procedure LV_SpeedsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LV_SpeedsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LV_SpeedsDblClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LV_SpeedsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MI_HelpClick(Sender: TObject);
  private
    E_LV: TEdit;
    editRow: TListItem;
    editCol: Integer;

    procedure E_LVExit(Sender: TObject);
    procedure E_LVKeyPress(Sender: TObject; var Key: Char);
    procedure ELVAccept();
    procedure ELVHide();

  public

    procedure Fill(tss: TList<TTrainSpeed>);
    procedure Default();
    procedure Clear();
    function Get(): TList<TTrainSpeed>;
  end;

var
  F_TrainSpeed: TF_TrainSpeed;

implementation

uses CommCtrl;

{$R *.dfm}

procedure TF_TrainSpeed.MI_DeleteClick(Sender: TObject);
begin
  if (Self.LV_Speeds.Selected <> nil) then
    Self.LV_Speeds.DeleteSelected();
end;

procedure TF_TrainSpeed.MI_HelpClick(Sender: TObject);
begin
  Application.MessageBox('Ka�d� ��dek obsahuje rychlost v km/h a krit�ria zad�van� jako regul�rn� v�razy.'+#13#10+
    '��dky se zpracov�vaj� postupn� odshora dol�. Pokud souprava vyhov� krit�ri�m v ��dku, je j� nastavena rychlost v onom ��dku.'+#13#10+
    'Krit�ria jsou tvo�ena typem vlaku a t��dou p�echodnosti hnac�ch vozidel. V�echna hnac� vozidla mus� vyhov�t regul�rn�mu v�razu t��dy p�echodnosti.'+#13#10+
    'P��klad tabulky:'+#13#10+
    '90 Sc|Ec|Ic|Ex|R .*'+#13#10+
    '60 Pn|Vn .*'+#13#10+
    '70 .* .*'+#13#10+
    'Tento p��klad zajist�, �e soupravy typu Sc, Ec, Ic, Ec a R pojedou rychlosti 90 km/h, soupravy typu Pn a Vn 60 km/h a v�echny ostatn� soupravy rychlost� 70 km/h.'+#13#10+
    'V�dy je t�eba uv�st posledn� ��dek, kter�mu vyhov� v�echny soupravy.'+#13#10+
    '�prava ��dku se prov�d� dvojklikem.'+#13#10+
    'Pokud je rychlostn� tabulka v j�zdn� cest� pro dal�� n�v�st povoluj�c� j�zdu pr�zdn�, p�eb�raj� se rychlosti pro dal�� n�v�st nepovoluj�c� j�zdu.'+#13#10+
    'Pokud je rychlostn� tabulka v trati pro sud� sm�r pr�zdn�, p�eb�raj� se rychlosti pro lich� sm�r.',
    'N�pov�da', MB_ICONINFORMATION OR MB_OK);
end;

procedure TF_TrainSpeed.MI_NewRecordClick(Sender: TObject);
begin
  var LI: TListItem;
  if (Self.LV_Speeds.Selected <> nil) then
    LI := Self.LV_Speeds.Items.Insert(Self.LV_Speeds.ItemIndex)
  else
    LI := Self.LV_Speeds.Items.Add();

  LI.Caption := '0';
  LI.SubItems.Add('.*');
  LI.SubItems.Add('.*');
end;

procedure TF_TrainSpeed.PM_SpeedsPopup(Sender: TObject);
begin
  Self.MI_Delete.Enabled := (Self.LV_Speeds.Selected <> nil);
end;

procedure TF_TrainSpeed.Fill(tss: TList<TTrainSpeed>);
begin
  Self.ELVHide();
  Self.LV_Speeds.Clear();
  for var ts: TTrainSpeed in tss do
  begin
    var LI: TListItem := Self.LV_Speeds.Items.Add();
    LI.Caption := IntToStr(ts.speed);
    LI.SubItems.Add(ts.trainTypeRe);
    LI.SubItems.Add(ts.hvTransienceRe);
  end;
end;

procedure TF_TrainSpeed.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.editRow := nil;
end;

procedure TF_TrainSpeed.FormCreate(Sender: TObject);
begin
  Self.editRow := nil;

  Self.E_LV := TEdit.Create(nil);
  Self.E_LV.Parent := Self.LV_Speeds;
  Self.E_LV.OnExit := Self.E_LVExit;
  Self.E_LV.OnKeyPress := Self.E_LVKeyPress;
  Self.E_LV.Visible := false;
end;

procedure TF_TrainSpeed.FormDestroy(Sender: TObject);
begin
  Self.E_LV.Free();
end;

procedure TF_TrainSpeed.FormShow(Sender: TObject);
begin
  Self.editRow := nil;
  Self.E_LV.Visible := false;
end;

procedure TF_TrainSpeed.E_LVExit(Sender: TObject);
begin
  Self.ELVAccept();
end;

procedure TF_TrainSpeed.E_LVKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #10) then // enter
    Self.ELVAccept();
end;

procedure TF_TrainSpeed.ELVAccept();
begin
  if (Assigned(editRow)) then
  begin
    if (Self.editCol = 0) then
      editRow.Caption := Self.E_LV.Text
    else
      editRow.SubItems[editCol-1] := Self.E_LV.Text;
    Self.ELVHide();
  end;
end;

procedure TF_TrainSpeed.ELVHide();
begin
  Self.E_LV.Visible := false;
  editRow := nil;
end;

procedure TF_TrainSpeed.LV_SpeedsDblClick(Sender: TObject);
var LPoint: TPoint;
    LVHitTestInfo: TLVHitTestInfo;
    LRect: TRect;
begin
  LPoint := Self.LV_Speeds.ScreenToClient(Mouse.CursorPos);
  LVHitTestInfo.pt := LPoint;

  if (Self.LV_Speeds.Perform(LVM_SUBITEMHITTEST, 0, LPARAM(@LVHitTestInfo)) <> -1) then
  begin
    Self.editRow := Self.LV_Speeds.Items[LVHitTestInfo.iItem];
    Self.editCol := LVHitTestInfo.iSubItem;

    LRect.Top := Self.editCol; // subitem index
    LRect.Left := LVIR_BOUNDS;
    Self.LV_Speeds.Perform(LVM_GETSUBITEMRECT, LVHitTestInfo.iItem, LPARAM(@LRect));
    MapWindowPoints(Self.LV_Speeds.Handle, Self.LV_Speeds.Handle, LRect, 2);

    if (Self.editCol = 0) then
      Self.E_LV.Text := Self.editRow.Caption
    else
      Self.E_LV.Text := Self.editRow.Subitems[Self.editCol-1];

    Self.E_LV.BoundsRect := LRect;
    Self.E_LV.Visible := true;
    Self.E_LV.SetFocus();
  end;
end;

function TF_TrainSpeed.Get(): TList<TTrainSpeed>;
begin
  Result := TList<TTrainSpeed>.Create();
  try
    for var LI: TListItem in Self.LV_Speeds.Items do
    begin
      if ((LI.SubItems[0].Contains('{')) or LI.SubItems[0].Contains('{')) then
        raise EInvalidData.Create('trainTypeRe obsahuje neplatn� znak: "{" nebo "}"');
      if ((LI.SubItems[1].Contains('{')) or LI.SubItems[1].Contains('{')) then
        raise EInvalidData.Create('hvTransienceRe obsahuje neplatn� znak: "{" nebo "}"');
      Result.Add(TTrainSpeed.Create(StrToInt(LI.Caption), LI.SubItems[0], LI.SubItems[1]));
    end;
  except
    for var item in Result do
      item.Free();
    Result.Free();
    Result := nil;
  end;
end;

procedure TF_TrainSpeed.LV_SpeedsDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  if (Source = Self.LV_Speeds) then
  begin
    var myItem: TListItem;
    if (Self.LV_Speeds.GetItemAt(X,Y) <> nil) then
      myItem := Self.LV_Speeds.Items.Insert(Self.LV_Speeds.GetItemAt(X,Y).Index)
    else
      myItem := Self.LV_Speeds.Items.Add();

    myItem.Assign(Self.LV_Speeds.Selected);
    Self.LV_Speeds.Selected.Delete();
  end;
end;

procedure TF_TrainSpeed.LV_SpeedsDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source = Self.LV_Speeds);
end;

procedure TF_TrainSpeed.LV_SpeedsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
    Self.MI_DeleteClick(Self);
end;

procedure TF_TrainSpeed.Default();
begin
  Self.ELVHide();
  Self.LV_Speeds.Clear();
  var LI: TListItem := Self.LV_Speeds.Items.Add();
  LI.Caption := '40';
  LI.SubItems.Add('.*');
  LI.SubItems.Add('.*');
end;

procedure TF_TrainSpeed.Clear();
begin
  Self.ELVHide();
  Self.LV_Speeds.Clear();
end;

end.
