unit fBlkTurnout;

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Spin, StdCtrls, ExtCtrls, fMain, BlockTurnout, BlockDb,
  Generics.Collections;

type
  TF_BlkTurnout = class(TForm)
    L_Vyh01: TLabel;
    E_Name: TEdit;
    SE_ID: TSpinEdit;
    L_Vyh02: TLabel;
    GB_RCS: TGroupBox;
    L_Vyh05: TLabel;
    L_Vyh06: TLabel;
    SE_Out_Plus_port: TSpinEdit;
    SE_Out_Minus_port: TSpinEdit;
    L_Vyh07: TLabel;
    L_Vyh08: TLabel;
    SE_In_Plus_port: TSpinEdit;
    SE_In_Minus_port: TSpinEdit;
    L_Vyh09: TLabel;
    B_Storno: TButton;
    B_Save: TButton;
    L_Usek03: TLabel;
    LB_Stanice: TListBox;
    SE_Out_Plus_module: TSpinEdit;
    SE_Out_Minus_module: TSpinEdit;
    SE_In_Plus_module: TSpinEdit;
    SE_In_Minus_module: TSpinEdit;
    Label1: TLabel;
    GB_Zamek: TGroupBox;
    CB_Lock: TComboBox;
    CHB_Lock: TCheckBox;
    Label2: TLabel;
    CB_Lock_Pos: TComboBox;
    GB_Neprofil: TGroupBox;
    CHB_npPlus: TCheckBox;
    CB_npPlus: TComboBox;
    CHB_npMinus: TCheckBox;
    CB_npMinus: TComboBox;
    GB_Coupling: TGroupBox;
    CHB_Coupling: TCheckBox;
    CB_Coupling: TComboBox;
    CHB_Coupling_Common_In: TCheckBox;
    CHB_Coupling_Common_Out: TCheckBox;
    CHB_Feedback: TCheckBox;
    GB_Indications: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    SE_Ind_Minus_Module: TSpinEdit;
    SE_Ind_Plus_Module: TSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    SE_Ind_Plus_Port: TSpinEdit;
    SE_Ind_Minus_Port: TSpinEdit;
    CHB_Indication: TCheckBox;
    CHB_Indication_Pst: TCheckBox;
    GB_Controllers: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    SE_Cont_Minus_Module: TSpinEdit;
    SE_Cont_Plus_Module: TSpinEdit;
    SE_Cont_Plus_Port: TSpinEdit;
    SE_Cont_Minus_Port: TSpinEdit;
    CHB_Controllers: TCheckBox;
    CHB_Controllers_Pst: TCheckBox;
    procedure B_StornoClick(Sender: TObject);
    procedure B_SaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CHB_CouplingClick(Sender: TObject);
    procedure CHB_LockClick(Sender: TObject);
    procedure CHB_npPlusClick(Sender: TObject);
    procedure CHB_npMinusClick(Sender: TObject);
    procedure SE_moduleExit(Sender: TObject);
    procedure CHB_FeedbackClick(Sender: TObject);
    procedure SE_Ind_Plus_ModuleExit(Sender: TObject);
    procedure SE_Ind_Minus_ModuleExit(Sender: TObject);
    procedure SE_Cont_Plus_ModuleExit(Sender: TObject);
    procedure SE_Cont_Minus_ModuleExit(Sender: TObject);
    procedure CHB_IndicationClick(Sender: TObject);
    procedure CHB_ControllersClick(Sender: TObject);
  private
    OpenIndex: Integer;
    Blk: TBlkTurnout;
    NewBlk: Boolean;
    CB_CouplingData: TArI;
    CB_LockData: TArI;
    CB_NeprofilData: TArI;

    procedure NewBlkOpenForm();
    procedure NormalOpenForm();
    procedure CommonOpenForm();
  public
    procedure OpenForm(BlokIndex: Integer);
    procedure NewBlkCreate;
  end;

var
  F_BlkTurnout: TF_BlkTurnout;

implementation

uses GetSystems, FileSystem, TechnologieRCS, Block, DataBloky, Area;

{$R *.dfm}

procedure TF_BlkTurnout.OpenForm(BlokIndex: Integer);
begin
  Blocks.GetBlkByIndex(BlokIndex, TBlk(Self.Blk));
  OpenIndex := BlokIndex;

  Self.CommonOpenForm();
  if (NewBlk) then
    Self.NewBlkOpenForm()
  else
    Self.NormalOpenForm();

  Self.ShowModal();
end;

procedure TF_BlkTurnout.SE_Cont_Minus_ModuleExit(Sender: TObject);
begin
  Self.SE_Cont_Minus_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Cont_Minus_Module.Value, Self.SE_Cont_Minus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Cont_Plus_ModuleExit(Sender: TObject);
begin
  Self.SE_Cont_Plus_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Cont_Plus_Module.Value, Self.SE_Cont_Plus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Ind_Minus_ModuleExit(Sender: TObject);
begin
  Self.SE_Ind_Minus_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Ind_Minus_Module.Value, Self.SE_Ind_Minus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Ind_Plus_ModuleExit(Sender: TObject);
begin
  Self.SE_Ind_Plus_Port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Ind_Plus_Module.Value, Self.SE_Ind_Plus_Port.Value);
end;

procedure TF_BlkTurnout.SE_moduleExit(Sender: TObject);
begin
  Self.SE_Out_Plus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Out_Plus_module.Value, Self.SE_Out_Plus_port.Value);
  Self.SE_Out_Minus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_Out_Minus_module.Value,
    Self.SE_Out_Minus_port.Value);
  Self.SE_In_Plus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_In_Plus_module.Value, Self.SE_In_Plus_port.Value);
  Self.SE_In_Minus_port.MaxValue := TBlocks.SEPortMaxValue(Self.SE_In_Minus_module.Value, Self.SE_In_Minus_port.Value);
end;

procedure TF_BlkTurnout.NewBlkOpenForm();
begin
  Self.E_Name.Text := '';
  Self.SE_ID.Value := Blocks.GetBlkID(Blocks.count - 1) + 1;

  Self.SE_Out_Plus_port.Value := 0;
  Self.SE_Out_Plus_module.Value := 1;
  Self.SE_Out_Minus_port.Value := 0;
  Self.SE_Out_Minus_module.Value := 1;
  Self.SE_In_Plus_port.Value := 0;
  Self.SE_In_Plus_module.Value := 1;
  Self.SE_In_Minus_port.Value := 0;
  Self.SE_In_Minus_module.Value := 1;
  Self.SE_moduleExit(Self);

  Self.CHB_Coupling.Checked := false;
  Self.CHB_Coupling_Common_In.Checked := false;
  Self.CHB_Coupling_Common_Out.Checked := false;
  Self.CHB_CouplingClick(Self.CHB_Coupling);

  Self.CHB_Lock.Checked := false;
  Self.CHB_LockClick(Self.CHB_Lock);

  Self.CHB_npPlus.Checked := false;
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := false;
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  Self.CHB_Feedback.Checked := true;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  Self.CHB_Indication.Checked := false;
  Self.CHB_IndicationClick(Self.CHB_Indication);

  Self.CHB_Controllers.Checked := false;
  Self.CHB_ControllersClick(Self.CHB_Controllers);

  Self.Caption := 'Nový blok Výhybka';
  Self.ActiveControl := Self.E_Name;
end;

procedure TF_BlkTurnout.NormalOpenForm();
var glob: TBlkSettings;
  turnoutSettings, settings: TBlkTurnoutSettings;
  turnout: TBlkTurnout;
begin
  glob := Self.Blk.GetGlobalSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  for var area in Self.Blk.areas do
    Self.LB_Stanice.Items.Add(area.name);

  settings := Blk.GetSettings();

  Self.CHB_Coupling.Checked := (settings.coupling > -1);
  Self.CHB_CouplingClick(Self.CHB_Coupling);

  Blocks.GetBlkByID(settings.coupling, TBlk(turnout));
  if ((turnout <> nil) and (turnout.typ = btTurnout)) then
  begin
    turnoutSettings := turnout.GetSettings();

    Self.CHB_Coupling_Common_In.Checked := (turnoutSettings.RCSAddrs.count >= 2) and (settings.RCSAddrs.count >= 2) and
      (turnoutSettings.RCSAddrs[0] = settings.RCSAddrs[0]) and (turnoutSettings.RCSAddrs[1] = settings.RCSAddrs[1]);
    Self.CHB_Coupling_Common_Out.Checked := (turnoutSettings.RCSAddrs.count >= 4) and (settings.RCSAddrs.count >= 4) and
      (turnoutSettings.RCSAddrs[2] = settings.RCSAddrs[2]) and (turnoutSettings.RCSAddrs[3] = settings.RCSAddrs[3]);
  end;

  Self.CHB_Lock.Checked := (settings.lock > -1);
  if (settings.lock > -1) then
    Self.CB_Lock_Pos.ItemIndex := Integer(settings.lockPosition);
  Self.CHB_LockClick(Self.CHB_Lock);

  if (settings.RCSAddrs.count > 0) then
  begin
    if (Self.Blk.rcsInPlus.board > Cardinal(Self.SE_In_Plus_module.MaxValue)) then
      Self.SE_In_Plus_module.MaxValue := 0;
    Self.SE_In_Plus_port.MaxValue := 0;

    Self.SE_In_Plus_module.Value := Self.Blk.rcsInPlus.board;
    Self.SE_In_Plus_port.Value := Self.Blk.rcsInPlus.port;
  end else begin
    Self.SE_In_Plus_module.Value := 0;
    Self.SE_In_Plus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 1) then
  begin
    if (Self.Blk.rcsInMinus.board > Cardinal(Self.SE_In_Minus_module.MaxValue)) then
      Self.SE_In_Minus_module.MaxValue := 0;
    Self.SE_In_Minus_port.MaxValue := 0;

    Self.SE_In_Minus_module.Value := Self.Blk.rcsInMinus.board;
    Self.SE_In_Minus_port.Value := Self.Blk.rcsInMinus.port;
  end else begin
    Self.SE_In_Minus_module.Value := 0;
    Self.SE_In_Minus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 2) then
  begin
    if (Self.Blk.rcsOutPlus.board > Cardinal(Self.SE_Out_Plus_module.MaxValue)) then
      Self.SE_Out_Plus_module.MaxValue := 0;
    Self.SE_Out_Plus_port.MaxValue := 0;

    Self.SE_Out_Plus_module.Value := Self.Blk.rcsOutPlus.board;
    Self.SE_Out_Plus_port.Value := Self.Blk.rcsOutPlus.port;
  end else begin
    Self.SE_Out_Plus_module.Value := 0;
    Self.SE_Out_Plus_port.Value := 0;
  end;

  if (settings.RCSAddrs.count > 3) then
  begin
    if (Self.Blk.rcsOutMinus.board > Cardinal(Self.SE_Out_Minus_module.MaxValue)) then
      Self.SE_Out_Minus_module.MaxValue := 0;
    Self.SE_Out_Minus_port.MaxValue := 0;

    Self.SE_Out_Minus_module.Value := Self.Blk.rcsOutMinus.board;
    Self.SE_Out_Minus_port.Value := Self.Blk.rcsOutMinus.port;
  end else begin
    Self.SE_Out_Minus_module.Value := 0;
    Self.SE_Out_Minus_port.Value := 0;
  end;

  Self.SE_moduleExit(Self);

  Self.CHB_Feedback.Checked := settings.posDetection;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  Self.CHB_npPlus.Checked := (settings.npPlus > -1);
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := (settings.npMinus > -1);
  Self.CHB_npMinusClick(Self.CHB_npMinus);

  begin
    var areas: TArStr;
    SetLength(areas, Self.Blk.areas.count);
    for var i := 0 to Self.Blk.areas.count - 1 do
      areas[i] := Self.Blk.areas[i].id;
  end;

  begin
    Self.CHB_Indication.Checked := settings.indication.enabled;
    Self.CHB_IndicationClick(Self.CHB_Indication);
    if (settings.indication.enabled) then
    begin
      if (settings.indication.rcsPlus.board > Cardinal(Self.SE_Ind_Plus_Module.MaxValue)) then
        Self.SE_Ind_Plus_Module.MaxValue := 0;
      Self.SE_Ind_Plus_Port.MaxValue := 0;

      Self.SE_Ind_Plus_Module.Value := settings.indication.rcsPlus.board;
      Self.SE_Ind_Plus_Port.Value := settings.indication.rcsPlus.port;

      if (settings.indication.rcsMinus.board > Cardinal(Self.SE_Ind_Minus_Module.MaxValue)) then
        Self.SE_Ind_Minus_Module.MaxValue := 0;
      Self.SE_Ind_Minus_Port.MaxValue := 0;

      Self.SE_Ind_Minus_Module.Value := settings.indication.rcsMinus.board;
      Self.SE_Ind_Minus_Port.Value := settings.indication.rcsMinus.port;
      Self.CHB_Indication_Pst.Checked := settings.indication.pstOnly;
    end;
  end;

  begin
    Self.CHB_Controllers.Checked := settings.controllers.enabled;
    Self.CHB_ControllersClick(Self.CHB_Controllers);
    if (settings.controllers.enabled) then
    begin
      if (settings.controllers.rcsPlus.board > Cardinal(Self.SE_Cont_Plus_Module.MaxValue)) then
        Self.SE_Cont_Plus_Module.MaxValue := 0;
      Self.SE_Cont_Plus_Port.MaxValue := 0;

      Self.SE_Cont_Plus_Module.Value := settings.controllers.rcsPlus.board;
      Self.SE_Cont_Plus_Port.Value := settings.controllers.rcsPlus.port;

      if (settings.controllers.rcsMinus.board > Cardinal(Self.SE_Cont_Minus_Module.MaxValue)) then
        Self.SE_Cont_Minus_Module.MaxValue := 0;
      Self.SE_Cont_Minus_Port.MaxValue := 0;

      Self.SE_Cont_Minus_Module.Value := settings.controllers.rcsMinus.board;
      Self.SE_Cont_Minus_Port.Value := settings.controllers.rcsMinus.port;
      Self.CHB_Controllers_Pst.Checked := settings.controllers.pstOnly;
    end;
  end;

  Self.Caption := 'Upravit blok ' + glob.name + ' (výhybka)';
  Self.ActiveControl := B_Save;
end;

procedure TF_BlkTurnout.CommonOpenForm();
begin
  Self.LB_Stanice.Clear();

  Self.SE_Out_Plus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Out_Minus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_In_Plus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_In_Minus_module.MaxValue := RCSi.maxModuleAddrSafe;

  if (Self.Blk <> nil) then
  begin
    var areas: TArStr;
    var Coupling_vypust: TArI;
    SetLength(areas, Self.Blk.areas.count);
    for var i := 0 to Self.Blk.areas.count - 1 do
      areas[i] := Self.Blk.areas[i].id;
    SetLength(Coupling_vypust, 1);
    Coupling_vypust[0] := Self.Blk.id;

    // Coupling
    Blocks.FillCB(Self.CB_Coupling, @Self.CB_CouplingData, @Coupling_vypust, areas, btTurnout,
      Self.Blk.GetSettings().coupling);
    Self.CHB_Coupling.Enabled := (Length(Self.CB_CouplingData) > 0) or (Self.Blk.GetSettings.coupling > -1);

    // Lock
    Blocks.FillCB(Self.CB_Lock, @Self.CB_LockData, nil, areas, btLock, Self.Blk.GetSettings().lock);
    Self.CHB_Lock.Enabled := (Length(Self.CB_LockData) > 0) or (Self.Blk.GetSettings.lock > -1);

    // neprofilove styky +
    Blocks.FillCB(Self.CB_npPlus, @Self.CB_NeprofilData, nil, areas, btTrack, Self.Blk.GetSettings().npPlus, btRT);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npPlus > -1);

    // neprofilove styky -
    Blocks.FillCB(Self.CB_npMinus, @Self.CB_NeprofilData, nil, areas, btTrack, Self.Blk.GetSettings().npMinus, btRT);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0) or (Self.Blk.GetSettings.npMinus > -1);

  end else begin
    Blocks.FillCB(Self.CB_Coupling, @Self.CB_CouplingData, nil, nil, btTurnout, -1);
    Self.CHB_Coupling.Enabled := (Length(Self.CB_CouplingData) > 0);

    Blocks.FillCB(Self.CB_Lock, @Self.CB_LockData, nil, nil, btLock, -1);
    Self.CHB_Lock.Enabled := (Length(Self.CB_LockData) > 0);

    Blocks.FillCB(Self.CB_npPlus, @Self.CB_NeprofilData, nil, nil, btTrack, -1, btRT);
    Self.CHB_npPlus.Enabled := (Length(Self.CB_NeprofilData) > 0);

    Blocks.FillCB(Self.CB_npMinus, @Self.CB_NeprofilData, nil, nil, btTrack, -1, btRT);
    Self.CHB_npMinus.Enabled := (Length(Self.CB_NeprofilData) > 0);
  end;

end;

procedure TF_BlkTurnout.NewBlkCreate;
begin
  Self.NewBlk := true;
  OpenForm(Blocks.count);
end;

procedure TF_BlkTurnout.B_StornoClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TF_BlkTurnout.CHB_ControllersClick(Sender: TObject);
begin
  Self.CHB_Controllers_Pst.Enabled := Self.CHB_Controllers.Checked;
  Self.SE_Cont_Plus_Module.Enabled := Self.CHB_Controllers.Checked;
  Self.SE_Cont_Plus_Port.Enabled := Self.CHB_Controllers.Checked;
  Self.SE_Cont_Minus_Module.Enabled := Self.CHB_Controllers.Checked;
  Self.SE_Cont_Minus_Port.Enabled := Self.CHB_Controllers.Checked;

  if (not Self.CHB_Controllers.Checked) then
  begin
    Self.CHB_Controllers_Pst.Checked := false;
    Self.SE_Cont_Plus_Module.Value := 0;
    Self.SE_Cont_Plus_Port.Value := 0;
    Self.SE_Cont_Minus_Module.Value := 0;
    Self.SE_Cont_Minus_Port.Value := 0;
  end;
end;

procedure TF_BlkTurnout.CHB_FeedbackClick(Sender: TObject);
begin
  Self.SE_In_Plus_module.Enabled := Self.CHB_Feedback.Checked;
  Self.SE_In_Plus_port.Enabled := Self.CHB_Feedback.Checked;
  Self.SE_In_Minus_module.Enabled := Self.CHB_Feedback.Checked;
  Self.SE_In_Minus_port.Enabled := Self.CHB_Feedback.Checked;

  if (not Self.CHB_Feedback.Checked) then
  begin
    Self.SE_In_Plus_module.Value := 0;
    Self.SE_In_Plus_port.Value := 0;
    Self.SE_In_Minus_module.Value := 0;
    Self.SE_In_Minus_port.Value := 0;
  end;
end;

procedure TF_BlkTurnout.CHB_IndicationClick(Sender: TObject);
begin
  Self.CHB_Indication_Pst.Enabled := Self.CHB_Indication.Checked;
  Self.SE_Ind_Plus_Module.Enabled := Self.CHB_Indication.Checked;
  Self.SE_Ind_Plus_Port.Enabled := Self.CHB_Indication.Checked;
  Self.SE_Ind_Minus_Module.Enabled := Self.CHB_Indication.Checked;
  Self.SE_Ind_Minus_Port.Enabled := Self.CHB_Indication.Checked;

  if (not Self.CHB_Indication.Checked) then
  begin
    Self.CHB_Indication_Pst.Checked := false;
    Self.SE_Ind_Plus_Module.Value := 0;
    Self.SE_Ind_Plus_Port.Value := 0;
    Self.SE_Ind_Minus_Module.Value := 0;
    Self.SE_Ind_Minus_Port.Value := 0;
  end;
end;

procedure TF_BlkTurnout.CHB_npMinusClick(Sender: TObject);
begin
  Self.CB_npMinus.Enabled := Self.CHB_npMinus.Checked;
  if (not Self.CHB_npMinus.Checked) then
    Self.CB_npMinus.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_npPlusClick(Sender: TObject);
begin
  Self.CB_npPlus.Enabled := Self.CHB_npPlus.Checked;
  if (not Self.CHB_npPlus.Checked) then
    Self.CB_npPlus.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_CouplingClick(Sender: TObject);
begin
  Self.CB_Coupling.Enabled := Self.CHB_Coupling.Checked;
  Self.CHB_Coupling_Common_In.Enabled := Self.CHB_Coupling.Checked;
  Self.CHB_Coupling_Common_Out.Enabled := Self.CHB_Coupling.Checked;
  Self.CHB_Coupling_Common_In.Checked := Self.CHB_Coupling.Checked;
  Self.CHB_Coupling_Common_Out.Checked := Self.CHB_Coupling.Checked;

  if (not Self.CHB_Coupling.Checked) then
    Self.CB_Coupling.ItemIndex := -1;
end;

procedure TF_BlkTurnout.CHB_LockClick(Sender: TObject);
begin
  Self.CB_Lock.Enabled := (Sender as TCheckBox).Checked;
  Self.CB_Lock_Pos.Enabled := (Sender as TCheckBox).Checked;
  if (not(Sender as TCheckBox).Checked) then
  begin
    Self.CB_Lock.ItemIndex := -1;
    Self.CB_Lock_Pos.ItemIndex := -1;
  end;
end;

procedure TF_BlkTurnout.B_SaveClick(Sender: TObject);
var glob: TBlkSettings;
  settings: TBlkTurnoutSettings;
begin
  if (E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlok(SE_ID.Value, OpenIndex)) then
  begin
    Application.MessageBox('ID již bylo definováno na jiním bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_Coupling.Checked) and (Self.CB_Coupling.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte spojku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Self.CHB_Lock.Checked) then
  begin
    if (Self.CB_Lock.ItemIndex < 0) then
    begin
      Application.MessageBox('Vyberte zámek!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
    if (Self.CB_Lock_Pos.ItemIndex < 0) then
    begin
      Application.MessageBox('Vyberte polohu výhybky pro uzamčení zámku!', 'Nelze uložit data',
        MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;
  if ((Self.CHB_npPlus.Checked) and (Self.CB_npPlus.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu plus!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if ((Self.CHB_npMinus.Checked) and (Self.CB_npMinus.ItemIndex < 0)) then
  begin
    Application.MessageBox('Vyberte hlídaný blok neprofilového styku pro polohu mínus!', 'Nelze uložit data',
      MB_OK OR MB_ICONWARNING);
    Exit();
  end;

  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btTurnout;

  if (NewBlk) then
  begin
    try
      Blk := Blocks.Add(glob) as TBlkTurnout;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    glob.note := Self.Blk.note;
    Self.Blk.SetGlobalSettings(glob);
  end;

  settings.RCSAddrs := TList<TechnologieRCS.TRCSAddr>.Create();
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_In_Plus_module.Value, SE_In_Plus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_In_Minus_module.Value, SE_In_Minus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_Out_Plus_module.Value, SE_Out_Plus_port.Value));
  settings.RCSAddrs.Add(TRCS.RCSAddr(SE_Out_Minus_module.Value, SE_Out_Minus_port.Value));

  var messages := '';
  for var i := 0 to settings.RCSAddrs.count - 1 do
  begin
    var typ: TRCSIOType;
    if (i < 2) then
      typ := TRCSIOType.input
    else
      typ := TRCSIOType.output;

    var another := Blocks.AnotherBlockUsesRCS(settings.RCSAddrs[i], Self.Blk, typ);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.RCSAddrs[i].ToString() + '.' + #13#10;
  end;

  if (Self.CHB_Coupling.Checked) then
  begin
    settings.coupling := Blocks.GetBlkID(Self.CB_CouplingData[Self.CB_Coupling.ItemIndex]);

    var turnout: TBlkTurnout;
    Blocks.GetBlkByID(settings.coupling, TBlk(turnout));
    if ((Blk = nil) or (Blk.typ <> btTurnout)) then
    begin
      Application.MessageBox('Blok spojky neexistuje nebo není výhybka', 'Chyba', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    var turnoutSettings := turnout.GetSettings();

    if ((turnoutSettings.coupling <> -1) and (turnoutSettings.coupling <> glob.id)) then
    begin
      Application.MessageBox('Na spojkové výhybce je již jiná Coupling!', 'Varování', MB_OK OR MB_ICONWARNING);
      Exit();
    end;

    while (turnoutSettings.RCSAddrs.count < 4) do
      turnoutSettings.RCSAddrs.Add(TRCS.RCSAddr(0, 0));

    if (Self.CHB_Coupling_Common_Out.Checked) then
    begin
      turnoutSettings.RCSAddrs[2] := settings.RCSAddrs[2];
      turnoutSettings.RCSAddrs[3] := settings.RCSAddrs[3];
    end;
    if (Self.CHB_Coupling_Common_In.Checked) then
    begin
      turnoutSettings.RCSAddrs[0] := settings.RCSAddrs[0];
      turnoutSettings.RCSAddrs[1] := settings.RCSAddrs[1];
    end;

    turnout.SetSettings(turnoutSettings);
  end else begin
    settings.coupling := -1;
  end;

  if (Self.CHB_Lock.Checked) then
  begin
    settings.lock := Blocks.GetBlkID(Self.CB_LockData[Self.CB_Lock.ItemIndex]);
    settings.lockPosition := TTurnoutPosition(Self.CB_Lock_Pos.ItemIndex);
  end else begin
    settings.lock := -1;
    settings.lockPosition := TTurnoutPosition.none;
  end;

  if (Self.CHB_npPlus.Checked) then
    settings.npPlus := Blocks.GetBlkID(Self.CB_NeprofilData[Self.CB_npPlus.ItemIndex])
  else
    settings.npPlus := -1;

  if (Self.CHB_npMinus.Checked) then
    settings.npMinus := Blocks.GetBlkID(Self.CB_NeprofilData[Self.CB_npMinus.ItemIndex])
  else
    settings.npMinus := -1;

  settings.posDetection := Self.CHB_Feedback.Checked;

  settings.indication.enabled := Self.CHB_Indication.Checked;
  if (Self.CHB_Indication.Checked) then
  begin
    settings.indication.rcsPlus.board := Self.SE_Ind_Plus_Module.Value;
    settings.indication.rcsPlus.port := Self.SE_Ind_Plus_Port.Value;
    settings.indication.rcsMinus.board := Self.SE_Ind_Minus_Module.Value;
    settings.indication.rcsMinus.port := Self.SE_Ind_Minus_Port.Value;
    settings.indication.pstOnly := Self.CHB_Indication_Pst.Checked;

    var another := Blocks.AnotherBlockUsesRCS(settings.indication.rcsPlus, Self.Blk, TRCSIOType.output);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.indication.rcsPlus.ToString() + '.' + #13#10;

    another := Blocks.AnotherBlockUsesRCS(settings.indication.rcsMinus, Self.Blk, TRCSIOType.output);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.indication.rcsMinus.ToString() + '.' + #13#10;
  end;

  settings.controllers.enabled := Self.CHB_Controllers.Checked;
  if (Self.CHB_Controllers.Checked) then
  begin
    settings.controllers.rcsPlus.board := Self.SE_Cont_Plus_Module.Value;
    settings.controllers.rcsPlus.port := Self.SE_Cont_Plus_Port.Value;
    settings.controllers.rcsMinus.board := Self.SE_Cont_Minus_Module.Value;
    settings.controllers.rcsMinus.port := Self.SE_Cont_Minus_Port.Value;
    settings.controllers.pstOnly := Self.CHB_Controllers_Pst.Checked;

    var another := Blocks.AnotherBlockUsesRCS(settings.controllers.rcsPlus, Self.Blk, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.controllers.rcsPlus.ToString() + '.' + #13#10;

    another := Blocks.AnotherBlockUsesRCS(settings.controllers.rcsMinus, Self.Blk, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.controllers.rcsMinus.ToString() + '.' + #13#10;
  end;

  try
    Self.Blk.SetSettings(settings);
  except
    on E: Exception do
    begin
      Application.MessageBox(PChar(E.Message), 'Nelze uložit', MB_OK OR MB_ICONWARNING);
      Exit();
    end;
  end;

  if (messages <> '') then
    Application.MessageBox(PChar(messages), 'Varování', MB_OK OR MB_ICONWARNING);

  Self.Close();
  Self.Blk.Change();
end;

procedure TF_BlkTurnout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.NewBlk := false;
  Self.OpenIndex := -1;
  BlokyTableData.UpdateTable();
end;

end.
