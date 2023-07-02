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
    SE_Out_Plus_module: TSpinEdit;
    SE_Out_Minus_module: TSpinEdit;
    SE_In_Plus_module: TSpinEdit;
    SE_In_Minus_module: TSpinEdit;
    Label1: TLabel;
    GB_Lock: TGroupBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    openIndex: Integer;
    block: TBlkTurnout;
    isNewBlock: Boolean;
    CB_CouplingIds: TList<Integer>;
    CB_LockIds: TList<Integer>;
    CB_NeprofilIds: TList<Integer>;

    procedure CommonOpenForm();
    procedure EditOpenForm();
    procedure NewOpenForm();
  public

    procedure EditBlock(blockIndex: Integer);
    procedure NewBlock();
  end;

var
  F_BlkTurnout: TF_BlkTurnout;

implementation

uses GetSystems, TechnologieRCS, Block, DataBloky, Area, ifThenElse;

{$R *.dfm}

procedure TF_BlkTurnout.FormCreate(Sender: TObject);
begin
  Self.CB_CouplingIds := TList<Integer>.Create();
  Self.CB_LockIds := TList<Integer>.Create();
  Self.CB_NeprofilIds := TList<Integer>.Create();
end;

procedure TF_BlkTurnout.FormDestroy(Sender: TObject);
begin
  Self.CB_CouplingIds.Free();
  Self.CB_LockIds.Free();
  Self.CB_NeprofilIds.Free();
end;

procedure TF_BlkTurnout.EditBlock(blockIndex: Integer);
begin
  Self.isNewBlock := false;
  Self.openIndex := blockIndex;
  Self.block := Blocks.GetBlkByIndex(blockIndex) as TBlkTurnout;
  if (Self.block = nil) then
    raise Exception.Create('Blok #'+IntToStr(blockIndex)+' neexistuje!');

  Self.CommonOpenForm();
  Self.EditOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkTurnout.NewBlock();
begin
  Self.isNewBlock := true;
  Self.openIndex := -1;
  Self.block := nil;

  Self.CommonOpenForm();
  Self.NewOpenForm();
  Self.ShowModal();
end;

procedure TF_BlkTurnout.SE_Cont_Minus_ModuleExit(Sender: TObject);
begin
  Self.SE_Cont_Minus_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Cont_Minus_Module.Value, Self.SE_Cont_Minus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Cont_Plus_ModuleExit(Sender: TObject);
begin
  Self.SE_Cont_Plus_Port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_Cont_Plus_Module.Value, Self.SE_Cont_Plus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Ind_Minus_ModuleExit(Sender: TObject);
begin
  Self.SE_Ind_Minus_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_Ind_Minus_Module.Value, Self.SE_Ind_Minus_Port.Value);
end;

procedure TF_BlkTurnout.SE_Ind_Plus_ModuleExit(Sender: TObject);
begin
  Self.SE_Ind_Plus_Port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_Ind_Plus_Module.Value, Self.SE_Ind_Plus_Port.Value);
end;

procedure TF_BlkTurnout.SE_moduleExit(Sender: TObject);
begin
  Self.SE_Out_Plus_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_Out_Plus_module.Value, Self.SE_Out_Plus_port.Value);
  Self.SE_Out_Minus_port.MaxValue := TBlocks.SEOutPortMaxValue(Self.SE_Out_Minus_module.Value,
    Self.SE_Out_Minus_port.Value);
  Self.SE_In_Plus_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_In_Plus_module.Value, Self.SE_In_Plus_port.Value);
  Self.SE_In_Minus_port.MaxValue := TBlocks.SEInPortMaxValue(Self.SE_In_Minus_module.Value, Self.SE_In_Minus_port.Value);
end;

procedure TF_BlkTurnout.NewOpenForm();
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

procedure TF_BlkTurnout.EditOpenForm();
var glob: TBlkSettings;
begin
  glob := Self.block.GetGlobalSettings();

  Self.E_Name.Text := glob.name;
  Self.SE_ID.Value := glob.id;

  var settings := Self.block.GetSettings();

  Self.CHB_Coupling.Checked := (settings.coupling > -1);
  Self.CHB_CouplingClick(Self.CHB_Coupling);

  var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(settings.coupling);
  if (turnout <> nil) then
  begin
    var turnoutSettings := turnout.GetSettings();

    Self.CHB_Coupling_Common_In.Checked := (turnoutSettings.rcs.inp = settings.rcs.inp) and (turnoutSettings.rcs.inm = settings.rcs.inm);
    Self.CHB_Coupling_Common_Out.Checked := (turnoutSettings.rcs.outp = settings.rcs.outp) and (turnoutSettings.rcs.outm = settings.rcs.outm);
  end;

  Self.CHB_Lock.Checked := (settings.lock > -1);
  if (settings.lock > -1) then
    Self.CB_Lock_Pos.ItemIndex := Integer(settings.lockPosition);
  Self.CHB_LockClick(Self.CHB_Lock);


  if (Self.block.rcsInPlus.board > Cardinal(Self.SE_In_Plus_module.MaxValue)) then
    Self.SE_In_Plus_module.MaxValue := 0;
  Self.SE_In_Plus_port.MaxValue := 0;

  Self.SE_In_Plus_module.Value := Self.block.rcsInPlus.board;
  Self.SE_In_Plus_port.Value := Self.block.rcsInPlus.port;


  if (Self.block.rcsInMinus.board > Cardinal(Self.SE_In_Minus_module.MaxValue)) then
    Self.SE_In_Minus_module.MaxValue := 0;
  Self.SE_In_Minus_port.MaxValue := 0;

  Self.SE_In_Minus_module.Value := Self.block.rcsInMinus.board;
  Self.SE_In_Minus_port.Value := Self.block.rcsInMinus.port;


  if (Self.block.rcsOutPlus.board > Cardinal(Self.SE_Out_Plus_module.MaxValue)) then
    Self.SE_Out_Plus_module.MaxValue := 0;
  Self.SE_Out_Plus_port.MaxValue := 0;

  Self.SE_Out_Plus_module.Value := Self.block.rcsOutPlus.board;
  Self.SE_Out_Plus_port.Value := Self.block.rcsOutPlus.port;


  if (Self.block.rcsOutMinus.board > Cardinal(Self.SE_Out_Minus_module.MaxValue)) then
    Self.SE_Out_Minus_module.MaxValue := 0;
  Self.SE_Out_Minus_port.MaxValue := 0;

  Self.SE_Out_Minus_module.Value := Self.block.rcsOutMinus.board;
  Self.SE_Out_Minus_port.Value := Self.block.rcsOutMinus.port;


  Self.SE_moduleExit(Self);

  Self.CHB_Feedback.Checked := settings.posDetection;
  Self.CHB_FeedbackClick(Self.CHB_Feedback);

  Self.CHB_npPlus.Checked := (settings.npPlus > -1);
  Self.CHB_npPlusClick(Self.CHB_npPlus);

  Self.CHB_npMinus.Checked := (settings.npMinus > -1);
  Self.CHB_npMinusClick(Self.CHB_npMinus);

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
  Self.SE_Out_Plus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_Out_Minus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_In_Plus_module.MaxValue := RCSi.maxModuleAddrSafe;
  Self.SE_In_Minus_module.MaxValue := RCSi.maxModuleAddrSafe;

  if (Self.block <> nil) then
  begin
    var couplingIgnore: TList<Integer> := TList<Integer>.Create();
    try
      couplingIgnore.Add(Self.block.id);
    finally
      couplingIgnore.Free();
    end;

    // coupling
    Blocks.FillCB(Self.CB_Coupling, Self.CB_CouplingIds, couplingIgnore, Self.block.areas, btTurnout, btAny,
      Self.block.GetSettings().coupling);
    Self.CHB_Coupling.Enabled := (Self.CB_CouplingIds.Count > 0) or (Self.block.GetSettings.coupling > -1);

    // lock
    Blocks.FillCB(Self.CB_Lock, Self.CB_LockIds, nil, Self.block.areas, btLock, btAny, Self.block.GetSettings().lock);
    Self.CHB_Lock.Enabled := (Self.CB_LockIds.Count > 0) or (Self.block.GetSettings.lock > -1);

    // non-profile +
    Blocks.FillCB(Self.CB_npPlus, Self.CB_NeprofilIds, nil, Self.block.areas, btTrack, btRT, Self.block.GetSettings().npPlus);
    Self.CHB_npPlus.Enabled := (Self.CB_NeprofilIds.Count > 0) or (Self.block.GetSettings.npPlus > -1);

    // non-profile -
    Blocks.FillCB(Self.CB_npMinus, Self.CB_NeprofilIds, nil, Self.block.areas, btTrack, btRT, Self.block.GetSettings().npMinus);
    Self.CHB_npMinus.Enabled := (Self.CB_NeprofilIds.Count > 0) or (Self.block.GetSettings.npMinus > -1);

  end else begin
    Blocks.FillCB(Self.CB_Coupling, Self.CB_CouplingIds, nil, nil, btTurnout);
    Self.CHB_Coupling.Enabled := (Self.CB_CouplingIds.Count > 0);

    Blocks.FillCB(Self.CB_Lock, Self.CB_LockIds, nil, nil, btLock);
    Self.CHB_Lock.Enabled := (Self.CB_LockIds.Count > 0);

    Blocks.FillCB(Self.CB_npPlus, Self.CB_NeprofilIds, nil, nil, btTrack, btRT);
    Self.CHB_npPlus.Enabled := (Self.CB_NeprofilIds.Count > 0);

    Blocks.FillCB(Self.CB_npMinus, Self.CB_NeprofilIds, nil, nil, btTrack, btRT);
    Self.CHB_npMinus.Enabled := (Self.CB_NeprofilIds.Count > 0);
  end;

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
begin
  if (Self.E_Name.Text = '') then
  begin
    Application.MessageBox('Vyplňte název bloku!', 'Nelze uložit data', MB_OK OR MB_ICONWARNING);
    Exit();
  end;
  if (Blocks.IsBlock(Self.SE_ID.Value, Self.openIndex)) then
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

  var glob: TBlkSettings;
  glob.name := Self.E_Name.Text;
  glob.id := Self.SE_ID.Value;
  glob.typ := btTurnout;

  if (Self.isNewBlock) then
  begin
    try
      Self.block := Blocks.Add(glob) as TBlkTurnout;
    except
      on E: Exception do
      begin
        Application.MessageBox(PChar('Nepodařilo se přidat blok:' + #13#10 + E.Message), 'Nelze uložit data',
          MB_OK OR MB_ICONWARNING);
        Exit();
      end;
    end;
  end else begin
    Self.block.SetGlobalSettings(glob);
  end;

  var settings: TBlkTurnoutSettings;
  settings.posDetection := Self.CHB_Feedback.Checked;

  settings.rcs.outp := TRCS.RCSAddr(SE_Out_Plus_module.Value, SE_Out_Plus_port.Value);
  settings.rcs.outm := TRCS.RCSAddr(SE_Out_Minus_module.Value, SE_Out_Minus_port.Value);

  if (Self.CHB_Feedback.Checked) then
  begin
    settings.rcs.inp := TRCS.RCSAddr(SE_In_Plus_module.Value, SE_In_Plus_port.Value);
    settings.rcs.inm := TRCS.RCSAddr(SE_In_Minus_module.Value, SE_In_Minus_port.Value);
  end else begin
    settings.rcs.inp := RCSi.RCSAddr(0, 0);
    settings.rcs.inm := RCSi.RCSAddr(0, 0);
  end;


  var messages := '';
  var another: TBlk;
  another := Blocks.AnotherBlockUsesRCS(settings.rcs.outp, Self.block, TRCSIOType.output);
  if (another <> nil) then
    messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.rcs.outp.ToString() + '.' + #13#10;
  another := Blocks.AnotherBlockUsesRCS(settings.rcs.outm, Self.block, TRCSIOType.output);
  if (another <> nil) then
    messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.rcs.outm.ToString() + '.' + #13#10;

  if (Self.CHB_Feedback.Checked) then
  begin
    another := Blocks.AnotherBlockUsesRCS(settings.rcs.inp, Self.block, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.rcs.inp.ToString() + '.' + #13#10;
    another := Blocks.AnotherBlockUsesRCS(settings.rcs.inm, Self.block, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.rcs.inm.ToString() + '.' + #13#10;
  end;

  if (Self.CHB_Coupling.Checked) then
  begin
    settings.coupling := Self.CB_CouplingIds[Self.CB_Coupling.ItemIndex];

    var turnout: TBlkTurnout := Blocks.GetBlkTurnoutByID(settings.coupling);
    if (turnout = nil) then
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

    if (Self.CHB_Coupling_Common_Out.Checked) then
    begin
      turnoutSettings.rcs.outp := settings.rcs.outp;
      turnoutSettings.rcs.outm := settings.rcs.outm;
    end;
    if (Self.CHB_Coupling_Common_In.Checked) then
    begin
      turnoutSettings.rcs.inp := settings.rcs.inp;
      turnoutSettings.rcs.inm := settings.rcs.inm;
    end;

    turnout.SetSettings(turnoutSettings);
  end else begin
    settings.coupling := -1;
  end;

  if (Self.CHB_Lock.Checked) then
  begin
    settings.lock := Self.CB_LockIds[Self.CB_Lock.ItemIndex];
    settings.lockPosition := TTurnoutPosition(Self.CB_Lock_Pos.ItemIndex);
  end else begin
    settings.lock := -1;
    settings.lockPosition := TTurnoutPosition.none;
  end;

  if (Self.CHB_npPlus.Checked) then
    settings.npPlus := Self.CB_NeprofilIds[Self.CB_npPlus.ItemIndex]
  else
    settings.npPlus := -1;

  if (Self.CHB_npMinus.Checked) then
    settings.npMinus := Self.CB_NeprofilIds[Self.CB_npMinus.ItemIndex]
  else
    settings.npMinus := -1;

  settings.indication.enabled := Self.CHB_Indication.Checked;
  if (Self.CHB_Indication.Checked) then
  begin
    settings.indication.rcsPlus.board := Self.SE_Ind_Plus_Module.Value;
    settings.indication.rcsPlus.port := Self.SE_Ind_Plus_Port.Value;
    settings.indication.rcsMinus.board := Self.SE_Ind_Minus_Module.Value;
    settings.indication.rcsMinus.port := Self.SE_Ind_Minus_Port.Value;
    settings.indication.pstOnly := Self.CHB_Indication_Pst.Checked;

    another := Blocks.AnotherBlockUsesRCS(settings.indication.rcsPlus, Self.block, TRCSIOType.output);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.indication.rcsPlus.ToString() + '.' + #13#10;

    another := Blocks.AnotherBlockUsesRCS(settings.indication.rcsMinus, Self.block, TRCSIOType.output);
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

    another := Blocks.AnotherBlockUsesRCS(settings.controllers.rcsPlus, Self.block, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.controllers.rcsPlus.ToString() + '.' + #13#10;

    another := Blocks.AnotherBlockUsesRCS(settings.controllers.rcsMinus, Self.block, TRCSIOType.input);
    if (another <> nil) then
      messages := messages + 'Blok ' + another.name + ' využívá také RCS adresu ' + settings.controllers.rcsMinus.ToString() + '.' + #13#10;
  end;

  try
    Self.block.SetSettings(settings);
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
  Self.block.Change();
end;

procedure TF_BlkTurnout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.isNewBlock := false;
  Self.openIndex := -1;
  BlocksTablePainter.UpdateTable();
end;

end.
