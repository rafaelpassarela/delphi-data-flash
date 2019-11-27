unit uRpDataFlash.ProxyFactory.CmdSelector;

interface

{  if Assigned(TreeViewComandos.Selected) then
  begin
    tn := TreeViewComandos.Selected;

    BoolResult := tn.StateIndex in [cFlatChecked,cFlatRadioChecked];
    Memo1.Text := tn.Text + #13#10 + 'Selected: ' + BoolToStr(BoolResult, True);

  end;}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ImgList, StdCtrls, ExtCtrls, DB, IniFiles, System.ImageList;

type
  TInfoStorage = class
  private
    FDescription: string;
    FGroup: string;
    FName: string;
  public
    constructor Create(const AGroup, AName, ADescription : string);
    property Description : string read FDescription;
    property Group : string read FGroup;
    property Name : string read FName;
  end;

  TFormLRDataFlashProxyGenerator = class(TForm)
    TreeViewComandos: TTreeView;
    ImageListStatus: TImageList;
    PanelControle: TPanel;
    ButtonCancelar: TButton;
    ButtonOk: TButton;
    PanelMarcar: TPanel;
    ButtonMarcar: TButton;
    ButtonDesmarcar: TButton;
    ButtonExpandir: TButton;
    ButtonRecolher: TButton;
    MemoInfoCmd: TMemo;
    SplitterCmd: TSplitter;
    PanelConfig: TPanel;
    LabelProxyName: TLabel;
    EditProxyName: TEdit;
    LabelAvisoSave: TLabel;
    GroupBoxConfig: TGroupBox;
    LabelAvisoInterface: TLabel;
    LabelUnitConfig: TLabel;
    EditUnitConfig: TEdit;
    LabelClassName: TLabel;
    EditClassName: TEdit;
    CheckBoxUsarBase: TCheckBox;
    procedure TreeViewComandosClick(Sender: TObject);
    procedure TreeViewComandosKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewComandosCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure ButtonDesmarcarClick(Sender: TObject);
    procedure ButtonMarcarClick(Sender: TObject);
    procedure ButtonRecolherClick(Sender: TObject);
    procedure ButtonExpandirClick(Sender: TObject);
    procedure TreeViewComandosChange(Sender: TObject; Node: TTreeNode);
    procedure ButtonOkClick(Sender: TObject);
  private
    { Private declarations }
    FConfigFileName : string;
    procedure DoAddNode(const AGrupo, AComando, ADescricao : string);
    procedure DoSelectNode(const ANode : TTreeNode; var AList : TStrings);
    procedure ToggleAll(const AState : Integer);
    procedure ResetTreeView;
    procedure LoadConfig;
    procedure SaveConfig;
  public
    { Public declarations }
    procedure InitCommandList(const ADataSet : TDataSet; const AConfigFileName : string);
    procedure GetListaSelecionados(var AList : TStrings);
  end;

const
  //ImageList.StateIndex=0 has some bugs, so we add one dummy image to position 0
  cFlatUnCheck = 1;
  cFlatChecked = 2;
  cFlatRadioUnCheck = 3;
  cFlatRadioChecked = 4;

var
  FormLRDataFlashProxyGenerator: TFormLRDataFlashProxyGenerator;

implementation

{$R *.dfm}

procedure ToggleTreeViewCheckBoxes(Node:TTreeNode; cUnChecked, cChecked, cRadioUnchecked, cRadioChecked:integer);
var
  tmp:TTreeNode;
  i: Integer;
begin
  if Assigned(Node) then
  begin
    if Node.StateIndex = cUnChecked then
      Node.StateIndex := cChecked
    else
      if Node.StateIndex = cChecked then
        Node.StateIndex := cUnChecked
      else
        if Node.StateIndex = cRadioUnChecked then
        begin
          tmp := Node.Parent;
          if not Assigned(tmp) then
            tmp := TTreeView(Node.TreeView).Items.getFirstNode
          else
            tmp := tmp.getFirstChild;
          while Assigned(tmp) do
          begin
            if (tmp.StateIndex in [cRadioUnChecked,cRadioChecked]) then
              tmp.StateIndex := cRadioUnChecked;
            tmp := tmp.getNextSibling;
          end;
          Node.StateIndex := cRadioChecked;
        end;
  end;

  // verifica se o node tem filhos
  for i := 0 to Node.Count - 1 do
    Node.Item[i].StateIndex := Node.StateIndex;

  // verifica o node pai
  if (Node.Parent <> nil) and (Node.StateIndex = cFlatChecked) and (Node.Parent.StateIndex <> cFlatChecked) then
    Node.Parent.StateIndex := cFlatChecked;
end; 

procedure TFormLRDataFlashProxyGenerator.ToggleAll(const AState: Integer);
var
  i: Integer;
begin
  for i := 0 to TreeViewComandos.Items.Count - 1 do
    TreeViewComandos.Items[i].StateIndex := AState;
end;

procedure TFormLRDataFlashProxyGenerator.TreeViewComandosChange(Sender: TObject;
  Node: TTreeNode);
begin
  if Assigned(Node) and Assigned(Node.Data) then
  begin
    MemoInfoCmd.Clear;
    MemoInfoCmd.Lines.Add('Grupo: ' + TInfoStorage(Node.Data).Group);
    MemoInfoCmd.Lines.Add('Comando: ' + TInfoStorage(Node.Data).Name);
    MemoInfoCmd.Lines.Add('');
    MemoInfoCmd.Lines.Add(TInfoStorage(Node.Data).Description);
  end
  else
    MemoInfoCmd.Clear;
end;

procedure TFormLRDataFlashProxyGenerator.TreeViewComandosClick(Sender: TObject);
var
  P:TPoint;
begin
  GetCursorPos(P);
  P := TreeViewComandos.ScreenToClient(P);

  if (htOnStateIcon in TreeViewComandos.GetHitTestInfoAt(P.X, P.Y)) then
    ToggleTreeViewCheckBoxes(TreeViewComandos.Selected, cFlatUnCheck, cFlatChecked, cFlatRadioUnCheck, cFlatRadioChecked);
end;

procedure TFormLRDataFlashProxyGenerator.TreeViewComandosKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(TreeViewComandos.Selected) then
    ToggleTreeViewCheckBoxes(TreeViewComandos.Selected, cFlatUnCheck, cFlatChecked, cFlatRadioUnCheck, cFlatRadioChecked);
end; 

procedure TFormLRDataFlashProxyGenerator.ButtonDesmarcarClick(Sender: TObject);
begin
  ToggleAll( cFlatUnCheck );
end;

procedure TFormLRDataFlashProxyGenerator.ButtonExpandirClick(Sender: TObject);
begin
  TreeViewComandos.FullExpand;
end;

procedure TFormLRDataFlashProxyGenerator.ButtonMarcarClick(Sender: TObject);
begin
  ToggleAll( cFlatChecked );
end;

procedure TFormLRDataFlashProxyGenerator.ButtonOkClick(Sender: TObject);
begin
  SaveConfig;
  ModalResult := mrOk;
end;

procedure TFormLRDataFlashProxyGenerator.ButtonRecolherClick(Sender: TObject);
begin
  TreeViewComandos.FullCollapse;
end;

procedure TFormLRDataFlashProxyGenerator.DoAddNode(const AGrupo, AComando, ADescricao: string);
var
  lRoot : TTreeNode;
  lCmdNode: TTreeNode;

  function FindRootNode : TTreeNode;
  var
    lCount: Integer;
  begin
    Result := nil;
    LCount := 0;
    while (lCount < TreeViewComandos.Items.Count) and (Result = nil) do
    begin
      if (TreeViewComandos.Items.Item[lCount].Text = AGrupo) and (TreeViewComandos.Items.Item[lCount].Parent = nil) then
        Result := TreeViewComandos.Items.Item[lCount];
      Inc(lCount);
    end;
  end;

begin
  lRoot := FindRootNode;
  if lRoot = nil then
  begin
    lRoot := TreeViewComandos.Items.Add(nil, AGrupo);
    lRoot.StateIndex := cFlatChecked;
  end;

  lCmdNode := TreeViewComandos.Items.AddChild(lRoot, AComando);
  lCmdNode.Data := TInfoStorage.Create(AGrupo, AComando, ADescricao);
  lCmdNode.StateIndex := cFlatChecked;
end;

procedure TFormLRDataFlashProxyGenerator.DoSelectNode(const ANode: TTreeNode;
  var AList: TStrings);
//var
//  i: Integer;
begin
  // se possui filhos
//  if ANode.Count > 0 then
//  begin
//    for i := 0 to ANode.Count - 1 do
//      DoSelectNode(ANode.Item[i], AList);
//  end
//  else
    if Assigned(ANode) and Assigned(ANode.Parent)
    and (ANode.Parent.StateIndex in [cFlatChecked, cFlatRadioChecked])
    and (ANode.StateIndex in [cFlatChecked, cFlatRadioChecked]) then
      AList.Add(ANode.Parent.Text + '|' + ANode.Text);
end;

procedure TFormLRDataFlashProxyGenerator.GetListaSelecionados(var AList: TStrings);
var
  i: Integer;
begin
  AList.Clear;
  for i := 0 to TreeViewComandos.Items.Count - 1 do
    DoSelectNode(TreeViewComandos.Items[i], AList);
end;

procedure TFormLRDataFlashProxyGenerator.InitCommandList(const ADataSet: TDataSet;
  const AConfigFileName : string);
begin
  ResetTreeView;

  if not ADataSet.Active then
    ADataSet.Open;

  ADataSet.DisableControls;
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    DoAddNode(ADataSet.FieldByName('GRUPO').AsString,
              ADataSet.FieldByName('COMANDO').AsString,
              ADataSet.FieldByName('DESCRICAO').AsString);
    ADataSet.Next;
  end;
  ADataSet.EnableControls;
  TreeViewComandos.FullExpand;

  TreeViewComandos.Select( TreeViewComandos.Items.GetFirstNode );

  FConfigFileName := AConfigFileName;
  LoadConfig;
end;

procedure TFormLRDataFlashProxyGenerator.LoadConfig;
var
  i: integer;
begin
  if FileExists(FConfigFileName) then
  begin
    with TIniFile.Create(FConfigFileName) do
    try
      EditProxyName.Text      := ReadString('config', 'proxy.filename', '');
      EditUnitConfig.Text     := ReadString('config', 'inft.filename', '');
      EditClassName.Text      := ReadString('config', 'intf.class', '');
      CheckBoxUsarBase.Checked := ReadBool( 'config', 'transport', False);
//      EditUnitBaseClass.Text  := ReadString('config', 'base.finelame', '');
//      EditBaseName.Text       := ReadString('config', 'base.class', '');

      for i := 0 to TreeViewComandos.Items.Count - 1 do
      begin
        if ReadBool('commands', TreeViewComandos.Items[i].Text, False) then
          TreeViewComandos.Items[i].StateIndex := cFlatChecked
        else
          TreeViewComandos.Items[i].StateIndex := cFlatUnCheck;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TFormLRDataFlashProxyGenerator.ResetTreeView;
var
  i: Integer;
begin
  for i := 0 to TreeViewComandos.Items.Count - 1 do
    if TreeViewComandos.Items[i].Data <> nil then
      TreeViewComandos.Items[i].Data := nil;

  TreeViewComandos.Items.Clear;
end;

procedure TFormLRDataFlashProxyGenerator.SaveConfig;
var
  i: Integer;
begin
  with TIniFile.Create(FConfigFileName) do
  try
    WriteString('config', 'proxy.filename', EditProxyName.Text);
    WriteString('config', 'inft.filename', EditUnitConfig.Text);
    WriteString('config', 'intf.class', EditClassName.Text);
    WriteBool(  'config', 'transport', CheckBoxUsarBase.Checked );
//    WriteString('config', 'base.finelame', EditUnitBaseClass.Text);
//    WriteString('config', 'base.class', EditBaseName.Text);

    for i := 0 to TreeViewComandos.Items.Count - 1 do
    begin
       WriteBool(
        'commands',
        TreeViewComandos.Items[i].Text,
        TreeViewComandos.Items[i].StateIndex = cFlatChecked);
    end;
  finally
    Free;
  end;
end;

procedure TFormLRDataFlashProxyGenerator.TreeViewComandosCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
//  AllowCollapse := False;
end;

{ TInfoStorage }

constructor TInfoStorage.Create(const AGroup, AName, ADescription: string);
begin
  FName := AName;
  FGroup := AGroup;
  FDescription := ADescription;
end;

end.
