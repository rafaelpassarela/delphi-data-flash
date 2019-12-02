unit uMainClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SHDocVw,
  Dialogs, StdCtrls, DB, DBClient, Grids, DBGrids, ExtCtrls, DBCtrls, OleCtrls,
  uRpDataFlash.ProxyGenerator, Gauges, uRpDataFlash.Types, uRpDataFlash.Command,
  uRpDataFlash.DataSet, uRpDataFlash.CommandExecutor, uRpDataFlash.Components,
  uRpDataFlash.CommandHelper, uRpDataFlash.GetCommandList,
  uRpDataFlash.CommandController;

type
  TFormMainClient = class(TForm)
    LabelNomeServer: TLabel;
    EditNomeServer: TEdit;
    LabelPorta: TLabel;
    EditPorta: TEdit;
    ButtonVerLog: TButton;
    ButtonConectar: TButton;
    ButtonDesconectar: TButton;
    MemoLog: TMemo;
    RpDataFlashClientConnectionTeste: TRpDataFlashClientConnection;
    ScrollBoxComandos: TScrollBox;
    ButtonSomarProxy: TButton;
    DBGrid1: TDBGrid;
    ButtonOpenDS: TButton;
    EditFiltro: TEdit;
    LabelFiltro: TLabel;
    DataSource1: TDataSource;
    ButtonCloseDS: TButton;
    DBNavigatorPessoas: TDBNavigator;
    ButtonCommit: TButton;
    RpDataFlashDataSetFormatter1: TRpDataFlashDataSetFormatter;
    ButtonXMLData: TButton;
    WebBrowser1: TWebBrowser;
    LabelUser: TLabel;
    EditUser: TEdit;
    EditSenha: TEdit;
    RpDataFlashCommandExecutorSomar: TRpDataFlashCommandExecutor;
    ButtonSomarExecutor: TButton;
    ButtonInverter: TButton;
    RpDataFlashDataSetPessoas: TRpDataFlashDataSet;
    RpDataFlashDataSetPessoasID: TIntegerField;
    RpDataFlashDataSetPessoasNOME: TWideStringField;
    RpDataFlashDataSetPessoasIDADE: TIntegerField;
    RpDataFlashDataSetPessoasDATA_CADASTRO: TDateTimeField;
    RpDataFlashDataSetPessoasSALARIO: TFloatField;
    ButtonGetFile: TButton;
    ButtonSendFile: TButton;
    Gauge1: TGauge;
    LabelStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonVerLogClick(Sender: TObject);
    procedure ButtonDesconectarClick(Sender: TObject);
    procedure ButtonConectarClick(Sender: TObject);
    procedure ButtonSomarProxyClick(Sender: TObject);
    procedure ButtonOpenDSClick(Sender: TObject);
    procedure ButtonCloseDSClick(Sender: TObject);
    procedure ButtonCommitClick(Sender: TObject);
    procedure ButtonXMLDataClick(Sender: TObject);
    procedure ButtonSomarExecutorClick(Sender: TObject);
    procedure ButtonInverterClick(Sender: TObject);
    procedure WebBrowser1NavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
    procedure ButtonGetFileClick(Sender: TObject);
    procedure ButtonSendFileClick(Sender: TObject);
    procedure RpDataFlashClientConnectionTesteNewLog(Sender: TObject;
      ALogType: TRpDataFlashServiceLog; const ALog: string;
      const AClientInfo: TRpDataFlashClientInfo);
    procedure RpDataFlashClientConnectionTesteStatus(Sender: TObject;
      const AStatus: TRpDataFlashStatusType; const AProcTotal,
      AProcCurrent: Integer; const AStatusStr: string);
//    procedure LRDataFlashConexaoClienteTesteStatus(Sender: TObject;
//      const ASituacao: TLRDataFlashStatusType; const AProcessamentoTotal,
//      AProcessamentoAtual: Integer);
  private
    { Private declarations }
    FInternalLog: TStrings;
    procedure SetEnables(const Value : Boolean);    
  public
    { Public declarations }
  end;

var
  FormMainClient: TFormMainClient;

implementation

uses
  uGeneratedProxy;

{$R *.dfm}

{ TFormMainClient }

procedure TFormMainClient.ButtonOpenDSClick(Sender: TObject);
begin
  if MemoLog.Text = EmptyStr then
    RpDataFlashDataSetPessoas.ProviderClass := 'DFPCadastro_Pessoas'
  else
  begin
    RpDataFlashDataSetPessoas.ProviderClass := '';
    RpDataFlashDataSetPessoas.ProviderCustom.SelectSQL.Text := MemoLog.Text;
  end;
  RpDataFlashDataSetPessoas.StartTransaction;
  RpDataFlashDataSetPessoas.Open( EditFiltro.Text );
end;

procedure TFormMainClient.ButtonXMLDataClick(Sender: TObject);
var
  lTempFile: TFileName;
  lXml: TStrings;
begin
  lTempFile := StringReplace(Application.ExeName, '.exe', '.xml', [rfIgnoreCase]);
  lXml := TStringList.Create;
  try
    lXml.Text := RpDataFlashDataSetPessoas.XMLData;
    lXml.SaveToFile(lTempFile);
    WebBrowser1.Hint := lTempFile;
    WebBrowser1.Navigate(lTempFile);
  finally
    FreeAndNil(lXml);
  end;
end;

procedure TFormMainClient.ButtonGetFileClick(Sender: TObject);
var
  lFileData: IRpFileProxy;
begin
  lFileData := TRpFileProxy.Create;

//  if ProxyFactory.Arquivos.GetFile('c:\Daruma32.log', lFileData) then
//    lFileData.SaveToFile('c:\NewFile.txt')
//  else
//    ShowMessage( ProxyFactory.Arquivos.GetLastError );

end;

procedure TFormMainClient.ButtonCloseDSClick(Sender: TObject);
begin
  RpDataFlashDataSetPessoas.Close;
end;

procedure TFormMainClient.ButtonCommitClick(Sender: TObject);
begin
  RpDataFlashDataSetPessoas.ApplyUpdates(0);
  RpDataFlashDataSetPessoas.CommitRetaining;
end;

procedure TFormMainClient.ButtonConectarClick(Sender: TObject);
begin
  try
    RpDataFlashClientConnectionTeste.Server := EditNomeServer.Text;

    RpDataFlashClientConnectionTeste.Port := StrToInt64Def( EditPorta.Text, 8890 );
    EditPorta.Text := IntToStr(RpDataFlashClientConnectionTeste.Port);

    RpDataFlashClientConnectionTeste.UserName := EditUser.Text;
    RpDataFlashClientConnectionTeste.Password := EditSenha.Text;

    RpDataFlashClientConnectionTeste.Connect;
    if RpDataFlashClientConnectionTeste.Connected then
    begin
      SetEnables(False);
//      ConfigureProxy(RpDataFlashClientConnectionTeste);
    end
    else
      SetEnables(True);
  except
    on E: Exception do
    begin
      SetEnables(True);
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMainClient.ButtonDesconectarClick(Sender: TObject);
begin
  RpDataFlashClientConnectionTeste.Disconnect;
  SetEnables(True);
end;

procedure TFormMainClient.ButtonInverterClick(Sender: TObject);
var
  lPalavra, lResult: string;
begin
  InputQuery('Informe uma Palavra', 'Informe uma palavra ou frase:', lPalavra);

  if not ProxyFactory.HARD_CODE.CodeInverter(lPalavra, lResult) then
    ShowMessage('Erro de proxy. ' + ProxyFactory.HARD_CODE.GetLastError)
  else
    ShowMessage(lPalavra + ' <-> ' + lResult);
end;

procedure TFormMainClient.ButtonSendFileClick(Sender: TObject);
var
  lFile : IRpFileProxy;
  lDestino: string;
begin
  with TOpenDialog.Create(Self) do
  try
    if Execute then
    begin
      lFile := TRpFileProxy.Create;
      lFile.LoadFromFile( FileName );
//      if ProxyFactory.FileManager.SendFile(lFile, FileName, lDestino) then
//        ShowMessage('Arquivo salvo em: ' + lDestino)
//      else
//        ShowMessage('ERRO!! ' + ProxyFactory.FileManager.GetLastError);
    end;

  finally
    Free;
  end;
end;

procedure TFormMainClient.ButtonSomarExecutorClick(Sender: TObject);
var
  lAux: string;
begin
  InputQuery('Valor A', 'Informe o valor de A:', lAux);
  RpDataFlashCommandExecutorSomar.Params['A'].AsFloat := StrToFloatDef(lAux, 0);

  InputQuery('Valor B', 'Informe o valor de B:', lAux);
  RpDataFlashCommandExecutorSomar.Params['B'].AsFloat := StrToFloatDef(lAux, 0);

  if RpDataFlashCommandExecutorSomar.Execute then
    ShowMessage( Format('%f + %f = %f', [
      RpDataFlashCommandExecutorSomar.Params['A'].AsFloat,
      RpDataFlashCommandExecutorSomar.Params['B'].AsFloat,
      RpDataFlashCommandExecutorSomar.ResultParams['X'].AsFloat] ) )
//  else
//    ShowMessage('Erro de processamento. ' + ProxyFactory.Matematica.GetLastError);
end;

procedure TFormMainClient.ButtonSomarProxyClick(Sender: TObject);
var
  lA, lB, lTotal: Double;
  lAux: string;
begin
  InputQuery('Valor A', 'Informe o valor de A:', lAux);
  lA := StrToFloatDef(lAux, 0);

  InputQuery('Valor B', 'Informe o valor de B:', lAux);
  lB := StrToFloatDef(lAux, 0);

  if ProxyFactory.MathAndText.AddNum(lA, lB, lTotal) then
    ShowMessage( Format('%f + %f = %f', [lA, lB, lTotal] ) )
  else
    ShowMessage('Erro de processamento. ' + ProxyFactory.MathAndText.GetLastError);
end;

procedure TFormMainClient.ButtonVerLogClick(Sender: TObject);
begin
  MemoLog.Lines.Assign( FInternalLog );
  FInternalLog.Clear;
end;

procedure TFormMainClient.FormCreate(Sender: TObject);
begin
  FInternalLog := TStringList.Create;
  LabelStatus.Caption := EmptyStr;

//  ConfigureProxy( RpDataFlashClientConnectionTeste );

  SetEnables( True );
end;

procedure TFormMainClient.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FInternalLog );
end;

procedure TFormMainClient.RpDataFlashClientConnectionTesteNewLog(
  Sender: TObject; ALogType: TRpDataFlashServiceLog; const ALog: string;
  const AClientInfo: TRpDataFlashClientInfo);
var
  lLogTxt : string;
begin
  case ALogType of
    slOnConnection: lLogTxt := 'Connection: ';
    slOnDisconnection : lLogTxt := 'Disconnection: ';
    slOnSend : lLogTxt := 'Send: ';
    slOnReceive: lLogTxt := 'Receive: ';
    slOnError : lLogTxt := 'ERROR: ';
    slOnStatus : lLogTxt := 'Status: ';
    slOnBridge : lLogTxt := 'Bridge: ';
    slOnCommand : lLogTxt := 'Command: ';
    slOnApplyRule : lLogTxt := 'Apply Rule: ';
    slOnFile : lLogTxt := 'File: ';
    slOnSync : lLogTxt := 'Sync: ';
    slOnSyncXml : lLogTxt := 'Sync XML: ';
  end;

  lLogTxt := lLogTxt + '[ ' + AClientInfo.DisplayName + ' / ' + AClientInfo.IP + ' ] -> ' + ALog;

  FInternalLog.Add( lLogTxt );
  FInternalLog.Add( StringOfChar('-', 80 ) );
end;

procedure TFormMainClient.RpDataFlashClientConnectionTesteStatus(
  Sender: TObject; const AStatus: TRpDataFlashStatusType; const AProcTotal,
  AProcCurrent: Integer; const AStatusStr: string);
var
  lLog : string;
begin
  Gauge1.MaxValue := AProcTotal;
  Gauge1.Progress := AProcCurrent;

  case AStatus of
    stPreparingData: lLog := 'PreparingData: ';
    stSendingData: lLog := 'SendingData: ';
    stReceivingData: lLog := 'ReceivingData: ';
  end;
  LabelStatus.Caption := lLog + AStatusStr;
end;

procedure TFormMainClient.SetEnables(const Value: Boolean);
begin
  EditNomeServer.Enabled := Value;
  EditPorta.Enabled := Value;
  EditSenha.Enabled := Value;
  EditUser.Enabled := Value;

  ButtonConectar.Enabled := Value;
  ButtonDesconectar.Enabled := not Value;

  // comandos
  ButtonSomarProxy.Enabled := not Value;
  ButtonSomarExecutor.Enabled := not Value;
  ButtonInverter.Enabled := not Value;
  ButtonGetFile.Enabled := not Value;
  ButtonSendFile.Enabled := not Value;
end;

procedure TFormMainClient.WebBrowser1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  DeleteFile(WebBrowser1.Hint);
end;

end.
