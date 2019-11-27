unit uMainClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, DBClient, Grids, DBGrids, ExtCtrls, DBCtrls, OleCtrls, SHDocVw,
  uRpDataFlash.ProxyGenerator, Gauges, uRpDataFlash.Types,
  uRpDataFlash.DataSet, uRpDataFlash.CommandExecutor, uRpDataFlash.Components,
  uRpDataFlash.CommandHelper;

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonVerLogClick(Sender: TObject);
    procedure LRDataFlashConexaoClienteTesteNovoLog(Sender: TObject;
      ATipoLog: TLRDataFlashTipoLogService; const ALog: string;
      const AClientInfo: TLRDataFlashClientInfo);
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
    procedure LRDataFlashConexaoClienteTesteStatus(Sender: TObject;
      const ASituacao: TLRDataFlashStatusType; const AProcessamentoTotal,
      AProcessamentoAtual: Integer);
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
    LRDataFlashDataSetPessoas.ProviderClass := 'DFPCadastro_Pessoas'
  else
  begin
    LRDataFlashDataSetPessoas.ProviderClass := '';
    LRDataFlashDataSetPessoas.ProviderCustom.SelectSQL.Text := MemoLog.Text;
  end;
  LRDataFlashDataSetPessoas.StartTransaction;
  LRDataFlashDataSetPessoas.Open( EditFiltro.Text );
end;

procedure TFormMainClient.ButtonXMLDataClick(Sender: TObject);
var
  lTempFile: TFileName;
  lXml: TStrings;
begin
  lTempFile := StringReplace(Application.ExeName, '.exe', '.xml', [rfIgnoreCase]);
  lXml := TStringList.Create;
  try
    lXml.Text := LRDataFlashDataSetPessoas.XMLData;
    lXml.SaveToFile(lTempFile);
    WebBrowser1.Hint := lTempFile;   
    WebBrowser1.Navigate(lTempFile);
  finally
    FreeAndNil(lXml);
  end;
end;

procedure TFormMainClient.ButtonGetFileClick(Sender: TObject);
var
  lFileData: IFileProxy;
begin
  lFileData := TFileProxy.Create;

  if ProxyFactory.Arquivos.GetFile('c:\Daruma32.log', lFileData) then
    lFileData.SaveToFile('c:\NewFile.txt')
  else
    ShowMessage( ProxyFactory.Arquivos.GetLastError );

end;

procedure TFormMainClient.ButtonCloseDSClick(Sender: TObject);
begin
  LRDataFlashDataSetPessoas.Close;
end;

procedure TFormMainClient.ButtonCommitClick(Sender: TObject);
begin
  LRDataFlashDataSetPessoas.ApplyUpdates(0);
  LRDataFlashDataSetPessoas.CommitRetaining;
end;

procedure TFormMainClient.ButtonConectarClick(Sender: TObject);
begin
  try
    LRDataFlashConexaoClienteTeste.Servidor := EditNomeServer.Text;
      
    LRDataFlashConexaoClienteTeste.Porta := StrToInt64Def( EditPorta.Text, 8890 );
    EditPorta.Text := IntToStr(LRDataFlashConexaoClienteTeste.Porta);

    LRDataFlashConexaoClienteTeste.UserName := EditUser.Text;
    LRDataFlashConexaoClienteTeste.Password := EditSenha.Text;    

    LRDataFlashConexaoClienteTeste.Conectar;
    if LRDataFlashConexaoClienteTeste.Conectado then
    begin
      SetEnables(False);
      ConfigureProxy(LRDataFlashConexaoClienteTeste);
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
  LRDataFlashConexaoClienteTeste.Desconectar;
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
  lFile : IFileProxy;
  lDestino: string;
begin
  with TOpenDialog.Create(Self) do
  try
    if Execute then
    begin
      lFile := TFileProxy.Create;
      lFile.LoadFromFile( FileName );
      if ProxyFactory.Arquivos.SendFile(lFile, FileName, lDestino) then
        ShowMessage('Arquivo salvo em: ' + lDestino)
      else
        ShowMessage('ERRO!! ' + ProxyFactory.Arquivos.GetLastError);
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
  LRDataFlashExecutorComandoSomar.Parametros['A'].AsFloat := StrToFloatDef(lAux, 0);

  InputQuery('Valor B', 'Informe o valor de B:', lAux);
  LRDataFlashExecutorComandoSomar.Parametros['B'].AsFloat := StrToFloatDef(lAux, 0);

  if LRDataFlashExecutorComandoSomar.Execute then
    ShowMessage( Format('%f + %f = %f', [
      LRDataFlashExecutorComandoSomar.Parametros['A'].AsFloat,
      LRDataFlashExecutorComandoSomar.Parametros['B'].AsFloat,
      LRDataFlashExecutorComandoSomar.Retornos['X'].AsFloat] ) )
  else
    ShowMessage('Erro de processamento. ' + ProxyFactory.Matematica.GetLastError);
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

  if ProxyFactory.Matematica.Somar(lA, lB, lTotal) then
    ShowMessage( Format('%f + %f = %f', [lA, lB, lTotal] ) )
  else
    ShowMessage('Erro de processamento. ' + ProxyFactory.Matematica.GetLastError);  
end;

procedure TFormMainClient.ButtonVerLogClick(Sender: TObject);
begin
  MemoLog.Lines.Assign( FInternalLog );
  FInternalLog.Clear;
end;

procedure TFormMainClient.FormCreate(Sender: TObject);
begin
  FInternalLog := TStringList.Create;

  ConfigureProxy( LRDataFlashConexaoClienteTeste );

  SetEnables( True );
end;

procedure TFormMainClient.FormDestroy(Sender: TObject);
begin
  FreeAndNil( FInternalLog );
end;

procedure TFormMainClient.LRDataFlashConexaoClienteTesteNovoLog(Sender: TObject;
  ATipoLog: TLRDataFlashTipoLogService; const ALog: string;
  const AClientInfo: TLRDataFlashClientInfo);
var
  lLinha : string;
begin
  case ATipoLog of
    tlsConexao    : lLinha := 'Conexao: ';
    tlsDesconexao : lLinha := 'Desconexao: ';
    tlsEnvio      : lLinha := 'Envio: ';
    tlsRecebimento: lLinha := 'Recebimento: ';
    tlsErro       : lLinha := 'ERRO: ';
    tlsStatus     : lLinha := 'Status: ';
    tlsPonte      : lLinha := 'Ponte: ';
    tlsComando    : lLinha := 'Comando: ';
    tlsRegra      : lLinha := 'Regra: ';
    tlsArquivo    : lLinha := 'Arquivo: ';
  end;

  lLinha := lLinha + '[ ' + AClientInfo.DisplayName + ' / ' + AClientInfo.IP + ' ] -> ' + ALog;

  FInternalLog.Add( lLinha );
  FInternalLog.Add( StringOfChar('-', 80 ) );
end;

procedure TFormMainClient.LRDataFlashConexaoClienteTesteStatus(Sender: TObject;
  const ASituacao: TLRDataFlashStatusType; const AProcessamentoTotal,
  AProcessamentoAtual: Integer);
begin
  Gauge1.MaxValue := AProcessamentoTotal;
  Gauge1.Progress := AProcessamentoAtual;
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
end;

procedure TFormMainClient.WebBrowser1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
begin
  DeleteFile(WebBrowser1.Hint);
end;

end.
