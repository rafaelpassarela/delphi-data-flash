unit uMainServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uLRDF.Types, uLRDF.ProxyGenerator, uLRDF.Comando,
  uLRDF.DataSetProvider, uLRDF.Component;

//  , uLRDF.Component, uLRDF.Types, uLRDF.Comando, uLRDF.DataSetProvider,
//  uLRDF.ComandoGetProviderList, uLRDF.ProxyGenerator;

type
  TComandoCodeInverter = class(TLRDataFlashComando)
  protected
    function DoExecutar : Boolean; override;
    function DoGetDescricao: string; override;
    procedure DoRegistrarParametros; override;
  end;

  TFormMainServer = class(TForm)
    LRDataFlashConexaoServerTeste: TLRDataFlashConexaoServer;
    LabelNomeServer: TLabel;
    EditNomeServer: TEdit;
    LabelPorta: TLabel;
    EditPorta: TEdit;
    ButtonConectar: TButton;
    ButtonDesconectar: TButton;
    MemoLog: TMemo;
    ButtonVerLog: TButton;
    DFCControllerMatematica: TLRDataFlashComandController;
    DFPCadastro_Pessoas: TLRDataFlashDataSetProvider;
    DFCControllerManipulaTexto: TLRDataFlashComandController;
    DFCControllerArquivos: TLRDataFlashComandController;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConectarClick(Sender: TObject);
    procedure ButtonDesconectarClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LRDataFlashConexaoServerTesteNovoLog(Sender: TObject;
      ATipoLog: TLRDataFlashTipoLogService; const ALog: string;
      const AClientInfo: TLRDataFlashClientInfo);
    procedure ButtonVerLogClick(Sender: TObject);
    function DFCControllerMatematicaComandos0Execute(
      const AComando: IComandoTCPInterfaced): Boolean;
    procedure LRDataFlashConexaoServerTesteConexaoCliente(Sender: TObject;
      const AConexaoItem: TLRDataFlashConexaoItem);
    procedure LRDataFlashConexaoServerTesteAutenticarCliente(Sender: TObject;
      const AItemConexao: IAutenticationProvider; out AAutenticado: Boolean;
      out AErrorMessage: string);
    function DFCControllerManipulaTextoComandos0Execute(
      const AComando: IComandoTCPInterfaced): Boolean;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function DFCControllerArquivosComandos0Execute(
      const AComando: IComandoTCPInterfaced): Boolean;
    function DFCControllerArquivosComandos1Execute(
      const AComando: IComandoTCPInterfaced): Boolean;
  private
    { Private declarations }
    FInternalLog: TStrings;
    procedure SetEnables(const Value : Boolean);
  public
    { Public declarations }
  end;

var
  FormMainServer: TFormMainServer;

implementation

uses uDataModuleServer;

{$R *.dfm}

procedure TFormMainServer.ButtonVerLogClick(Sender: TObject);
begin
  MemoLog.Lines.Assign( FInternalLog );
  FInternalLog.Clear;
end;

function TFormMainServer.DFCControllerArquivosComandos0Execute(
  const AComando: IComandoTCPInterfaced): Boolean;
var
  lNomeArquivo: string;
  lFile : TFileProxy;
begin
  lNomeArquivo := AComando.GetParametros.Parametro['FileName'].AsString;
  lFile := TFileProxy.Create;
  try
    lFile.LoadFromFile( lNomeArquivo );
    AComando.GetParametros.Retorno['FileData'].AsBase64 := lFile.Save;
    Result := True;
  finally
    FreeAndNil( lFile );
  end;
end;

function TFormMainServer.DFCControllerArquivosComandos1Execute(
  const AComando: IComandoTCPInterfaced): Boolean;
var
//  lFile : IFileProxy;
  lFileName: string;
begin
//  lFile := TFileProxy.Create;
  lFileName := AComando.GetParametros.Parametro['FileName'].AsString;
  lFileName := ExtractFileName(lFileName);

  lFileName := 'D:\Arq_Client_' + lFileName;
  AComando.GetParametros.Parametro['FileData'].SaveToFile( lFileName );
  AComando.GetParametros.Retorno['LocalSalvo'].AsString := lFileName;

  Result := True;
end;

function TFormMainServer.DFCControllerManipulaTextoComandos0Execute(
  const AComando: IComandoTCPInterfaced): Boolean;
begin
  try
    AComando.GetParametros.Retorno['StrC'].AsString := AComando.GetParametros.Parametro['StrA'].AsString
                                                     + AComando.GetParametros.Parametro['StrB'].AsString;
    Result := True;
  except
    Result := False;
  end;
end;

function TFormMainServer.DFCControllerMatematicaComandos0Execute(
  const AComando: IComandoTCPInterfaced): Boolean;
var
  lA, lB, lTotal : Double;
begin
  try
    // read input parans
    lA := AComando.GetParametros.Parametro['A'].AsFloat;
    lB := AComando.GetParametros.Parametro['B'].AsFloat;

    // execute the command event
    lTotal := lA + lB;

    // return the output value to client
    AComando.GetParametros.Retorno['X'].AsFloat := lTotal;
    Result := True;
  except
    // on any error, sinalize as "not executed"
    Result := False;
  end;
end;

procedure TFormMainServer.ButtonConectarClick(Sender: TObject);
begin
  try
    LRDataFlashConexaoServerTeste.Porta := StrToInt64Def( EditPorta.Text, 8890 );
    EditPorta.Text := IntToStr(LRDataFlashConexaoServerTeste.Porta);

    LRDataFlashConexaoServerTeste.Conectar;
    if LRDataFlashConexaoServerTeste.Conectado then
      SetEnables(False)
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

procedure TFormMainServer.ButtonDesconectarClick(Sender: TObject);
begin
  LRDataFlashConexaoServerTeste.Desconectar;
  SetEnables(True);
end;

procedure TFormMainServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  LRDataFlashConexaoServerTeste.Desconectar;
end;

procedure TFormMainServer.FormCreate(Sender: TObject);
begin
  EditNomeServer.Text := LRDataFlashConexaoServerTeste.Servidor;
  FInternalLog := TStringList.Create;
  SetEnabled(True);
end;

procedure TFormMainServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FInternalLog);
end;

procedure TFormMainServer.LRDataFlashConexaoServerTesteAutenticarCliente(
  Sender: TObject; const AItemConexao: IAutenticationProvider;
  out AAutenticado: Boolean; out AErrorMessage: string);
begin
  AAutenticado := (AItemConexao.UserName = 'TESTE') and (AItemConexao.Password = '1234');
  if not AAutenticado then
    AErrorMessage := 'Usuário ou senha inválidos. Para login, utilize:'#10#13'Usuário = TESTE'#10#13'Senha = 1234';
end;

procedure TFormMainServer.LRDataFlashConexaoServerTesteConexaoCliente(
  Sender: TObject; const AConexaoItem: TLRDataFlashConexaoItem);
begin
  // cria uma conexão entre o cliente e o banco de dados
  AConexaoItem.Executor := (TDataModuleServer.Create(nil) as IExecutorComandoPacote);
end;

procedure TFormMainServer.LRDataFlashConexaoServerTesteNovoLog(Sender: TObject;
  ATipoLog: TLRDataFlashTipoLogService; const ALog: string;
  const AClientInfo: TLRDataFlashClientInfo);
var
  lLinha : string;
begin
  if Assigned(FInternalLog) then
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
end;

procedure TFormMainServer.SetEnables(const Value: Boolean);
begin
  EditNomeServer.Enabled := Value;
  EditPorta.Enabled := Value;

  ButtonConectar.Enabled := Value;
  ButtonDesconectar.Enabled := not Value;
end;

{ TComandoHardCode }

function TComandoCodeInverter.DoExecutar: Boolean;
var
  i: Integer;
  lPalavra: string;
  lInvertida: string;
begin
  lInvertida := EmptyStr;
  lPalavra := Parametro['Palavra'].AsString;
  for i := 1 to Length(lPalavra) do
    lInvertida := lPalavra[i] + lInvertida;

  Retorno['Invertida'].AsString := lInvertida;
  Result := True;
end;

function TComandoCodeInverter.DoGetDescricao: string;
begin
  Result := 'Comando de Teste declarado no código, diferente dos outros que é no componente. ' + sLineBreak
          + 'Este comando recebe uma palavra e retorna ela invertida.';
end;

procedure TComandoCodeInverter.DoRegistrarParametros;
begin
  inherited;
  NovoParametro('Palavra', tvpString);
  NovoRetorno('Invertida', tvpString);
end;

initialization
  TCPClassRegistrer.Registrar(TComandoCodeInverter, 'HARD_CODE');

end.
