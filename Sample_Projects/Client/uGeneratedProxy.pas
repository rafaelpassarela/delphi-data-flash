unit uGeneratedProxy;

//   Não modifique esta Unit, seu código é gerado automaticamente pelo Cliente de
// TCP buscando as classes de serviço registradas no servidor.
// Proxy gerado em 20/02/2014 14:59:31

interface

uses
  SysUtils, uLRDF.Comando, uLRDF.Component, uLRDF.Types, Windows, ShellApi;

type
  TBase64 = string;

  TCustomProxy = class(TCustomProxyClient)
  protected
    procedure DoAoProcessarErroComunicacao; override;
  end;

  TProxyHARD_CODE = class(TCustomProxy)
  public
    //TComandoCodeInverter
    { Comando de Teste declarado no código, diferente dos outros que é no componente. 
Este
      comando recebe uma palavra e retorna ela invertida. }
    function CodeInverter(const pPalavra: String; out AInvertida: String) : Boolean;
  end;

  TProxyMatematica = class(TCustomProxy)
  public
    //Somar
    { Somar Dois Números }
    function Somar(const pA: Double; const pB: Double; out AX: Double) : Boolean;
    //Multiplicar
    { Multiplica Dois Numeros }
    function Multiplicar(const pValA: Double; const pValB: Double; out ATotal: Double) : Boolean;
  end;

  TProxyManipulaTexto = class(TCustomProxy)
  public
    //Concatenar
    { Concatena Duas Strings }
    function Concatenar(const pStrA: String; const pStrB: String; out AStrC: String) : Boolean;
  end;

  TProxyArquivos = class(TCustomProxy)
  public
    //GetFile
    { Busca o arquivo informado no parâmetro no servidor }
    function GetFile(const pFileName: String; AFileData: IFileProxy) : Boolean;
    //SendFile
    { Envia um arquivo para o servidor }
    function SendFile(const pFileData: IFileProxy; const pFileName: String;
      out ALocalSalvo: String) : Boolean;
  end;

  TProxyFactory = class
  private
    FTcpClient : TLRDataFlashConexaoCliente;
    FSharedClient: Boolean;
    FProxyHARD_CODE : TProxyHARD_CODE;
    FProxyMatematica : TProxyMatematica;
    FProxyManipulaTexto : TProxyManipulaTexto;
    FProxyArquivos : TProxyArquivos;
  public
    constructor Create;
    destructor Destroy; override;
    function HARD_CODE : TProxyHARD_CODE;
    function Matematica : TProxyMatematica;
    function ManipulaTexto : TProxyManipulaTexto;
    function Arquivos : TProxyArquivos;
  end;

function ProxyFactory : TProxyFactory;
procedure ConfigureProxy(ATcpClient : TLRDataFlashConexaoCliente); overload;
procedure ConfigureProxy(const pServer : string; const pPort : Integer = 8890;
  const pCriptografia : TLRDataFlashTipoCriptografia = tcBase64Compressed;
  const pTipoComunicacao : TLRDataFlashTipoComunicacao = tcTexto); overload;

implementation

var
  FProxyFactory : TProxyFactory;

function ProxyFactory : TProxyFactory;
begin
  if FProxyFactory = nil then
    raise Exception.Create('ProxyFactory não foi inicializado ou configurado.');
  Result := FProxyFactory;
end;

procedure ConfigureProxy(ATcpClient : TLRDataFlashConexaoCliente); overload;
begin
  if FProxyFactory = nil then
    FProxyFactory := TProxyFactory.Create;

  if FProxyFactory.FTcpClient = nil then
  begin
    FProxyFactory.FTcpClient := ATcpClient;
    FProxyFactory.FSharedClient := True;
  end;
end;

procedure ConfigureProxy(const pServer : string; const pPort : Integer;
  const pCriptografia : TLRDataFlashTipoCriptografia; const pTipoComunicacao : TLRDataFlashTipoComunicacao); overload;
begin
  if FProxyFactory = nil then
    FProxyFactory := TProxyFactory.Create;

  if FProxyFactory.FTcpClient = nil then
  begin
    FProxyFactory.FTcpClient := TLRDataFlashConexaoCliente.Create(nil);
    FProxyFactory.FTcpClient.Servidor := pServer;
    FProxyFactory.FTcpClient.Porta := pPort;
    FProxyFactory.FTcpClient.TipoCriptografia := pCriptografia;
    FProxyFactory.FTcpClient.TipoComunicacao := pTipoComunicacao;
    FProxyFactory.FSharedClient := False;
  end;
end;

{ TCustomProxy }

procedure TCustomProxy.DoAoProcessarErroComunicacao;
begin
  inherited;
  //Sample for Restart the service
  // ShellExecute(0, nil, 'cmd.exe', '/C net start SomeServiceName', nil, SW_HIDE);
end;

{ TProxyHARD_CODE }

function TProxyHARD_CODE.CodeInverter(const pPalavra: String; out AInvertida: String) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('TComandoCodeInverter');
    lCmd.Parametros.Novo('Palavra', pPalavra, tpEntrada, tvpString);
    FTcpClient.Comunicar(lCmd);
    AInvertida := lCmd.Retorno['Invertida'].AsString;
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

{ TProxyMatematica }

function TProxyMatematica.Somar(const pA: Double; const pB: Double; out AX: Double) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('Somar');
    lCmd.Parametros.Novo('A', pA, tpEntrada, tvpFloat);
    lCmd.Parametros.Novo('B', pB, tpEntrada, tvpFloat);
    FTcpClient.Comunicar(lCmd);
    AX := lCmd.Retorno['X'].AsFloat;
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

function TProxyMatematica.Multiplicar(const pValA: Double; const pValB: Double;
  out ATotal: Double) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('Multiplicar');
    lCmd.Parametros.Novo('ValA', pValA, tpEntrada, tvpFloat);
    lCmd.Parametros.Novo('ValB', pValB, tpEntrada, tvpFloat);
    FTcpClient.Comunicar(lCmd);
    ATotal := lCmd.Retorno['Total'].AsFloat;
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

{ TProxyManipulaTexto }

function TProxyManipulaTexto.Concatenar(const pStrA: String; const pStrB: String;
  out AStrC: String) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('Concatenar');
    lCmd.Parametros.Novo('StrA', pStrA, tpEntrada, tvpString);
    lCmd.Parametros.Novo('StrB', pStrB, tpEntrada, tvpString);
    FTcpClient.Comunicar(lCmd);
    AStrC := lCmd.Retorno['StrC'].AsString;
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

{ TProxyArquivos }

function TProxyArquivos.GetFile(const pFileName: String; AFileData: IFileProxy) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('GetFile');
    lCmd.Parametros.Novo('FileName', pFileName, tpEntrada, tvpString);
    FTcpClient.Comunicar(lCmd);
    AFileData.Load(lCmd.Retorno['FileData'].AsBase64);
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

function TProxyArquivos.SendFile(const pFileData: IFileProxy; const pFileName: String;
  out ALocalSalvo: String) : Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
begin
  FLastError := EmptyStr;
  FStatusProcessamento := tspNenhum;
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando('SendFile');
    lCmd.Parametros.Novo('FileData', ' ', tpEntrada, tvpFile);
    lCmd.Parametro['FileData'].AsBase64 := pFileData.Save;
    lCmd.Parametros.Novo('FileName', pFileName, tpEntrada, tvpString);
    FTcpClient.Comunicar(lCmd);
    ALocalSalvo := lCmd.Retorno['LocalSalvo'].AsString;
    Result := lCmd.StatusRetorno;
    FStatusProcessamento := lCmd.StatusProcessamento;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      FLastError := E.Message;
    end;
  end;
  FreeAndNil(lCmd);
end;

{ TProxyFactory }

constructor TProxyFactory.Create;
begin
  FSharedClient := False;
end;

destructor TProxyFactory.Destroy;
begin
  if Assigned(FProxyHARD_CODE) then
    FreeAndNil(FProxyHARD_CODE);
  if Assigned(FProxyMatematica) then
    FreeAndNil(FProxyMatematica);
  if Assigned(FProxyManipulaTexto) then
    FreeAndNil(FProxyManipulaTexto);
  if Assigned(FProxyArquivos) then
    FreeAndNil(FProxyArquivos);
  if Assigned(FTcpClient) and (not FSharedClient) then
  begin
    try
      if FTcpClient.Conectado then
        FTcpClient.Desconectar;
    finally
      FreeAndNil(FTcpClient);
    end;
  end;
  inherited;
end;

function TProxyFactory.HARD_CODE: TProxyHARD_CODE;
begin
  if FProxyHARD_CODE = nil then
    FProxyHARD_CODE := TProxyHARD_CODE.Create(FTcpClient);
  Result := FProxyHARD_CODE;
end;

function TProxyFactory.Matematica: TProxyMatematica;
begin
  if FProxyMatematica = nil then
    FProxyMatematica := TProxyMatematica.Create(FTcpClient);
  Result := FProxyMatematica;
end;

function TProxyFactory.ManipulaTexto: TProxyManipulaTexto;
begin
  if FProxyManipulaTexto = nil then
    FProxyManipulaTexto := TProxyManipulaTexto.Create(FTcpClient);
  Result := FProxyManipulaTexto;
end;

function TProxyFactory.Arquivos: TProxyArquivos;
begin
  if FProxyArquivos = nil then
    FProxyArquivos := TProxyArquivos.Create(FTcpClient);
  Result := FProxyArquivos;
end;

initialization

finalization
  if FProxyFactory <> nil then
    FProxyFactory.Free;

end.
