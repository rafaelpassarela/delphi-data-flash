unit uGeneratedProxy;

//   Não modifique esta Unit, seu código é gerado automaticamente pelo Cliente de
// TCP buscando as classes de serviço registradas no servidor.
// - Gerado em...: 28/11/2019 17:12:02
// - App Servidor: "D:\Git\delphi-data-flash\Bin\Server.exe"
// - Server......: Programacao-2

interface

uses
  SysUtils, uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types, 
  uRpSerialization, Windows, ShellApi;

type
  TBase64 = string;
  TConnectionType = (ctTcpIp, ctREST);

  TCustomProxy = class(TCustomProxyClient)
  private
    FReconnect: Boolean;
  protected
    procedure DoAoProcessarErroComunicacao; override;
  public
    property Reconnect : Boolean read FReconnect write FReconnect;
  end;

  TProxyHARD_CODE = class(TCustomProxy)
  public
    { Comando: TComandoCodeInverter
      Comando de Teste declarado no código, diferente dos outros que é no componente. 
Este comando
      recebe uma palavra e retorna ela invertida. }
    function CodeInverter(const pPalavra: String; const pInvertida: String) : Boolean;
  end;

  TProxyMathAndText = class(TCustomProxy)
  public
    { Comando: AddNum
      Adds two numbers }
    function AddNum(const pA: Double; const pB: Double; out AX: Double) : Boolean;
    { Comando: Multiply
      Multiply two numbers }
    function Multiply(const pA: Double; const pB: Double; out AC: Double) : Boolean;
    { Comando: TextInverter
      Invert Text }
    function TextInverter(const pText: String; out AInverted: String) : Boolean;
  end;

  TProxyFileManager = class(TCustomProxy)
  public
    { Comando: SendFile
      Send File to Server }
    function SendFile(const pFileName: String; const pFileData: TBase64;
      out ASavePath: String) : Boolean;
    { Comando: LoadFile
      Load a file from Server }
    function LoadFile(const pFileName: String; out AFileData: TBase64) : Boolean;
  end;

  TProxyFactory = class
  protected
    function GetLazyConection: Boolean;
    procedure SetLazyConection(const Value: Boolean);
  private
    FClientTCP : TRpDataFlashClientConnection;
    FClientREST : TRpDataFlashRESTClient;
    FSharedClientTCP: Boolean;
    FSharedClientREST: Boolean;
    FSerializationFormat: TSerializationFormat;
    FReconnect: Boolean;
    FConnectionType: TConnectionType;
    FBusy : Boolean;
    FCheckBusy: Boolean;
    FProxyHARD_CODE : TProxyHARD_CODE;
    FProxyMathAndText : TProxyMathAndText;
    FProxyFileManager : TProxyFileManager;
    procedure DoCheckBusy;
    procedure BusyCallback(const AStart : Boolean);
  public
    constructor Create(AClient : TRpDataFlashCustomClientConnection; const AConnectionType : TConnectionType);
    destructor Destroy; override;
    procedure Config(const AHost : string; const APorta, APortaFTP : Integer;
      const AConnectionType : TConnectionType;
      const ALocalHostToIP : Boolean; 
      const ATipoCriptografia : TRpDataFlashEncryptionType = tcBase64Compressed;
      const ATipoComunicacao : TRpDataFlashCommunicationType = ctText); overload;
    procedure Config(const AConexaoTCP : TRpDataFlashClientConnection); overload;
    procedure Config(const AConexaoREST : TRpDataFlashRESTClient); overload;
    function ServerOnline : Boolean;
    property Reconnect : Boolean read FReconnect write FReconnect;
    property ConnectionType : TConnectionType read FConnectionType;
    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat;
    property CheckBusy : Boolean read FCheckBusy write FCheckBusy default True;
    property LazyConection : Boolean read GetLazyConection write SetLazyConection;
    function HARD_CODE : TProxyHARD_CODE;
    function MathAndText : TProxyMathAndText;
    function FileManager : TProxyFileManager;
  end;

function ProxyFactory(ATcpClient : TRpDataFlashClientConnection = nil) : TProxyFactory;
function ProxyFactoryRest(ARestClient : TRpDataFlashRESTClient = nil) : TProxyFactory;
 
procedure NewProxyFactory(out AProxyFactory : TProxyFactory; const ATcpClient : TRpDataFlashClientConnection = nil);
procedure NewProxyFactoryRest(out AProxyFactory : TProxyFactory; const ARestClient : TRpDataFlashRESTClient = nil);
 
procedure RenewProxyFactory;
procedure RenewProxyFactoryRest;
 
procedure FreeProxyFactory;
procedure FreeProxyFactoryRest;

implementation

var
  FProxyFactory : TProxyFactory;
  FProxyFactoryRest : TProxyFactory;

function ProxyFactory(ATcpClient : TRpDataFlashClientConnection) : TProxyFactory;
begin
  if FProxyFactory = nil then
    FProxyFactory := TProxyFactory.Create(ATcpClient, ctTcpIp);
  Result := FProxyFactory;
end;
 
function ProxyFactoryRest(ARestClient : TRpDataFlashRESTClient) : TProxyFactory;
begin
  if FProxyFactoryRest = nil then
    FProxyFactoryRest := TProxyFactory.Create(ARestClient, ctREST);
  Result := FProxyFactoryRest;
end;
 
procedure NewProxyFactory(out AProxyFactory : TProxyFactory; const ATcpClient : TRpDataFlashClientConnection = nil);
begin
  AProxyFactory := TProxyFactory.Create(ATcpClient, ctTcpIp);
end;
 
procedure NewProxyFactoryRest(out AProxyFactory : TProxyFactory; const ARestClient : TRpDataFlashRESTClient = nil);
begin
  AProxyFactory := TProxyFactory.Create(ARestClient, ctREST);
end;
 
procedure RenewProxyFactory;
var
  lClient: TRpDataFlashClientConnection;
begin
  if Assigned(FProxyFactory) then
  begin
    if FProxyFactory.FSharedClientTCP then
      lClient := FProxyFactory.FClientTCP
    else
      lClient := nil;
 
    FreeAndNil( FProxyFactory );
    ProxyFactory( lClient );
  end
  else
    ProxyFactory;
end;
 
procedure RenewProxyFactoryRest;
var
  lClientRest: TRpDataFlashRESTClient;
begin
  if Assigned(FProxyFactoryRest) then
  begin
    if FProxyFactoryRest.FSharedClientREST then
      lClientRest := FProxyFactoryRest.FClientREST
    else
      lClientRest := nil;
 
    FreeAndNil( FProxyFactoryRest );
    ProxyFactoryRest( lClientRest );
  end
  else
    ProxyFactoryRest;
end;
 
procedure FreeProxyFactory;
begin
  if Assigned(FProxyFactory) then
    FreeAndNil( FProxyFactory );
end;
 
procedure FreeProxyFactoryRest;
begin
  if Assigned(FProxyFactoryRest) then
    FreeAndNil( FProxyFactoryRest );
end;

{ TCustomProxy }

procedure TCustomProxy.DoAoProcessarErroComunicacao;
begin
  inherited;
  //Sample for Restart the service
  //if FReconnect then
  //  ShellExecute(0, nil, 'cmd.exe', '/C net start SomeServiceName', nil, SW_HIDE);
end;

{ TProxyHARD_CODE }

function TProxyHARD_CODE.CodeInverter(const pPalavra: String; const pInvertida: String) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('Palavra', pPalavra, tpInput, tvpString);
    lParametros.AddNew('Invertida', pInvertida, tpInput, tvpString);
    Result := DoEnviar('TComandoCodeInverter', lParametros);
  finally
    FreeAndNil(lParametros);
  end;
end;

{ TProxyMathAndText }

function TProxyMathAndText.AddNum(const pA: Double; const pB: Double; out AX: Double) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('A', pA, tpInput, tvpFloat);
    lParametros.AddNew('B', pB, tpInput, tvpFloat);
    Result := DoEnviar('AddNum', lParametros);
    if Result then
    begin
      AX := lParametros.ResultParam['X'].AsFloat;
    end;
  finally
    FreeAndNil(lParametros);
  end;
end;

function TProxyMathAndText.Multiply(const pA: Double; const pB: Double; out AC: Double) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('A', pA, tpInput, tvpFloat);
    lParametros.AddNew('B', pB, tpInput, tvpFloat);
    Result := DoEnviar('Multiply', lParametros);
    if Result then
    begin
      AC := lParametros.ResultParam['C'].AsFloat;
    end;
  finally
    FreeAndNil(lParametros);
  end;
end;

function TProxyMathAndText.TextInverter(const pText: String; out AInverted: String) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('Text', pText, tpInput, tvpString);
    Result := DoEnviar('TextInverter', lParametros);
    if Result then
    begin
      AInverted := lParametros.ResultParam['Inverted'].AsString;
    end;
  finally
    FreeAndNil(lParametros);
  end;
end;

{ TProxyFileManager }

function TProxyFileManager.SendFile(const pFileName: String; const pFileData: TBase64;
  out ASavePath: String) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('FileName', pFileName, tpInput, tvpString);
    lParametros.AddNew('FileData',  ' ' , tpInput, tvpBase64);
    lParametros.Param['FileData'].AsBase64 := pFileData;
    Result := DoEnviar('SendFile', lParametros);
    if Result then
    begin
      ASavePath := lParametros.ResultParam['SavePath'].AsString;
    end;
  finally
    FreeAndNil(lParametros);
  end;
end;

function TProxyFileManager.LoadFile(const pFileName: String; out AFileData: TBase64) : Boolean;
var
  lParametros : TRpDataFlashCommandParameterList;
begin
  lParametros := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParametros.AddNew('FileName', pFileName, tpInput, tvpString);
    Result := DoEnviar('LoadFile', lParametros);
    if Result then
    begin
      AFileData := lParametros.ResultParam['FileData'].AsBase64;
    end;
  finally
    FreeAndNil(lParametros);
  end;
end;

{ TProxyFactory }

constructor TProxyFactory.Create(AClient : TRpDataFlashCustomClientConnection; const AConnectionType : TConnectionType);
var
  lFileConf : IRpDataFlashConfig;
begin
  FConnectionType := AConnectionType;
  FSerializationFormat := sfXML;
  FReconnect := True;
  FSharedClientTCP := False;
  FSharedClientREST := False;
  FClientTCP := nil;
  FClientREST := nil;
  FBusy := False;
  FCheckBusy := True;
 
  case AConnectionType of
    ctTcpIp:
      if Assigned(AClient) then
        FClientTCP := AClient as TRpDataFlashClientConnection;
    ctREST:
      if Assigned(AClient) then
        FClientREST := AClient as TRpDataFlashRESTClient;
  end;
  FSharedClientTCP := Assigned(FClientTCP);
  FSharedClientREST := Assigned(FClientREST);
  if (not FSharedClientREST) or (not FSharedClientTCP) then
  begin
//    lFileConf := IRpDataFlashConfig(TObject.Create);
//    try
//      if not FSharedClientTCP then
//      begin
//        FClientTCP := TRpDataFlashClientConnection.Create(nil);
//        FClientTCP.Server := lFileConf.ServerName;
//        FClientTCP.Port := lFileConf.ServerPort;
//        FClientTCP.FileTransfer.Port := lFileConf.FTPPort;
//        FClientTCP.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;
//        FClientTCP.EncryptionType := lFileConf.EncryptionType;
//        FClientTCP.CommunicationType := lFileConf.CommunicationType;
//        FClientTCP.ConvertLocalHostToIP := lFileConf.LocalHostToIP;
//      end;
//      if not FSharedClientREST then
//      begin
//        FClientREST := TRpDataFlashRESTClient.Create(nil);
//        FClientREST.Server := lFileConf.ServerName;
//        FClientREST.Port := lFileConf.RestPort;
//        FClientREST.FileTransfer.Port := lFileConf.FTPPort;
//        FClientREST.FileTransfer.Enabled := FClientREST.FileTransfer.Port > 0;
//        FClientREST.EncryptionType := lFileConf.EncryptionType;
//        FClientREST.CommunicationType := lFileConf.CommunicationType;
//        FClientREST.ConvertLocalHostToIP := lFileConf.LocalHostToIP;
//      end;
//    finally
//      lFileConf := nil;
//    end;
  end;
end;

destructor TProxyFactory.Destroy;
begin
  if Assigned(FProxyHARD_CODE) then
    FreeAndNil(FProxyHARD_CODE);
  if Assigned(FProxyMathAndText) then
    FreeAndNil(FProxyMathAndText);
  if Assigned(FProxyFileManager) then
    FreeAndNil(FProxyFileManager);
  if Assigned(FClientREST) and (not FSharedClientREST) then
    FreeAndNil(FClientREST);
  if Assigned(FClientTCP) and (not FSharedClientTCP) then
  begin
    try
      if FClientTCP.Connected then
        FClientTCP.Disconnect;
    finally
      FreeAndNil(FClientTCP);
    end;
  end;
  inherited;
end;

function TProxyFactory.GetLazyConection: Boolean;
begin
  Result := False;
  if FConnectionType = ctTcpIp then
  begin
    if FClientTCP <> nil then
      Result := FClientTCP.LazyConnection;
  end else if FClientREST <> nil then
    Result := FClientREST.LazyConnection;
end;
 
procedure TProxyFactory.SetLazyConection(const Value: Boolean);
begin
  if FConnectionType = ctTcpIp then
  begin
    if FClientTCP <> nil then
      FClientTCP.LazyConnection := Value;
  end else if FClientREST <> nil then
    FClientREST.LazyConnection := Value;
end;
 
procedure TProxyFactory.Config(const AHost: string; const APorta, APortaFTP: Integer;
  const AConnectionType : TConnectionType;
  const ALocalHostToIP : Boolean;
  const ATipoCriptografia : TRpDataFlashEncryptionType;
  const ATipoComunicacao : TRpDataFlashCommunicationType);
begin
  if AConnectionType = ctTcpIp then
  begin
    if FClientTCP <> nil then
    begin
      if FSharedClientTCP then
        FClientTCP.Disconnect
      else
        FreeAndNil(FClientTCP);
    end;

    if not FSharedClientTCP then
      FClientTCP := TRpDataFlashClientConnection.Create(nil);

    FClientTCP.Server := AHost;
    FClientTCP.Port := APorta;
    FClientTCP.EncryptionType := ATipoCriptografia;
    FClientTCP.CommunicationType := ATipoComunicacao;
    FClientTCP.FileTransfer.Port := APortaFTP;
    FClientTCP.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;
    FClientTCP.ConvertLocalHostToIP := ALocalHostToIP;
  end else
  begin
    if (FClientREST <> nil) and (not FSharedClientREST) then
      FreeAndNil(FClientREST);

    if not FSharedClientREST then
      FClientREST := TRpDataFlashRESTClient.Create(nil);

    FClientREST.Server := AHost;
    FClientREST.Port := APorta;
    FClientREST.EncryptionType := ATipoCriptografia;
    FClientREST.CommunicationType := ATipoComunicacao;
    FClientREST.FileTransfer.Port := APortaFTP;
    FClientREST.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;
    FClientREST.ConvertLocalHostToIP := ALocalHostToIP;
  end;
end;

function TProxyFactory.ServerOnline: Boolean;
begin
  Result := FClientTCP.ServerOnline;
end;

procedure TProxyFactory.DoCheckBusy;
begin
  if FCheckBusy then
  begin
    while FBusy do
      Sleep(500);
  end;
end;

function TProxyFactory.HARD_CODE: TProxyHARD_CODE;
begin
  if FProxyHARD_CODE = nil then
  begin
    if FConnectionType = ctTcpIp then
      FProxyHARD_CODE := TProxyHARD_CODE.Create(FClientTCP, BusyCallback, FSharedClientTCP)
    else
      FProxyHARD_CODE := TProxyHARD_CODE.Create(FClientREST, BusyCallback, FSharedClientREST);
    FProxyHARD_CODE.Reconnect := FReconnect;
  end;
  FProxyHARD_CODE.SerializationFormat := FSerializationFormat;
  DoCheckBusy;
  Result := FProxyHARD_CODE;
end;

function TProxyFactory.MathAndText: TProxyMathAndText;
begin
  if FProxyMathAndText = nil then
  begin
    if FConnectionType = ctTcpIp then
      FProxyMathAndText := TProxyMathAndText.Create(FClientTCP, BusyCallback, FSharedClientTCP)
    else
      FProxyMathAndText := TProxyMathAndText.Create(FClientREST, BusyCallback, FSharedClientREST);
    FProxyMathAndText.Reconnect := FReconnect;
  end;
  FProxyMathAndText.SerializationFormat := FSerializationFormat;
  DoCheckBusy;
  Result := FProxyMathAndText;
end;

function TProxyFactory.FileManager: TProxyFileManager;
begin
  if FProxyFileManager = nil then
  begin
    if FConnectionType = ctTcpIp then
      FProxyFileManager := TProxyFileManager.Create(FClientTCP, BusyCallback, FSharedClientTCP)
    else
      FProxyFileManager := TProxyFileManager.Create(FClientREST, BusyCallback, FSharedClientREST);
    FProxyFileManager.Reconnect := FReconnect;
  end;
  FProxyFileManager.SerializationFormat := FSerializationFormat;
  DoCheckBusy;
  Result := FProxyFileManager;
end;

procedure TProxyFactory.Config(const AConexaoTCP: TRpDataFlashClientConnection);
begin
  ProxyFactory.Config(
    AConexaoTCP.Server,
    AConexaoTCP.Port,
    AConexaoTCP.FileTransfer.Port,
    ctTcpIp,
    AConexaoTCP.ConvertLocalHostToIP,
    AConexaoTCP.EncryptionType,
    AConexaoTCP.CommunicationType);
end;

procedure TProxyFactory.Config(const AConexaoREST: TRpDataFlashRESTClient);
begin
  ProxyFactory.Config(
    AConexaoREST.Server,
    AConexaoREST.Port,
    AConexaoREST.FileTransfer.Port,
    ctREST,
    AConexaoREST.ConvertLocalHostToIP,
    AConexaoREST.EncryptionType,
    AConexaoREST.CommunicationType);
end;
 
procedure TProxyFactory.BusyCallback(const AStart: Boolean);
begin
  FBusy := AStart;
end;

initialization

finalization
  if FProxyFactory <> nil then
    FProxyFactory.Free;
  if FProxyFactoryRest <> nil then
    FProxyFactoryRest.Free;

end.
