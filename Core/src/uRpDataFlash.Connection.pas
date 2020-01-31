unit uRpDataFlash.Connection;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  IdTCPClient, Classes, uRpDataFlash.Types, IdExceptionCore, IdStack, SysUtils,
  IdHTTP, Controls, uRpDataFlash.Utils;

type
  TRpDataFlashConnectionHelperCustomClass = class of TRpDataFlashConnectionHelperCustom;

  TRpDataFlashConnectionHelperCustom = class
  private
    class var _LocalHostIP : string;
  private
    FDoConnect: TRpDataFlashOnConnectEvent;
    FDoDisconnect: TRpDataFlashOnConnectEvent;
    FOnConnect: TRpDataFlashOnConnectOnServer;
    FOnNoService: TRpDataFlashOnNoService;
    FNewException: TRpDataFlashOnExceptionHandler;
    FConector: TIdTCPClientCustom;
    FOwner: TComponent;
    FServidor: string;
    FPorta: Integer;
    FConfigurarConexao: Boolean;
    function ConfiguradorConexao : Boolean;
    function GetIsConectado: Boolean;
    function GetConector: TIdTCPClientCustom; virtual;
    function GetURL: string; virtual;
    procedure Reconectar(const AException : Exception);
    procedure Disconnect;
    procedure ConfigurarConector(const pConectionTimeout : Integer; const pReadTimeout : Integer); virtual; abstract;
    procedure InternalConectar; virtual;
    function GetServerName(const AConvert : Boolean; const AHost : string) : string;
  public
    constructor Create(AOwner : TComponent);

    procedure LiberarConector;
    procedure ReinicializarConector(const pHost : string;
      const pConfigurarConexao : Boolean; const pPort : Integer;
      const pConectionTimeout : Integer; const pReadTimeout : Integer;
      const pConvertLocalHost : Boolean);
    function Connect : Boolean;

    property Conector : TIdTCPClientCustom read GetConector;
    property IsConectado : Boolean read GetIsConectado;
    property OnConnect: TRpDataFlashOnConnectOnServer read FOnConnect write FOnConnect;
    property OnNoService: TRpDataFlashOnNoService read FOnNoService write FOnNoService;
    property DoConnect: TRpDataFlashOnConnectEvent read FDoConnect write FDoConnect;
    property DoDisconnect : TRpDataFlashOnConnectEvent read FDoDisconnect write FDoDisconnect;
    property NewException: TRpDataFlashOnExceptionHandler read FNewException write FNewException;
    property URL : string read GetURL;
  end;

  TRpDataFlashConnectionHelperTCP = class(TRpDataFlashConnectionHelperCustom)
  private
    function GetConector: TIdTCPClient; reintroduce;
    procedure ConfigurarConector(const pConectionTimeout : Integer;
      const pReadTimeout : Integer); override;
  public
    property Conector : TIdTCPClient read GetConector; // write SetConector;
  end;

  TRpDataFlashConnectionHelperREST = class(TRpDataFlashConnectionHelperCustom)
  private
    function GetConector: TIdHTTP; reintroduce;
    procedure ConfigurarConector(const pConectionTimeout : Integer;
      const pReadTimeout : Integer); override;
    procedure InternalConectar; override;
    function GetURL : string; override;
  public
    property Conector : TIdHTTP read GetConector; // write SetConector;
  end;

implementation

uses
  uRpDataFlash.DefinirConexao, uRpDataFlash.Components;

{ TRpDataFlashConnectionHelperCustom }

function TRpDataFlashConnectionHelperCustom.Connect: Boolean;
var
  lAutMessage: string;
begin
  Result := False;
  try
    if (FServidor = EmptyStr) or (FPorta <= 0) then
      raise ERpDataFlashInvalidConnection.Create('Servidor/Porta não foi informado.');

    InternalConectar;

    // se possui informacoes para autenticacao, valida no servidor
    if (TRpDataFlashCustomClientConnection(FOwner).UserName <> '') or (TRpDataFlashCustomClientConnection(FOwner).Password <> '') then
    begin
      Result := TRpDataFlashCustomClientConnection(FOwner).Autenticar(lAutMessage);

      if not Result then
      begin
        if lAutMessage = '' then
          lAutMessage := 'Usuário e Senha inválidos.';
        raise ERpDataFlashAuthError.Create(lAutMessage);
      end;
    end
    else
      Result := True;

    if Assigned(FOnConnect) then
      FOnConnect(Self, FServidor, FPorta);
  except
    on E: ERpDataFlashInvalidConnection do
      raise;

    on E: ERpDataFlashAuthError do
    begin
      // pode estar conectado, neste caso desconecta o cliente
      NewException(E);
      if IsConectado then
        Disconnect;
      raise;
    end;

    on E: Exception do
    begin
      if E.InheritsFrom(EIdPortRequired) or E.InheritsFrom(EIdSocketError) then
        Reconectar(E)
      else
      begin
        NewException(E);
        raise;
      end;
    end;
  end;
end;

function TRpDataFlashConnectionHelperCustom.ConfiguradorConexao: Boolean;
var
  lConfigurador: TfrmLRDataFlashDefinirConexao;
begin
  lConfigurador := TfrmLRDataFlashDefinirConexao.Create(nil);
  try
    lConfigurador.Server := FServidor;
    lConfigurador.Porta := FPorta;
    Result := lConfigurador.ShowModal = mrOk;
    if Result then
    begin
      FServidor := lConfigurador.Server;
      FPorta := lConfigurador.Porta;
      // configura a conexão original
      TRpDataFlashCustomClientConnection(FOwner).Server := FServidor;
      TRpDataFlashCustomClientConnection(FOwner).Port := FPorta;
    end;
  finally
    FreeAndNil(lConfigurador);
  end;
end;

constructor TRpDataFlashConnectionHelperCustom.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
end;

procedure TRpDataFlashConnectionHelperCustom.Disconnect;
begin
  if Assigned(FDoDisconnect) then
    FDoDisconnect; // Desconectar que está no client
end;

function TRpDataFlashConnectionHelperCustom.GetConector: TIdTCPClientCustom;
begin
  Result := FConector;
end;

function TRpDataFlashConnectionHelperCustom.GetIsConectado: Boolean;
begin
  Result := (FConector <> nil)
        and FConector.Connected;
end;

function TRpDataFlashConnectionHelperCustom.GetServerName(const AConvert: Boolean;
  const AHost: string): string;
begin
  if AConvert and ((UpperCase(AHost) = 'LOCALHOST') or (AHost = '127.0.0.1')) then
  begin
    if TRpDataFlashConnectionHelperCustom._LocalHostIP = EmptyStr then
      TRpDataFlashConnectionHelperCustom._LocalHostIP := TRpDataFlashUtils.GetLocalComputerIp;
    Result := TRpDataFlashConnectionHelperCustom._LocalHostIP;
  end else
    Result := AHost;
end;

function TRpDataFlashConnectionHelperCustom.GetURL: string;
begin
  Result := Format('%s:%d', [FServidor, FPorta]);
end;

procedure TRpDataFlashConnectionHelperCustom.InternalConectar;
begin
  FConector.Connect;
end;

procedure TRpDataFlashConnectionHelperCustom.LiberarConector;
begin
  if FConector <> nil then
    FreeAndNil(FConector);
end;

procedure TRpDataFlashConnectionHelperCustom.Reconectar(const AException: Exception);
var
  lReconectar: Boolean;
begin
  lReconectar := FConfigurarConexao
             and ConfiguradorConexao;

  if lReconectar then
    DoConnect // DoConectar que esta no Client
  else
  begin
    if Assigned(FOnNoService) then
      FOnNoService(Self, FServidor, FPorta, AException, lReconectar);

    if lReconectar then
      DoConnect // DoConectar que esta no Client
  end;
end;

procedure TRpDataFlashConnectionHelperCustom.ReinicializarConector(const pHost : string;
  const pConfigurarConexao : Boolean; const pPort : Integer; const pConectionTimeout : Integer;
  const pReadTimeout : Integer; const pConvertLocalHost : Boolean);
begin
  FServidor := GetServerName(pConvertLocalHost, pHost);
  FPorta := pPort;
  FConfigurarConexao := pConfigurarConexao;

  LiberarConector;
  ConfigurarConector(pConectionTimeout, pReadTimeout);
end;

{ TRpDataFlashConnectionHelperTCP }

procedure TRpDataFlashConnectionHelperTCP.ConfigurarConector(
  const pConectionTimeout, pReadTimeout: Integer);
begin
  FConector := TIdTCPClient.Create(FOwner);
  Conector.Host := FServidor;
  Conector.Port := FPorta;
  Conector.ConnectTimeout := pConectionTimeout;
  Conector.ReadTimeout := pReadTimeout;
end;

function TRpDataFlashConnectionHelperTCP.GetConector: TIdTCPClient;
begin
  Result := TIdTCPClient(FConector);
//  Result.IOHandler.LargeStream := True;
end;

{ TRpDataFlashConnectionHelperREST }

procedure TRpDataFlashConnectionHelperREST.ConfigurarConector(
  const pConectionTimeout, pReadTimeout: Integer);
begin
  FConector := TIdHTTP.Create(FOwner);
  Conector.ConnectTimeout := pConectionTimeout;
  Conector.ReadTimeout := pReadTimeout;
  Conector.Request.ContentType := C_REST_CONTENT_TYPE_DELPHI;
end;

function TRpDataFlashConnectionHelperREST.GetConector: TIdHTTP;
begin
  Result := TIdHTTP(FConector);
end;

function TRpDataFlashConnectionHelperREST.GetURL: string;
begin
  Result := 'http://' + inherited GetURL;
end;

procedure TRpDataFlashConnectionHelperREST.InternalConectar;
begin
//HTTP client don't connect
end;

initialization
  TRpDataFlashConnectionHelperCustom._LocalHostIP := EmptyStr;

end.
