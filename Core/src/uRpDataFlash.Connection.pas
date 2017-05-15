unit uRpDataFlash.Connection;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  IdTCPClient, Classes, uRpDataFlash.Types, IdExceptionCore, IdStack, SysUtils,
  IdHTTP, uRpDataFlash.Utils;

type
  TLRDataFlashConnectionHelperCustomClass = class of TLRDataFlashConnectionHelperCustom;

  TLRDataFlashConnectionHelperCustom = class
  private
    class var _LocalHostIP : string;
  private
    FDoConectar: TRpDataFlashOnConnectEvent;
    FDoDesconectar: TRpDataFlashOnConnectEvent;
    FAoConectar: TRpDataFlashOnConnectOnServer;
    FAoSemServico: TRpDataFlashOnNoService;
    FNovaExcecao: TRpDataFlashOnExceptionHandler;
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
    procedure Desconectar;
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
    function Conectar : Boolean;

    property Conector : TIdTCPClientCustom read GetConector;
    property IsConectado : Boolean read GetIsConectado;
    property AoConectar: TRpDataFlashOnConnectOnServer read FAoConectar write FAoConectar;
    property AoSemServico: TRpDataFlashOnNoService read FAoSemServico write FAoSemServico;
    property DoConectar: TRpDataFlashOnConnectEvent read FDoConectar write FDoConectar;
    property DoDesconectar : TRpDataFlashOnConnectEvent read FDoDesconectar write FDoDesconectar;
    property NovaExcecao: TRpDataFlashOnExceptionHandler read FNovaExcecao write FNovaExcecao;
    property URL : string read GetURL;
  end;

  TLRDataFlashConnectionHelperTCP = class(TLRDataFlashConnectionHelperCustom)
  private
    function GetConector: TIdTCPClient; reintroduce;
    procedure ConfigurarConector(const pConectionTimeout : Integer;
      const pReadTimeout : Integer); override;
  public
    property Conector : TIdTCPClient read GetConector; // write SetConector;
  end;

  TLRDataFlashConnectionHelperREST = class(TLRDataFlashConnectionHelperCustom)
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
  fLRDF.DefinirConexao, Controls, uRpDataFlash.Components;

{ TLRDataFlashConnectionHelper }

function TLRDataFlashConnectionHelperCustom.Conectar: Boolean;
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

    if Assigned(FAoConectar) then
      FAoConectar(Self, FServidor, FPorta);
  except
    on E: ERpDataFlashInvalidConnection do
      raise;

    on E: ERpDataFlashAuthError do
    begin
      // pode estar conectado, neste caso desconecta o cliente
      NovaExcecao(E);
      if IsConectado then
        Desconectar;
      raise;
    end;

    on E: Exception do
    begin
      if E.InheritsFrom(EIdPortRequired) or E.InheritsFrom(EIdSocketError) then
        Reconectar(E)
      else
      begin
        NovaExcecao(E);
        raise;
      end;
    end;
  end;
end;

function TLRDataFlashConnectionHelperCustom.ConfiguradorConexao: Boolean;
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
      TRpDataFlashCustomClientConnection(FOwner).Servidor := FServidor;
      TRpDataFlashCustomClientConnection(FOwner).Porta := FPorta;
    end;
  finally
    FreeAndNil(lConfigurador);
  end;
end;

constructor TLRDataFlashConnectionHelperCustom.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
end;

procedure TLRDataFlashConnectionHelperCustom.Desconectar;
begin
  if Assigned(FDoDesconectar) then
    FDoDesconectar; // Desconectar que está no client
end;

function TLRDataFlashConnectionHelperCustom.GetConector: TIdTCPClientCustom;
begin
  Result := FConector;
end;

function TLRDataFlashConnectionHelperCustom.GetIsConectado: Boolean;
begin
  Result := (FConector <> nil)
        and FConector.Connected;
end;

function TLRDataFlashConnectionHelperCustom.GetServerName(const AConvert: Boolean;
  const AHost: string): string;
begin
  if AConvert and ((UpperCase(AHost) = 'LOCALHOST') or (AHost = '127.0.0.1')) then
  begin
    if TLRDataFlashConnectionHelperCustom._LocalHostIP = EmptyStr then
      TLRDataFlashConnectionHelperCustom._LocalHostIP := TRpDataFlashUtils.GetLocalComputerIp;
    Result := TLRDataFlashConnectionHelperCustom._LocalHostIP;
  end else
    Result := AHost;
end;

function TLRDataFlashConnectionHelperCustom.GetURL: string;
begin
  Result := Format('%s:%d', [FServidor, FPorta]);
end;

procedure TLRDataFlashConnectionHelperCustom.InternalConectar;
begin
  FConector.Connect;
end;

procedure TLRDataFlashConnectionHelperCustom.LiberarConector;
begin
  if FConector <> nil then
    FreeAndNil(FConector);
end;

procedure TLRDataFlashConnectionHelperCustom.Reconectar(const AException: Exception);
var
  lReconectar: Boolean;
begin
  lReconectar := FConfigurarConexao
             and ConfiguradorConexao;

  if lReconectar then
    DoConectar // DoConectar que esta no Client
  else
  begin
    if Assigned(FAoSemServico) then
      FAoSemServico(Self, FServidor, FPorta, AException, lReconectar);

    if lReconectar then
      DoConectar // DoConectar que esta no Client
  end;
end;

procedure TLRDataFlashConnectionHelperCustom.ReinicializarConector(const pHost : string;
  const pConfigurarConexao : Boolean; const pPort : Integer; const pConectionTimeout : Integer;
  const pReadTimeout : Integer; const pConvertLocalHost : Boolean);
begin
  FServidor := GetServerName(pConvertLocalHost, pHost);
  FPorta := pPort;
  FConfigurarConexao := pConfigurarConexao;

  LiberarConector;
  ConfigurarConector(pConectionTimeout, pReadTimeout);
end;

{ TLRDataFlashConnectionHelperTCP }

procedure TLRDataFlashConnectionHelperTCP.ConfigurarConector(
  const pConectionTimeout, pReadTimeout: Integer);
begin
  FConector := TIdTCPClient.Create(FOwner);
  Conector.Host := FServidor;
  Conector.Port := FPorta;
  Conector.ConnectTimeout := pConectionTimeout;
  Conector.ReadTimeout := pReadTimeout;
end;

function TLRDataFlashConnectionHelperTCP.GetConector: TIdTCPClient;
begin
  Result := TIdTCPClient(FConector);
//  Result.IOHandler.LargeStream := True;
end;

{ TLRDataFlashConnectionHelperREST }

procedure TLRDataFlashConnectionHelperREST.ConfigurarConector(
  const pConectionTimeout, pReadTimeout: Integer);
begin
  FConector := TIdHTTP.Create(FOwner);
  Conector.ConnectTimeout := pConectionTimeout;
  Conector.ReadTimeout := pReadTimeout;
  Conector.Request.ContentType := C_REST_CONTENT_TYPE;
end;

function TLRDataFlashConnectionHelperREST.GetConector: TIdHTTP;
begin
  Result := TIdHTTP(FConector);
end;

function TLRDataFlashConnectionHelperREST.GetURL: string;
begin
  Result := 'http://' + inherited GetURL;
end;

procedure TLRDataFlashConnectionHelperREST.InternalConectar;
begin
//HTTP client don't connect
end;

initialization
  TLRDataFlashConnectionHelperCustom._LocalHostIP := EmptyStr;

end.
