unit uRpDataFlash.ProxyBase;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Components, uRpDataFlash.Command, uRpDataFlash.Protocol,
  SysUtils;

type
  TRpDataFlashComandoProxy = class(TRpDataFlashCommand)
  protected
    FLastError : string;
    function GetCommand: string; override;
    procedure DoBeforeExecute; virtual;
    procedure DoAfterExecute(const AResult : string); virtual;
    function DoExecute : Boolean; override;
  private
    procedure OnErroEnvio(Sender: TObject; const AProtocolo: TRpDataFlashProtocol; const AException: Exception);
  public
    function GetLastError : string;
    function Executar(const pTcpClient : TRpDataFlashConexaoCliente) : Boolean; reintroduce;
  end;

  TRpDataFlashComandoServer = class(TRpDataFlashCommand)
  protected
    procedure DoLoad; override;
    function DoExecute: Boolean; override;
  end;

implementation

uses
  StrUtils;

{ TRpDataFlashComandoProxy }

procedure TRpDataFlashComandoProxy.DoAfterExecute(const AResult: string);
begin

end;

procedure TRpDataFlashComandoProxy.DoBeforeExecute;
begin

end;

function TRpDataFlashComandoProxy.DoExecute: Boolean;
begin
  Result := True;
end;

function TRpDataFlashComandoProxy.Executar(const pTcpClient : TRpDataFlashConexaoCliente): Boolean;
var
  lStrResult: string;
begin
  FLastError := EmptyStr;
  try
    DoBeforeExecute;
    pTcpClient.AoErroEnvio := OnErroEnvio;
    lStrResult := pTcpClient.Comunicar(Self, Params);
    DoAfterExecute(lStrResult);
    Result := True;
  except
    Result := False;
  end;
  pTcpClient.AoErroEnvio := nil;
end;

function TRpDataFlashComandoProxy.GetCommand: string;
begin
  Result := Self.ClassName;
  if LowerCase(RightStr(Result, 5)) = 'proxy' then
    Delete(Result, Length(Result) - 4, 5)
  else
    if LowerCase(RightStr(Result, 7)) = 'adapter' then
      Delete(Result, Length(Result) - 6, 7);
end;

function TRpDataFlashComandoProxy.GetLastError: string;
begin
  Result := FLastError;
end;

procedure TRpDataFlashComandoProxy.OnErroEnvio(Sender: TObject;
  const AProtocolo: TRpDataFlashProtocol; const AException: Exception);
begin
  FLastError := AException.Message + AProtocolo.Message;
end;

{ TRpDataFlashComandoServer }

procedure TRpDataFlashComandoServer.DoLoad;
begin
// definir os parametros de rotorno para o cliente
end;

function TRpDataFlashComandoServer.DoExecute: Boolean;
begin
  Result := False;
end;

end.
