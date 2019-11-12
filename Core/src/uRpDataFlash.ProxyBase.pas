unit uRpDataFlash.ProxyBase;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Components, uRpDataFlash.Command, uRpDataFlash.Protocol,
  SysUtils;

type
  TLRDataFlashComandoProxy = class(TRpDataFlashCommand)
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
    function Executar(const pTcpClient : TLRDataFlashConexaoCliente) : Boolean; reintroduce;
  end;

  TLRDataFlashComandoServer = class(TRpDataFlashCommand)
  protected
    procedure DoLoad; override;
    function DoExecute: Boolean; override;
  end;

implementation

uses
  StrUtils;

{ TLRDataFlashComandoProxy }

procedure TLRDataFlashComandoProxy.DoAfterExecute(const AResult: string);
begin

end;

procedure TLRDataFlashComandoProxy.DoBeforeExecute;
begin

end;

function TLRDataFlashComandoProxy.DoExecute: Boolean;
begin
  Result := True;
end;

function TLRDataFlashComandoProxy.Executar(const pTcpClient : TLRDataFlashConexaoCliente): Boolean;
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

function TLRDataFlashComandoProxy.GetCommand: string;
begin
  Result := Self.ClassName;
  if LowerCase(RightStr(Result, 5)) = 'proxy' then
    Delete(Result, Length(Result) - 4, 5)
  else
    if LowerCase(RightStr(Result, 7)) = 'adapter' then
      Delete(Result, Length(Result) - 6, 7);
end;

function TLRDataFlashComandoProxy.GetLastError: string;
begin
  Result := FLastError;
end;

procedure TLRDataFlashComandoProxy.OnErroEnvio(Sender: TObject;
  const AProtocolo: TRpDataFlashProtocol; const AException: Exception);
begin
  FLastError := AException.Message + AProtocolo.Message;
end;

{ TLRDataFlashComandoServer }

procedure TLRDataFlashComandoServer.DoLoad;
begin
// definir os parametros de rotorno para o cliente
end;

function TLRDataFlashComandoServer.DoExecute: Boolean;
begin
  Result := False;
end;

end.
