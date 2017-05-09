unit uRpDataFlash.Ping;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types;

type
  TLRDataFlashComandoPing = class(TRpDataFlashCommand)
  protected
    function DoExecutar : Boolean; override;
    procedure DoRegistrarParametros; override;
    function GetTipoProcessamento : TRpDataFlashProcessType; override;
  public
    class function Ping(ATcpClient : TLRDataFlashConexaoCliente; out AResultMSG : string) : Boolean;
  end;

implementation

uses
  SysUtils;

{ TLRDataFlashComandoPing }

function TLRDataFlashComandoPing.DoExecutar: Boolean;
begin
  Retorno['PinkOk'].AsBoolean := True;
  Result := True;
end;

procedure TLRDataFlashComandoPing.DoRegistrarParametros;
begin
  inherited;
  NovoRetorno('PinkOk', tvpBoolean);
  NovoRetorno('Msg', tvpString);
end;

function TLRDataFlashComandoPing.GetTipoProcessamento: TRpDataFlashProcessType;
begin
  Result := prtLocal;
end;

class function TLRDataFlashComandoPing.Ping(ATcpClient : TLRDataFlashConexaoCliente; out AResultMSG : string): Boolean;
var
  lCmd : TLRDataFlashComandoPing;
begin
  lCmd := TLRDataFlashComandoPing.Create;
  try
    try
      ATcpClient.Comunicar(lCmd);
      Result := lCmd.Retorno['PinkOk'].AsBoolean;
      AResultMSG := lCmd.Retorno['Msg'].AsString;
    except
      on E:Exception do
      begin
        Result := False;
        AResultMSG := E.Message;
      end;
    end;
  finally
    FreeAndNil(lCmd);
  end;
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoPing, C_GROUP_INTERNAL);

end.