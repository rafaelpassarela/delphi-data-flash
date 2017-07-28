unit uRpDataFlash.Ping;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types;

type
  TRpDataFlashPingCommand = class(TRpDataFlashCommand)
  protected
    function DoExecutar : Boolean; override;
    procedure DoRegisterParams; override;
    function GetProcessType : TRpDataFlashProcessType; override;
  public
    class function Ping(ATcpClient : TLRDataFlashConexaoCliente; out AResultMSG : string) : Boolean;
  end;

implementation

uses
  SysUtils;

{ TLRDataFlashComandoPing }

function TRpDataFlashPingCommand.DoExecutar: Boolean;
begin
  Retorno['PinkOk'].AsBoolean := True;
  Result := True;
end;

procedure TRpDataFlashPingCommand.DoRegisterParams;
begin
  inherited;
  NovoRetorno('PinkOk', tvpBoolean);
  NovoRetorno('Msg', tvpString);
end;

function TRpDataFlashPingCommand.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtLocal;
end;

class function TRpDataFlashPingCommand.Ping(ATcpClient : TLRDataFlashConexaoCliente; out AResultMSG : string): Boolean;
var
  lCmd : TRpDataFlashPingCommand;
begin
  lCmd := TRpDataFlashPingCommand.Create;
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
  TCPClassRegistrer.Registrar(TRpDataFlashPingCommand, C_GROUP_INTERNAL);

end.
