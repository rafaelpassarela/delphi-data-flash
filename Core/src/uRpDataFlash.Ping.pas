unit uRpDataFlash.Ping;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types;

type
  TRpDataFlashPingCommand = class(TRpDataFlashCommand)
  protected
    function DoExecute : Boolean; override;
    procedure DoRegisterParams; override;
    function GetProcessType : TRpDataFlashProcessType; override;
  public
    class function Ping(ATcpClient : TRpDataFlashClientConnection; out AResultMSG : string) : Boolean;
  end;

implementation

uses
  SysUtils;

{ TRpDataFlashPingCommand }

function TRpDataFlashPingCommand.DoExecute: Boolean;
begin
  ResultParam['PinkOk'].AsBoolean := True;
  Result := True;
end;

procedure TRpDataFlashPingCommand.DoRegisterParams;
begin
  inherited;
  NewResult('PinkOk', tvpBoolean);
  NewResult('Msg', tvpString);
end;

function TRpDataFlashPingCommand.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtLocal;
end;

class function TRpDataFlashPingCommand.Ping(ATcpClient : TRpDataFlashClientConnection; out AResultMSG : string): Boolean;
var
  lCmd : TRpDataFlashPingCommand;
begin
  lCmd := TRpDataFlashPingCommand.Create;
  try
    try
      ATcpClient.Comunicar(lCmd);
      Result := lCmd.ResultParam['PinkOk'].AsBoolean;
      AResultMSG := lCmd.ResultParam['Msg'].AsString;
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
  TCPClassRegistrer.Registrate(TRpDataFlashPingCommand, C_GROUP_INTERNAL);

end.
