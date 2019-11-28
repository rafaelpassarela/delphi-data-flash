unit uRpDataFlash.AuthCommand;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types, SysUtils;

type
  TRpDataFlashComandoAutenticar = class(TRpDataFlashCommand)
  protected
    function DoExecute : Boolean; override;
    procedure DoRegisterParams; override;
    function GetProcessType: TRpDataFlashProcessType; override;
    procedure DoExecuteBridgeError(var AContinue : Boolean); override;
    procedure DoExecuteBridgeSuccessfully(var AContinue : Boolean); override;
  public
    class function Autenticar(ATcpClient : TRpDataFlashCustomClientConnection;
      const AUsername, APassword : string; out AResultMSG : string) : Boolean;
  end;

implementation

{ TRpDataFlashComandoAutenticar }

class function TRpDataFlashComandoAutenticar.Autenticar(ATcpClient: TRpDataFlashCustomClientConnection;
  const AUsername, APassword : string; out AResultMSG: string): Boolean;
const
  C_ERRO_AUTENTICACAO = 'Erro enviando pedido de autenticação. ';
var
  lCmd : TRpDataFlashComandoAutenticar;
begin
  lCmd := TRpDataFlashComandoAutenticar.Create;
  try
    try
      lCmd.Param['Username'].AsString := AUsername;
      lCmd.Param['Password'].AsBase64 := APassword;

      ATcpClient.Comunicar(lCmd);
      Result := lCmd.ReturnStatus;
      if Result then
      begin
        Result := lCmd.ResultParam['Authenticated'].AsBoolean;
        AResultMSG := lCmd.ResultParam['ResultMSG'].AsString;
      end
      else
        AResultMSG := C_ERRO_AUTENTICACAO + lCmd.LastError;
    except
      on E: Exception do
      begin
        Result := False;
        AResultMSG := C_ERRO_AUTENTICACAO + E.Message;
      end;
    end;
  finally
    FreeAndNil(lCmd);
  end;
end;

function TRpDataFlashComandoAutenticar.DoExecute: Boolean;
var
  lAutenticado: Boolean;
  lMens: string;
begin
  if (GetServer <> nil) and Assigned(GetServer.OnAuthenticateClient) then
  begin
    if not Assigned(FConnectionItem) then
    begin
      Result := True;
      lAutenticado := False;
      lMens := 'Falha na autenticação. Item de conexão é inválido.';
    end
    else
      try
        FConnectionItem.Username := Param['Username'].AsString;
        FConnectionItem.Password := Param['Password'].AsBase64;
        GetServer.OnAuthenticateClient(GetServer, FConnectionItem, lAutenticado, lMens);

        FConnectionItem.Authenticated := lAutenticado;

        Result := True;
      except
        on E: Exception do
        begin
          Result := True;
          lAutenticado := False;
          lMens := 'Falha na autenticação. ' + E.Message;
        end;
      end;
  end
  else
  begin
    Result := True;
    lAutenticado := True;
    lMens := '';
  end;

  ResultParam['Authenticated'].AsBoolean := lAutenticado;
  ResultParam['ResultMSG'].AsString := lMens;
end;

procedure TRpDataFlashComandoAutenticar.DoExecuteBridgeSuccessfully(var AContinue: Boolean);
begin
  inherited;
  AContinue := True;
  // atualiza os dados da conexao local (quando retorna da ponte [servidor real] )
  FConnectionItem.Username := Param['Username'].AsString;
  FConnectionItem.Password := Param['Password'].AsBase64;
  FConnectionItem.Authenticated := ResultParam['Authenticated'].AsBoolean;
end;

procedure TRpDataFlashComandoAutenticar.DoExecuteBridgeError(var AContinue: Boolean);
begin
  inherited;
  FConnectionItem.Authenticated := False;
  AContinue := False;
end;

procedure TRpDataFlashComandoAutenticar.DoRegisterParams;
begin
  inherited;
  NewParam('Username', tvpString);
  // envia a senha criptografada
  NewParam('Password', tvpBase64);

  NewResult('Authenticated', tvpBoolean);
  NewResult('ResultMSG', tvpString);
end;

function TRpDataFlashComandoAutenticar.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtRemoteOnly;
end;

initialization
  TCPClassRegistrer.Registrar(TRpDataFlashComandoAutenticar, C_GROUP_INTERNAL);

end.
