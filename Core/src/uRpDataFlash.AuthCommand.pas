unit uRpDataFlash.AuthCommand;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types, SysUtils;

type
  TLRDataFlashComandoAutenticar = class(TRpDataFlashCommand)
  protected
    function DoExecutar : Boolean; override;
    procedure DoRegisterParams; override;
    function GetProcessType: TRpDataFlashProcessType; override;
    procedure DoExecutarPonteInvalida(var AContinuar : Boolean); override;
    procedure DoExecutarPonteBemSucedida(var AContinuar : Boolean); override;
  public
    class function Autenticar(ATcpClient : TRpDataFlashCustomClientConnection;
      const AUsername, APassword : string; out AResultMSG : string) : Boolean;
  end;

implementation

{ TLRDataFlashComandoAutenticar }

class function TLRDataFlashComandoAutenticar.Autenticar(ATcpClient: TRpDataFlashCustomClientConnection;
  const AUsername, APassword : string; out AResultMSG: string): Boolean;
const
  C_ERRO_AUTENTICACAO = 'Erro enviando pedido de autenticação. ';
var
  lCmd : TLRDataFlashComandoAutenticar;
begin
  lCmd := TLRDataFlashComandoAutenticar.Create;
  try
    try
      lCmd.Param['Username'].AsString := AUsername;
      lCmd.Param['Password'].AsBase64 := APassword;

      ATcpClient.Comunicar(lCmd);
      Result := lCmd.ReturnStatus;
      if Result then
      begin
        Result := lCmd.ResultParam['Autenticado'].AsBoolean;
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

function TLRDataFlashComandoAutenticar.DoExecutar: Boolean;
var
  lAutenticado: Boolean;
  lMens: string;
begin
  if (GetServer <> nil) and Assigned(GetServer.OnAutenticarCliente) then
  begin
    if not Assigned(FConexaoItem) then
    begin
      Result := True;
      lAutenticado := False;
      lMens := 'Falha na autenticação. Item de conexão é inválido.';
    end
    else
      try
        FConexaoItem.Username := Param['Username'].AsString;
        FConexaoItem.Password := Param['Password'].AsBase64;
        GetServer.OnAutenticarCliente(GetServer, FConexaoItem, lAutenticado, lMens);

        FConexaoItem.Authenticated := lAutenticado;

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

procedure TLRDataFlashComandoAutenticar.DoExecutarPonteBemSucedida(var AContinuar: Boolean);
begin
  inherited;
  AContinuar := True;
  // atualiza os dados da conexao local (quando retorna da ponte [servidor real] )
  FConexaoItem.Username := Param['Username'].AsString;
  FConexaoItem.Password := Param['Password'].AsBase64;
  FConexaoItem.Authenticated := ResultParam['Authenticated'].AsBoolean;
end;

procedure TLRDataFlashComandoAutenticar.DoExecutarPonteInvalida(var AContinuar: Boolean);
begin
  inherited;
  FConexaoItem.Authenticated := False;
  AContinuar := False;
end;

procedure TLRDataFlashComandoAutenticar.DoRegisterParams;
begin
  inherited;
  NovoParametro('Username', tvpString);
  // envia a senha criptografada
  NovoParametro('Password', tvpBase64);

  NovoRetorno('Authenticated', tvpBoolean);
  NovoRetorno('ResultMSG', tvpString);
end;

function TLRDataFlashComandoAutenticar.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtRemoteOnly;
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoAutenticar, C_GROUP_INTERNAL);

end.
