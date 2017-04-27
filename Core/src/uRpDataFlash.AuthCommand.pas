unit uRpDataFlash.AuthCommand;

{$I ..\..\Common\src\RpInc.inc}

interface

uses uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types, SysUtils;

type
  TLRDataFlashComandoAutenticar = class(TLRDataFlashComando)
  protected
    function DoExecutar : Boolean; override;
    procedure DoRegistrarParametros; override;
    function GetTipoProcessamento : TLRDataFlashTipoProcessamento; override;
    procedure DoExecutarPonteInvalida(var AContinuar : Boolean); override;
    procedure DoExecutarPonteBemSucedida(var AContinuar : Boolean); override;
  public
    class function Autenticar(ATcpClient : TLRDataFlashConexaoClienteCustom;
      const AUsername, APassword : string; out AResultMSG : string) : Boolean;
  end;

implementation

{ TLRDataFlashComandoAutenticar }

class function TLRDataFlashComandoAutenticar.Autenticar(ATcpClient: TLRDataFlashConexaoClienteCustom;
  const AUsername, APassword : string; out AResultMSG: string): Boolean;
const
  C_ERRO_AUTENTICACAO = 'Erro enviando pedido de autenticação. ';
var
  lCmd : TLRDataFlashComandoAutenticar;
begin
  lCmd := TLRDataFlashComandoAutenticar.Create;
  try
    try
      lCmd.Parametro['Username'].AsString := AUsername;
      lCmd.Parametro['Password'].AsBase64 := APassword;

      ATcpClient.Comunicar(lCmd);
      Result := lCmd.StatusRetorno;
      if Result then
      begin
        Result := lCmd.Retorno['Autenticado'].AsBoolean;
        AResultMSG := lCmd.Retorno['ResultMSG'].AsString;
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
        FConexaoItem.Username := Parametro['Username'].AsString;
        FConexaoItem.Password := Parametro['Password'].AsBase64;
        GetServer.OnAutenticarCliente(GetServer, FConexaoItem, lAutenticado, lMens);

        FConexaoItem.Autenticado := lAutenticado;

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

  Retorno['Autenticado'].AsBoolean := lAutenticado;
  Retorno['ResultMSG'].AsString := lMens;
end;

procedure TLRDataFlashComandoAutenticar.DoExecutarPonteBemSucedida(var AContinuar: Boolean);
begin
  inherited;
  AContinuar := True;
  // atualiza os dados da conexao local (quando retorna da ponte [servidor real] )
  FConexaoItem.Username := Parametro['Username'].AsString;
  FConexaoItem.Password := Parametro['Password'].AsBase64;
  FConexaoItem.Autenticado := Retorno['Autenticado'].AsBoolean;
end;

procedure TLRDataFlashComandoAutenticar.DoExecutarPonteInvalida(var AContinuar: Boolean);
begin
  inherited;
  FConexaoItem.Autenticado := False;
  AContinuar := False;
end;

procedure TLRDataFlashComandoAutenticar.DoRegistrarParametros;
begin
  inherited;
  NovoParametro('Username', tvpString);
  // envia a senha criptografada
  NovoParametro('Password', tvpBase64);

  NovoRetorno('Autenticado', tvpBoolean);
  NovoRetorno('ResultMSG', tvpString);
end;

function TLRDataFlashComandoAutenticar.GetTipoProcessamento: TLRDataFlashTipoProcessamento;
begin
  Result := tprSomentePonte;
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoAutenticar, C_GRUPO_INTERNO);

end.
