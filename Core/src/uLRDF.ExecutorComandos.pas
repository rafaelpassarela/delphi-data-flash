unit uLRDF.ExecutorComandos;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uLRDF.Component, uLRDF.Comando, uLRDF.Types, uLRDF.ComandController;

type
  TLRDFErrorExecucao = procedure(const AErrorMessage : string; var ARaiseException : Boolean) of object;
  TLRDFExecCmdBeforeExecute = procedure (Sender : TObject; var AContinue : Boolean) of object;
  TLRDFExecCmdAfterExecute = procedure (Sender : TObject; const AExecOk : Boolean; const AResultMessage : string) of object;

  TLRDataFlashExecutorComando = class(TComponent)
  private
    FConexaoCliente: TLRDataFlashConexaoCliente;
    FVerificarConexao: Boolean;
    FComando: string;
    FLastError: string;
    FOnError: TLRDFErrorExecucao;
    FLastStatusProcessamento : TLRDataFlashStatusProcessamento;
    FParametros: TLRDataFlashParametrosValueCollection;
    FRetornos: TLRDataFlashRetornosValueCollection;
    FOnAfterExecute: TLRDFExecCmdAfterExecute;
    FOnBeforeExecute: TLRDFExecCmdBeforeExecute;

    function DoInternalExecute : Boolean;
    function GetComando: string;
    procedure SetComando(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute : Boolean;
    function LastError : string;
    function LastStatusProcessamento : TLRDataFlashStatusProcessamento;
  published
    property ConexaoCliente : TLRDataFlashConexaoCliente read FConexaoCliente write FConexaoCliente;
    property VerificarConexao : Boolean read FVerificarConexao write FVerificarConexao default True;
    property Comando : string read GetComando write SetComando;
    property Parametros : TLRDataFlashParametrosValueCollection read FParametros write FParametros;
    property Retornos : TLRDataFlashRetornosValueCollection read FRetornos write FRetornos;

    property OnError : TLRDFErrorExecucao read FOnError write FOnError;
    property OnAfterExecute : TLRDFExecCmdAfterExecute read FOnAfterExecute write FOnAfterExecute;
    property OnBeforeExecute : TLRDFExecCmdBeforeExecute read FOnBeforeExecute write FOnBeforeExecute;
  end;

implementation

{ TLRDataFlashExecutorComando }

constructor TLRDataFlashExecutorComando.Create(AOwner: TComponent);
begin
  inherited;

  FVerificarConexao := True;
  FComando := '';
  FLastStatusProcessamento := tspNenhum;

  FParametros := TLRDataFlashParametrosValueCollection.Create(Self, TLRDataFlashParametroValueItem);
  FRetornos := TLRDataFlashRetornosValueCollection.Create(Self, TLRDataFlashParametroValueItem);
end;

destructor TLRDataFlashExecutorComando.Destroy;
begin
  FreeAndNil(FParametros);
  FreeAndNil(FRetornos);
  inherited;
end;

function TLRDataFlashExecutorComando.DoInternalExecute: Boolean;
var
  lCmd : TLRDataFlashComandoEnvio;
  i: Integer;
  lParam: TLRDataFlashParametroValueItem;
begin
  lCmd := TLRDataFlashComandoEnvio.Create;
  try
    lCmd.SetComando( FComando );

    // copia os parametros do componente para o do comando
    for i := 0 to Parametros.Count - 1 do
    begin
      lParam := Parametros.ByIndex(i);
      if lParam.TipoValor in [tvpBase, tvpBase64, tvpDAO, tvpFile] then
      begin
        lCmd.Parametros.AddParametro(lParam.Nome, ' ', lParam.TipoValor);
        lCmd.Parametro[lParam.Nome].AsBase64 := lParam.Valor;
      end
      else
        lCmd.Parametros.AddParametro(lParam.Nome, lParam.Valor, lParam.TipoValor);
    end;

    FConexaoCliente.Comunicar( lCmd );
    Result := lCmd.StatusRetorno;
    FLastError := lCmd.LastError;
    FLastStatusProcessamento := lCmd.StatusProcessamento;

    // copia os parametros do comando para o componente
    for i := 0 to Retornos.Count - 1 do
    begin
      lParam := Retornos.ByIndex(i);
      case lParam.TipoValor of
        tvpInteger:  lParam.Valor := lCmd.Retorno[lParam.Nome].AsInteger;
        tvpString:   lParam.Valor := lCmd.Retorno[lParam.Nome].AsString;
        tvpBoolean:  lParam.Valor := lCmd.Retorno[lParam.Nome].AsBoolean;
        tvpFloat:    lParam.Valor := lCmd.Retorno[lParam.Nome].AsFloat;
        tvpBase64:   lParam.Valor := lCmd.Retorno[lParam.Nome].AsBase64;
        tvpDateTime: lParam.Valor := lCmd.Retorno[lParam.Nome].AsDateTime;
        tvpFile:     lParam.Valor := lCmd.Retorno[lParam.Nome].AsBase64;
      end;
    end;

  finally
    FreeAndNil( lCmd );
  end;
end;

function TLRDataFlashExecutorComando.Execute: Boolean;
var
  lRaise: Boolean;
  lContinue: Boolean;
begin
  lRaise := True;
  lContinue := True;
  Result := False;
  FLastStatusProcessamento := tspNenhum;
  try
    if Assigned(FOnBeforeExecute) then
      FOnBeforeExecute(Self, lContinue);

    if lContinue then
    begin
      if not Assigned(FConexaoCliente) then
        raise ELRDataFlashConexaoInvalida.Create('Conexão cliente não foi configurada.');

      if FVerificarConexao and (not FConexaoCliente.Conectado) then
        raise ELRDataFlashConexaoInvalida.Create('A conexão ainda não foi estabelecida.');

      if Trim(FComando) = EmptyStr then
        raise ELRDataFlashConexaoInvalida.Create('O comando não foi informado.');

      Result := DoInternalExecute;

      if Assigned(FOnAfterExecute) then
        FOnAfterExecute(Self, Result, FLastError);
    end;
  except
    on E: Exception do
    begin
      if Assigned(FOnError) then
        FOnError(E.Message, lRaise);

      if lRaise then
        raise Exception.Create( E.Message );
    end;
  end;
end;

function TLRDataFlashExecutorComando.GetComando: string;
begin
  Result := FComando;
end;

procedure TLRDataFlashExecutorComando.SetComando(const Value: string);
begin
  if FComando <> Value then
  begin
    FComando := Value;
  end;
end;

function TLRDataFlashExecutorComando.LastError: string;
begin
  Result := FLastError;
end;

function TLRDataFlashExecutorComando.LastStatusProcessamento: TLRDataFlashStatusProcessamento;
begin
  Result := FLastStatusProcessamento;
end;

end.
