unit uRpDataFlash.CommandExecutor;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.Classes, System.SysUtils,
  {$ELSE}
  Classes, SysUtils,
  {$ENDIF}
  uRpDataFlash.Components, uRpDataFlash.Command, uRpDataFlash.Types,
  uRpDataFlash.CommandController;

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
    FLastStatusProcessamento : TRpDataFlashProcessingStatus;
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
    function LastStatusProcessamento : TRpDataFlashProcessingStatus;
  published
    property ConexaoCliente : TLRDataFlashConexaoCliente read FConexaoCliente write FConexaoCliente;
    property VerificarConexao : Boolean read FVerificarConexao write FVerificarConexao default True;
    property Comando : string read GetComando write SetComando;
    property Params : TLRDataFlashParametrosValueCollection read FParametros write FParametros;
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
  FLastStatusProcessamento := psNone;

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
  i: Integer;
  lCmd : TRpDataFlashSendCommand;
  lParam: TLRDataFlashParametroValueItem;
begin
  lCmd := TRpDataFlashSendCommand.Create;
  try
    lCmd.SetComando( FComando );

    // copia os parametros do componente para o do comando
    for i := 0 to Params.Count - 1 do
    begin
      lParam := Params.ByIndex(i);
      if lParam.TipoValor in [tvpBase, tvpBase64, tvpDAO, tvpFile] then
      begin
        lCmd.Params.AddParam(lParam.Nome, ' ', lParam.TipoValor);
        lCmd.Param[lParam.Nome].AsBase64 := lParam.Valor;
      end
      else
        lCmd.Params.AddParam(lParam.Nome, lParam.Valor, lParam.TipoValor);
    end;

    FConexaoCliente.Comunicar( lCmd );
    Result := lCmd.ReturnStatus;
    FLastError := lCmd.LastError;
    FLastStatusProcessamento := lCmd.ProcessingStatus;

    // copia os parametros do comando para o componente
    for i := 0 to Retornos.Count - 1 do
    begin
      lParam := Retornos.ByIndex(i);
      case lParam.TipoValor of
        tvpInteger:  lParam.Valor := lCmd.ResultParam[lParam.Nome].AsInteger;
        tvpString:   lParam.Valor := lCmd.ResultParam[lParam.Nome].AsString;
        tvpBoolean:  lParam.Valor := lCmd.ResultParam[lParam.Nome].AsBoolean;
        tvpFloat:    lParam.Valor := lCmd.ResultParam[lParam.Nome].AsFloat;
        tvpBase64:   lParam.Valor := lCmd.ResultParam[lParam.Nome].AsBase64;
        tvpDateTime: lParam.Valor := lCmd.ResultParam[lParam.Nome].AsDateTime;
        tvpFile:     lParam.Valor := lCmd.ResultParam[lParam.Nome].AsBase64;
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
  FLastStatusProcessamento := psNone;
  try
    if Assigned(FOnBeforeExecute) then
      FOnBeforeExecute(Self, lContinue);

    if lContinue then
    begin
      if not Assigned(FConexaoCliente) then
        raise ERpDataFlashInvalidConnection.Create('Conexão cliente não foi configurada.');

      if FVerificarConexao and (not FConexaoCliente.Conectado) then
        raise ERpDataFlashInvalidConnection.Create('A conexão ainda não foi estabelecida.');

      if Trim(FComando) = EmptyStr then
        raise ERpDataFlashInvalidConnection.Create('O comando não foi informado.');

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

function TLRDataFlashExecutorComando.LastStatusProcessamento: TRpDataFlashProcessingStatus;
begin
  Result := FLastStatusProcessamento;
end;

end.
