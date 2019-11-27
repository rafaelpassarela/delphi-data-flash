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
  TRpDataFlashErrorExecucao = procedure(const AErrorMessage : string; var ARaiseException : Boolean) of object;
  TRpDataFlashExecCmdBeforeExecute = procedure (Sender : TObject; var AContinue : Boolean) of object;
  TRpDataFlashExecCmdAfterExecute = procedure (Sender : TObject; const AExecOk : Boolean; const AResultMessage : string) of object;

  TRpDataFlashCommandExecutor = class(TComponent)
  private
    FConexaoCliente: TRpDataFlashConexaoCliente;
    FVerificarConexao: Boolean;
    FComando: string;
    FLastError: string;
    FOnError: TRpDataFlashErrorExecucao;
    FLastStatusProcessamento : TRpDataFlashProcessingStatus;
    FParams: TRpDataFlashParametrosValueCollection;
    FResultParam: TRpDataFlashRetornosValueCollection;
    FOnAfterExecute: TRpDataFlashExecCmdAfterExecute;
    FOnBeforeExecute: TRpDataFlashExecCmdBeforeExecute;

    function DoInternalExecute : Boolean;
    function GetCommand: string;
    procedure SetCommand(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Execute : Boolean;
    function LastError : string;
    function LastStatusProcessamento : TRpDataFlashProcessingStatus;
  published
    property ConexaoCliente : TRpDataFlashConexaoCliente read FConexaoCliente write FConexaoCliente;
    property VerificarConexao : Boolean read FVerificarConexao write FVerificarConexao default True;
    property Command : string read GetCommand write SetCommand;
    property Params : TRpDataFlashParametrosValueCollection read FParams write FParams;
    property ResultParams : TRpDataFlashRetornosValueCollection read FResultParam write FResultParam;

    property OnError : TRpDataFlashErrorExecucao read FOnError write FOnError;
    property OnAfterExecute : TRpDataFlashExecCmdAfterExecute read FOnAfterExecute write FOnAfterExecute;
    property OnBeforeExecute : TRpDataFlashExecCmdBeforeExecute read FOnBeforeExecute write FOnBeforeExecute;
  end;

implementation

{ TRpDataFlashExecutorComando }

constructor TRpDataFlashCommandExecutor.Create(AOwner: TComponent);
begin
  inherited;

  FVerificarConexao := True;
  FComando := '';
  FLastStatusProcessamento := psNone;

  FParams := TRpDataFlashParametrosValueCollection.Create(Self, TRpDataFlashParametroValueItem);
  FResultParam := TRpDataFlashRetornosValueCollection.Create(Self, TRpDataFlashParametroValueItem);
end;

destructor TRpDataFlashCommandExecutor.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FResultParam);
  inherited;
end;

function TRpDataFlashCommandExecutor.DoInternalExecute: Boolean;
var
  i: Integer;
  lCmd : TRpDataFlashSendCommand;
  lParam: TRpDataFlashParametroValueItem;
begin
  lCmd := TRpDataFlashSendCommand.Create;
  try
    lCmd.SetCommand( FComando );

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
    for i := 0 to ResultParams.Count - 1 do
    begin
      lParam := ResultParams.ByIndex(i);
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

function TRpDataFlashCommandExecutor.Execute: Boolean;
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
        raise ERpDataFlashInvalidConnection.Create('Conex�o cliente n�o foi configurada.');

      if FVerificarConexao and (not FConexaoCliente.Conectado) then
        raise ERpDataFlashInvalidConnection.Create('A conex�o ainda n�o foi estabelecida.');

      if Trim(FComando) = EmptyStr then
        raise ERpDataFlashInvalidConnection.Create('O comando n�o foi informado.');

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

function TRpDataFlashCommandExecutor.GetCommand: string;
begin
  Result := FComando;
end;

procedure TRpDataFlashCommandExecutor.SetCommand(const Value: string);
begin
  if FComando <> Value then
  begin
    FComando := Value;
  end;
end;

function TRpDataFlashCommandExecutor.LastError: string;
begin
  Result := FLastError;
end;

function TRpDataFlashCommandExecutor.LastStatusProcessamento: TRpDataFlashProcessingStatus;
begin
  Result := FLastStatusProcessamento;
end;

end.
