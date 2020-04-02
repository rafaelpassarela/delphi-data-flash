unit uRpDataFlash.ExecutorCommandList;

interface

uses
  DesignEditors, Classes, DesignIntf, uRpDataFlash.Command, Dialogs, SysUtils,
  uRpDataFlash.CommandExecutor, uRpDataFlash.Components, uRpDataFlash.Types,
  uRpDataFlash.CommandController;

type
  TRpDataFlashExecutorComandoList = class(TPropertyEditor)
  protected
    procedure CarregaParametros; virtual;
    function CanAddProxyName(const AProxyName : string) : Boolean; virtual;
    function GetConexaoCliente : TRpDataFlashClientConnection; virtual;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;
    function GetAttributes: TPropertyAttributes; override;

    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  uRpDataFlash.ProxyGenerator,
  uRpDataFlash.GetCommandList,
  uRpSerialization;

{ TRpDataFlashExecutorComandoList }

function TRpDataFlashExecutorComandoList.CanAddProxyName(
  const AProxyName: string): Boolean;
begin
  Result := True;
end;

procedure TRpDataFlashExecutorComandoList.CarregaParametros;
var
  lCmdList: TRpDataFlashComandoList;
  lLista: TStringList;
  lClient: TRpDataFlashClientConnection;
  lInfo: TRpDataFlashInfoCommand;
  i: Integer;

  procedure AdicionarParametro(const AParamXML : TRpDataFlashParamInfoComando);
  var
    lParam: TRpDataFlashParametroValueItem;
  begin
    if AParamXML.Tipo in [tpInput, tpInputNoReload] then   // tpSaida, tpInterno,
      lParam := (GetComponent(0) as TRpDataFlashCommandExecutor).Params.Add as TRpDataFlashParametroValueItem
    else
      if AParamXML.Tipo = tpOutput then
        lParam := (GetComponent(0) as TRpDataFlashCommandExecutor).ResultParams.Add as TRpDataFlashParametroValueItem
      else
        lParam := nil;

    if Assigned(lParam) then
    begin
      lParam.Name := AParamXML.Nome;
      lParam.ParamDataType := AParamXML.TipoValor;
    end;
  end;

begin
  SerializationClassRegistrer.Registrate(TRpDataFlashParamInfoComando);

  inherited;
  lInfo := nil;
  lLista := TStringList.Create;
  lClient := TRpDataFlashClientConnection.Create(nil);

  lCmdList := TRpDataFlashComandoList.Create;

  with (GetComponent(0) as TRpDataFlashCommandExecutor) do
  try
    lCmdList.Param['TipoBusca'].AsInteger := Ord(trpCommandInfo);
    lCmdList.Param['InfoString'].AsBase64 := Command;

    if Assigned(ConexaoCliente) then
    begin
      try
        lClient.ClonarDe( ConexaoCliente );
        lClient.Comunicar(lCmdList);
        if not lCmdList.ReturnStatus then
          ShowMessage('Não foi possível comunica com o servidor. ' + lCmdList.LastError)
        else
        begin
          lLista.Text := lCmdList.ResultParam['RetornoProxy'].AsBase64;
          if Trim(lLista.Text) <> EmptyStr then
          begin
            lInfo := TRpDataFlashInfoCommand.Create(nil);
            lInfo.LoadFromString( lLista.Text );

            for i := 0 to lInfo.ListaParametros.Count - 1 do
              AdicionarParametro( lInfo.ListaParametros[i] );
          end
          else
            ShowMessage('Comando "' + Command + '" não foi encontrado no servidor.');
        end;
      except
        on E: Exception do
        begin
          lClient.Disconnect;
          ShowMessage('Erro verificando lista de parâmetros. ' + E.Message);
        end;
      end;
    end
    else
      ShowMessage('Conexão cliente não está configurada!');
  finally
    FreeAndNil( lInfo );
    FreeAndNil( lCmdList );
    FreeAndNil( lLista );
    FreeAndNil( lClient );
//    if Assigned(TXMLRegister.RegistroBase) and (TXMLRegister.RegistroBase.Count = 1) then
//      FreeAndNil( TXMLRegister.RegistroBase );
  end;
end;

constructor TRpDataFlashExecutorComandoList.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
end;

destructor TRpDataFlashExecutorComandoList.Destroy;
begin
  inherited Destroy;
end;

function TRpDataFlashExecutorComandoList.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TRpDataFlashExecutorComandoList.GetConexaoCliente: TRpDataFlashClientConnection;
begin
  Result := (GetComponent(0) as TRpDataFlashCommandExecutor).ConexaoCliente;
end;

function TRpDataFlashExecutorComandoList.GetValue: string;
begin
  Result := (GetComponent(0) as TRpDataFlashCommandExecutor).Command;
end;

procedure TRpDataFlashExecutorComandoList.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  lCmdList: TRpDataFlashComandoList;
  lLista: TStringList;
  lClient: TRpDataFlashClientConnection;
  lConexCliente: TRpDataFlashClientConnection;
begin
  inherited;
  // obtem lista de comandos registrados
  lLista := TStringList.Create;
  lClient := TRpDataFlashClientConnection.Create(nil);

  lCmdList := TRpDataFlashComandoList.Create;

  try
    lCmdList.Param['TipoBusca'].AsInteger := Ord(trpCommandList);
    lConexCliente := GetConexaoCliente;
    if Assigned(lConexCliente) then
    begin
      try
        lClient.ClonarDe(lConexCliente);
        lClient.Comunicar(lCmdList);
        lCmdList.ReturnStatus;

        lLista.Text := lCmdList.ResultParam['RetornoProxy'].AsBase64;
        for i := 0 to lLista.Count - 1 do
          if CanAddProxyName(lLista[i]) then
            Proc( lLista[i] );
      except
        on E: Exception do
        begin
          lClient.Disconnect;
          ShowMessage('Erro verificando lista de comandos. ' + E.Message);
        end;
      end;
    end
    else
      ShowMessage('Conexão cliente não está configurada');
  finally
    FreeAndNil( lCmdList );
    FreeAndNil( lLista );
    FreeAndNil( lClient );
  end;
end;

procedure TRpDataFlashExecutorComandoList.SetValue(const Value: string);
begin
  inherited;

  with (GetComponent(0) as TRpDataFlashCommandExecutor) do
  begin
    if Command <> Value then
    begin
      // configura o novo valor para o componente
      Command := Value;
      // limpa os parametros atuais
      Params.Clear;
      ResultParams.Clear;

      if Value <> EmptyStr then
        CarregaParametros;
    end;
  end;
end;

end.
