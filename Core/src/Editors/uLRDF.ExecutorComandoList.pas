unit uLRDF.ExecutorComandoList;

interface

uses
  DesignEditors, Classes, DesignIntf, uLRDF.Comando, Dialogs, SysUtils,
  uLRDF.ExecutorComandos, uLRDF.Component, uLRDF.Types, uLRDF.ComandController;

type
  TLRDataFlashExecutorComandoList = class(TPropertyEditor)
  protected
    procedure CarregaParametros; virtual;
    function CanAddProxyName(const AProxyName : string) : Boolean; virtual;
    function GetConexaoCliente : TLRDataFlashConexaoCliente; virtual;
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
  uLRDF.ProxyGenerator,
  uLRDF.ComandoGetCommandList,
  uRpFileHelper;

{ TLRDataFlashExecutorComandoList }

function TLRDataFlashExecutorComandoList.CanAddProxyName(
  const AProxyName: string): Boolean;
begin
  Result := True;
end;

procedure TLRDataFlashExecutorComandoList.CarregaParametros;
var
  lCmdList: TLRDataFlashComandoList;
  lLista: TStringList;
  lClient: TLRDataFlashConexaoCliente;
  lInfo: TLRDFInfoComando;
  i: Integer;

  procedure AdicionarParametro(const AParamXML : TLRDFParametrosInfoComando);
  var
    lParam: TLRDataFlashParametroValueItem;
  begin
    if AParamXML.Tipo in [tpEntrada, tpEntradaSemRecaregar] then   // tpSaida, tpInterno,
      lParam := (GetComponent(0) as TLRDataFlashExecutorComando).Parametros.Add as TLRDataFlashParametroValueItem
    else
      if AParamXML.Tipo = tpSaida then
        lParam := (GetComponent(0) as TLRDataFlashExecutorComando).Retornos.Add as TLRDataFlashParametroValueItem
      else
        lParam := nil;

    if Assigned(lParam) then
    begin
      lParam.Nome := AParamXML.Nome;
      lParam.TipoValor := AParamXML.TipoValor;
    end;
  end;

begin
  FileClassRegistrer.Registrar(TLRDFParametrosInfoComando);

  inherited;
  lInfo := nil;
  lLista := TStringList.Create;
  lClient := TLRDataFlashConexaoCliente.Create(nil);

  lCmdList := TLRDataFlashComandoList.Create;

  with (GetComponent(0) as TLRDataFlashExecutorComando) do
  try
    lCmdList.Parametro['TipoBusca'].AsInteger := Ord(trpCommandInfo);
    lCmdList.Parametro['InfoString'].AsBase64 := Comando;

    if Assigned(ConexaoCliente) then
    begin
      try
        lClient.ClonarDe( ConexaoCliente );
        lClient.Comunicar(lCmdList);
        if not lCmdList.StatusRetorno then
          ShowMessage('Não foi possível comunica com o servidor. ' + lCmdList.LastError)
        else
        begin
          lLista.Text := lCmdList.Retorno['RetornoProxy'].AsBase64;
          if Trim(lLista.Text) <> EmptyStr then
          begin
            lInfo := TLRDFInfoComando.Create(nil);
            lInfo.LoadFromString( lLista.Text );

            for i := 0 to lInfo.ListaParametros.Count - 1 do
              AdicionarParametro( lInfo.ListaParametros[i] );
          end
          else
            ShowMessage('Comando "' + Comando + '" não foi encontrado no servidor.');
        end;
      except
        on E: Exception do
        begin
          lClient.Desconectar;
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

constructor TLRDataFlashExecutorComandoList.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
end;

destructor TLRDataFlashExecutorComandoList.Destroy;
begin
  inherited Destroy;
end;

function TLRDataFlashExecutorComandoList.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TLRDataFlashExecutorComandoList.GetConexaoCliente: TLRDataFlashConexaoCliente;
begin
  Result := (GetComponent(0) as TLRDataFlashExecutorComando).ConexaoCliente;
end;

function TLRDataFlashExecutorComandoList.GetValue: string;
begin
  Result := (GetComponent(0) as TLRDataFlashExecutorComando).Comando;
end;

procedure TLRDataFlashExecutorComandoList.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  lCmdList: TLRDataFlashComandoList;
  lLista: TStringList;
  lClient: TLRDataFlashConexaoCliente;
  lConexCliente: TLRDataFlashConexaoCliente;
begin
  inherited;
  // obtem lista de comandos registrados
  lLista := TStringList.Create;
  lClient := TLRDataFlashConexaoCliente.Create(nil);

  lCmdList := TLRDataFlashComandoList.Create;

  try
    lCmdList.Parametro['TipoBusca'].AsInteger := Ord(trpCommandList);
    lConexCliente := GetConexaoCliente;
    if Assigned(lConexCliente) then
    begin
      try
        lClient.ClonarDe(lConexCliente);
        lClient.Comunicar(lCmdList);
        lCmdList.StatusRetorno;

        lLista.Text := lCmdList.Retorno['RetornoProxy'].AsBase64;
        for i := 0 to lLista.Count - 1 do
          if CanAddProxyName(lLista[i]) then
            Proc( lLista[i] );
      except
        on E: Exception do
        begin
          lClient.Desconectar;
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

procedure TLRDataFlashExecutorComandoList.SetValue(const Value: string);
begin
  inherited;

  with (GetComponent(0) as TLRDataFlashExecutorComando) do
  begin
    if Comando <> Value then
    begin
      // configura o novo valor para o componente
      Comando := Value;
      // limpa os parametros atuais
      Parametros.Clear;
      Retornos.Clear;

      if Value <> EmptyStr then
        CarregaParametros;
    end;
  end;
end;

end.
