unit uLRDF.DataSetProviderList;

interface

uses
  DesignEditors, Classes, DesignIntf, uLRDF.Comando, Dialogs;

type
  TLRDataFlashDataSetProviderList = class(TPropertyEditor)
  protected
    function CanAddProxyName(const AProxyName : string) : Boolean; virtual;
  public
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;

    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

implementation

uses
  SysUtils,
  uLRDF.DataSet,
  uLRDF.Component,
  uLRDF.ProxyGenerator;

{ TLRDataFlashDataSetProviderList }

function TLRDataFlashDataSetProviderList.CanAddProxyName(
  const AProxyName: string): Boolean;
begin
  Result := True;
end;

constructor TLRDataFlashDataSetProviderList.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
end;

destructor TLRDataFlashDataSetProviderList.Destroy;
begin
  inherited Destroy;
end;

function TLRDataFlashDataSetProviderList.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TLRDataFlashDataSetProviderList.GetValue: string;
begin
  Result := (GetComponent(0) as TLRDataFlashDataSet).ProviderClass;
end;

procedure TLRDataFlashDataSetProviderList.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  lCmdList: TLRDataFlashComandoList;
  lLista: TStringList;
  lClient: TLRDataFlashConexaoCliente;
begin
  inherited;
  // obtem lista de comandos registrados
  lLista := TStringList.Create;
  lClient := TLRDataFlashConexaoCliente.Create(nil);

  lCmdList := TLRDataFlashComandoList.Create;

  with (GetComponent(0) as TLRDataFlashDataSet) do
  try
    lCmdList.Parametro['TipoBusca'].AsInteger := Ord(trpDataSetList);
    if Assigned(ConexaoCliente) then
    begin
      try
        lClient.ClonarDe( ConexaoCliente );
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
          ShowMessage('Erro verificando lista de providers. ' + E.Message);
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

procedure TLRDataFlashDataSetProviderList.SetValue(const Value: string);
begin
  inherited;
  (GetComponent(0) as TLRDataFlashDataSet).ProviderClass := Value;
end;

end.
