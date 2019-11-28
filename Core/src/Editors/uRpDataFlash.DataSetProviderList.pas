unit uRpDataFlash.DataSetProviderList;

interface

uses
  DesignEditors, Classes, DesignIntf, uRpDataFlash.Command, Dialogs;

type
  TRpDataFlashDataSetProviderList = class(TPropertyEditor)
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
  uRpDataFlash.DataSet,
  uRpDataFlash.Components,
  uRpDataFlash.ProxyGenerator;

{ TRpDataFlashDataSetProviderList }

function TRpDataFlashDataSetProviderList.CanAddProxyName(
  const AProxyName: string): Boolean;
begin
  Result := True;
end;

constructor TRpDataFlashDataSetProviderList.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
end;

destructor TRpDataFlashDataSetProviderList.Destroy;
begin
  inherited Destroy;
end;

function TRpDataFlashDataSetProviderList.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

function TRpDataFlashDataSetProviderList.GetValue: string;
begin
  Result := (GetComponent(0) as TRpDataFlashDataSet).ProviderClass;
end;

procedure TRpDataFlashDataSetProviderList.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  lCmdList: TRpDataFlashComandoList;
  lLista: TStringList;
  lClient: TRpDataFlashClientConnection;
begin
  inherited;
  // obtem lista de comandos registrados
  lLista := TStringList.Create;
  lClient := TRpDataFlashClientConnection.Create(nil);

  lCmdList := TRpDataFlashComandoList.Create;

  with (GetComponent(0) as TRpDataFlashDataSet) do
  try
    lCmdList.Param['TipoBusca'].AsInteger := Ord(trpDataSetList);
    if Assigned(ClientConnection) then
    begin
      try
        lClient.ClonarDe( ClientConnection );
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

procedure TRpDataFlashDataSetProviderList.SetValue(const Value: string);
begin
  inherited;
  (GetComponent(0) as TRpDataFlashDataSet).ProviderClass := Value;
end;

end.
