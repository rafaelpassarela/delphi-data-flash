unit uDataModuleServer;

interface

uses
  SysUtils, Classes, uRpDataFlash.Command, DB, Forms, Provider, DBClient,
  uRpSystem, IBX.IBDatabase, IBX.IBSQL, IBX.IBCustomDataSet, IBX.IBQuery;

type
  TDataModuleServer = class(TDataModule, IRpPackageCommandExecutor, IInterface)
    IBDatabaseServico: TIBDatabase;
    IBTransactionServico: TIBTransaction;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FRefCount : Integer;
  public
    { Public declarations }
    // ***** begin - IRpPackageCommandExecutor ******
    function GetName : string;
    function GetDataComponent : TComponent;
    function GetClassName : string;
    procedure DisconnectDataComponent;
    function AsObject: TObject;
    procedure ConfigDataComponent(const AUser: string; const APassword: string);
    // DataSet Events
    function Commit(const ARetaining : Boolean) : Boolean;
    function Rollback(const ARetaining : Boolean) : Boolean;
    function StartTransaction : Boolean;
    function Select(const ASelectSQL: string; out XMLData: string): Boolean;
    function ExecuteSQL(const ASQL: string): Boolean;

    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // ***** end - IRpPackageCommandExecutor ******

  end;

var
  DataModuleServer: TDataModuleServer;

implementation

uses
  Dialogs;

{$R *.dfm}

{ TDataModuleServer }

function TDataModuleServer.AsObject: TObject;
begin
  Result := Self;
end;

function TDataModuleServer.Commit(const ARetaining: Boolean): Boolean;
begin
  if IBTransactionServico.InTransaction then
  begin
    if ARetaining then
      IBTransactionServico.CommitRetaining
    else
      IBTransactionServico.Commit;
  end;
  Result := True;
end;

procedure TDataModuleServer.ConfigDataComponent(const AUser, APassword: string);
begin

end;

procedure TDataModuleServer.DataModuleCreate(Sender: TObject);
var
  lNomeBanco : string;
begin
  lNomeBanco := IncludeTrailingPathDelimiter( ExtractFilePath(Application.ExeName) );
  lNomeBanco := lNomeBanco + 'TESTEDATAFLASH.FDB';
  try
    IBDatabaseServico.Close;
//    IBDatabaseServico.Params.Clear;
    IBDatabaseServico.DatabaseName := 'LOCALHOST:' + lNomeBanco;
//    IBDatabaseServico.Params.Add('user_name=SYSDBA');
//    IBDatabaseServico.Params.Add('password=MASTERKEY');
    IBDatabaseServico.Open;
  except
    on E: Exception do
      raise Exception.Create('Erro conectando no banco de dados. ' + E.Message);
  end;
end;

procedure TDataModuleServer.DisconnectDataComponent;
begin
  try
    if IBDatabaseServico.Connected then
      IBDatabaseServico.Close;

//    FreeAndNil( Self );
//  Self.Free;
//    Self := nil;

  except
    on E:Exception do
      raise Exception.Create('Erro liberando conexão com o banco de dados. ' + E.Message);
  end;
end;

function TDataModuleServer.ExecuteSQL(const ASQL: string): Boolean;
var
  lIbExec : TIBSQL;
begin
  lIbExec := TIBSQL.Create(Self);
  try
    lIbExec.Database := IBDatabaseServico;
    lIbExec.Transaction := IBTransactionServico;
    lIbExec.SQL.Text := ASQL;
    lIbExec.ExecQuery;

    Result := True;
  finally
    FreeAndNil(lIbExec);
  end;
end;

function TDataModuleServer.GetClassName: string;
begin
  Result := ClassName;
end;

function TDataModuleServer.GetDataComponent: TComponent;
begin
  Result := IBTransactionServico;
end;

function TDataModuleServer.GetName: string;
begin
  Result := 'Servidor_Teste';
end;

function TDataModuleServer.Rollback(const ARetaining: Boolean): Boolean;
begin
  if IBTransactionServico.InTransaction then
  begin
    if ARetaining then
      IBTransactionServico.RollbackRetaining
    else
      IBTransactionServico.Rollback;
  end;
  Result := True;
end;

function TDataModuleServer.Select(const ASelectSQL: string;
  out XMLData: string): Boolean;
var
  lQry: TIBQuery;
  lProvider: TDataSetProvider;
  lCds: TClientDataSet;  
begin
  lQry := TIBQuery.Create(Self);
  lProvider := TDataSetProvider.Create(Self);
  lCds := TClientDataSet.Create(Self);
  try
    // Query
    lQry.Database := IBDatabaseServico;
    lQry.Transaction := IBTransactionServico;
    lQry.SQL.Text := ASelectSQL;

    // Provider
    lProvider.DataSet := lQry;
    lProvider.Name := 'Provider_' + FormatDateTime('hhnnsszzz', Time);

    // ClientDataSet
    lCds.ProviderName := lProvider.Name;
    lCds.Open;

    XMLData := lCds.XMLData;
    
    Result := True;
  finally
    lCds.Close;
    FreeAndNil(lCds);
    FreeAndNil(lProvider);
    FreeAndNil(lQry);
  end;
end;

function TDataModuleServer.StartTransaction: Boolean;
begin
  if not IBTransactionServico.InTransaction then
    IBTransactionServico.StartTransaction;
  Result := True;  
end;

function TDataModuleServer._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TDataModuleServer._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if FRefCount = 0 then
    Destroy;
end;

end.
