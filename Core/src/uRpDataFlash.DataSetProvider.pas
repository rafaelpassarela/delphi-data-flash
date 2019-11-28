unit uRpDataFlash.DataSetProvider;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Command, Classes, uRpDataFlash.Components, SysUtils,
  {$IFDEF UNICODE}
  System.Types, System.Contnrs,
  {$ELSE}
  Contnrs,
  {$ENDIF}
  uRpDataFlash.Types, uRpDataFlash.Utils;

type
  TRpDataFlashCustomDataSetProvider = class;
//  TRpDataFlashMasterDataSetProvider = class;

  TRpDataFlashOnProviderSendCallback = function (const AComando: IRpDataFlashCommandInterfaced; var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean of object;

  TRpDataFlashDataSetCommandProvider = class(TRpDataFlashDataSetProviderCommand)
  protected
    FProvider: TRpDataFlashCustomDataSetProvider;
    // from TRpDataFlashComando
    function GetCommand: string; override;
    function DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean; override;
    function GetProcessType : TRpDataFlashProcessType; override;
    function GetLifeCycle: TRpDataFlashLifeCycle; override;

    // all the decendents must implement this
    function ExecSQL(const pSQL : string) : Boolean; override; final;
    function Select(const pSelectSQL : string; out XMLData : string): Boolean; override; final;

    function DoStartTransaction : Boolean; override; final;
    function DoCommit(const pRetaining : Boolean) : Boolean; override; final;
    function DoRollback(const pRetaining : Boolean) : Boolean; override; final;
    // custom providers don't need this
    function GetSelectSQL : string; override; final;
    function GetInsertSQL : string; override; final;
    function GetUpdateSQL : string; override; final;
    function GetDeleteSQL : string; override; final;
//    function DoPrepareClient : Boolean;
//    function DoExecutar : Boolean; override; final;
//    procedure DoRegistrarParametros; override; final;
  public
    constructor Create(const AItem : TRpDataFlashCustomDataSetProvider); reintroduce;
  end;

  TRpDataFlashCustomDataSetProvider = class(TComponent)
  private
    FGroupName: string;
    FServer: TRpDataFlashServerConnection;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    FSelectSQL: TStrings;
    FProcessType: TRpDataFlashProcessType;
    FOnSendCallback: TRpDataFlashOnProviderSendCallback;
//    FOnCommit: TRpDataFlashOnTransactionEvent;
//    FOnRollback: TRpDataFlashOnTransactionEvent;
//    FOnStartTransaction: TRpDataFlashOnStartTransactionEvent;
//    FMasterProvider: TRpDataFlashMasterDataSetProvider;
    FLifeCycle: TRpDataFlashLifeCycle;
    FOnBeforeDataSetSelectSQL: TRpDataFlashOnSelect;
    FOnBeforeDataSetExecuteSQL: TRpDataFlashOnExecSQL;
    FDescription: string;
    procedure SetGroupName(const Value: string);
    procedure SetServer(const Value: TRpDataFlashServerConnection);
    procedure SetInsertSQL(const Value: TStrings);
    procedure SetUpdateSQL(const Value: TStrings);
    procedure SetDeleteSQL(const Value: TStrings);
    procedure SetSelectSQL(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Server : TRpDataFlashServerConnection read FServer write SetServer;
    property ProcessType : TRpDataFlashProcessType read FProcessType write FProcessType default prtRemote;
    property LifeCycle : TRpDataFlashLifeCycle read FLifeCycle write FLifeCycle default tlfInstance;

    property InsertSQL : TStrings read FInsertSQL write SetInsertSQL;
    property UpdateSQL : TStrings read FUpdateSQL write SetUpdateSQL;
    property DeleteSQL : TStrings read FDeleteSQL write SetDeleteSQL;
    property SelectSQL : TStrings read FSelectSQL write SetSelectSQL;

//    property MasterProvider : TRpDataFlashMasterDataSetProvider read FMasterProvider write FMasterProvider;

    property OnSendCallback : TRpDataFlashOnProviderSendCallback read FOnSendCallback write FOnSendCallback;
    property OnBeforeDataSetExecuteSQL : TRpDataFlashOnExecSQL read FOnBeforeDataSetExecuteSQL write FOnBeforeDataSetExecuteSQL;
    property OnBeforeDataSetSelectSQL : TRpDataFlashOnSelect read FOnBeforeDataSetSelectSQL write FOnBeforeDataSetSelectSQL;

//    property OnCommit : TRpDataFlashOnTransactionEvent read FOnCommit write FOnCommit;
//    property OnRollback : TRpDataFlashOnTransactionEvent read FOnRollback write FOnRollback;
//    property OnStartTransaction : TRpDataFlashOnStartTransactionEvent read FOnStartTransaction write FOnStartTransaction;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EditarProvider;

    property GroupName : string read FGroupName write SetGroupName;
    property Description : string read FDescription write FDescription;
  end;

{  TRpDataFlashMasterDataSetProvider = class(TRpDataFlashCustomDataSetProvider)
  published
    property OnSendCallback;
    property OnExecuteSQL;
    property OnSelect;
    property OnCommit;
    property OnRollback;
    property OnStartTransaction;
  end; }

  TRpDataFlashDataSetProvider = class(TRpDataFlashCustomDataSetProvider)
  published
    property GroupName;
    property Description;
    property Server;
    property ProcessType;
    property LifeCycle;

    property InsertSQL;
    property UpdateSQL;
    property DeleteSQL;
    property SelectSQL;

    property OnSendCallback;
    property OnBeforeDataSetExecuteSQL;
    property OnBeforeDataSetSelectSQL;
//    property OnCommit;
//    property OnRollback;
//    property OnStartTransaction;

//    property MasterProvider;
  end;

implementation

uses
  uRpDataFlash.EditorComandosProviderView,
  Controls;

{ TRpDataFlashDataSetCommandProvider }

constructor TRpDataFlashDataSetCommandProvider.Create(const AItem: TRpDataFlashCustomDataSetProvider);
begin
  FProvider := AItem;
  inherited Create;
end;

function TRpDataFlashDataSetCommandProvider.DoCallBack(
  var AParamsCallback: TRpDataFlashCommandParameterList): Boolean;
begin
  Result := Assigned(FProvider.OnSendCallback)
        and FProvider.OnSendCallback(Self, AParamsCallback);
end;

function TRpDataFlashDataSetCommandProvider.DoCommit(const pRetaining: Boolean): Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetCommitTransaction) then
    GetServer.OnBeforeDataSetCommitTransaction(Self, pRetaining, Result);

  if Result then
    Result := Executor.Commit(pRetaining);
end;

function TRpDataFlashDataSetCommandProvider.DoRollback(const pRetaining: Boolean): Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetRollbackTransaction) then
    GetServer.OnBeforeDataSetRollbackTransaction(Self, pRetaining, Result);

  if Result then
    Result := Executor.Rollback(pRetaining);
end;

function TRpDataFlashDataSetCommandProvider.DoStartTransaction: Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetStartTransaction) then
    GetServer.OnBeforeDataSetStartTransaction(Self, Result);

  if Result then
    Result := Executor.StartTransaction;
end;

function TRpDataFlashDataSetCommandProvider.ExecSQL(const pSQL: string): Boolean;
var
  lAuxSQL: string;
begin
  Result := True;
  lAuxSQL := pSQL;

  if Assigned(FProvider.OnBeforeDataSetExecuteSQL) then
    FProvider.OnBeforeDataSetExecuteSQL(Self, lAuxSQL, Result);

  if Result then
    Executor.ExecuteSQL(lAuxSQL);
end;

function TRpDataFlashDataSetCommandProvider.GetCommand: string;
begin
  Result := FProvider.Name;
end;

function TRpDataFlashDataSetCommandProvider.GetDeleteSQL: string;
begin
  Result := FProvider.DeleteSQL.Text;
end;

function TRpDataFlashDataSetCommandProvider.GetInsertSQL: string;
begin
  Result := FProvider.InsertSQL.Text;
end;

function TRpDataFlashDataSetCommandProvider.GetLifeCycle: TRpDataFlashLifeCycle;
begin
  Result := FProvider.LifeCycle;
end;

function TRpDataFlashDataSetCommandProvider.GetSelectSQL: string;
begin
  Result := FProvider.SelectSQL.Text;
end;

function TRpDataFlashDataSetCommandProvider.GetProcessType: TRpDataFlashProcessType;
begin
  Result := FProvider.ProcessType;
end;

function TRpDataFlashDataSetCommandProvider.GetUpdateSQL: string;
begin
  Result := FProvider.UpdateSQL.Text;
end;

function TRpDataFlashDataSetCommandProvider.Select(const pSelectSQL: string;
  out XMLData: string): Boolean;
const
  C_NULL_CLAUSE = ' 1 = 2 ';
  C_NULL_WHERE = ' where ' + C_NULL_CLAUSE;
var
  lAuxSQL: string;
  p: Integer;
begin
  Result := True;
  lAuxSQL := pSelectSQL;

  // nao retorno nenhum registro
  if InfoQuery then
  begin
    // insere depois do where
    p := Pos('WHERE', UpperCase(lAuxSQL));
    if p > 0 then
      Insert(C_NULL_CLAUSE + ' and ', lAuxSQL, p + 5)
    else
    begin
      // insere antes do order by
      p := Pos('ORDER BY', UpperCase(lAuxSQL));
      if p > 0 then
        Insert(C_NULL_WHERE, lAuxSQL, p - 1)
      else
      begin
        // insere antes do order by
        p := Pos('GROUP BY', UpperCase(lAuxSQL));
        if p > 0 then
          Insert(C_NULL_WHERE, lAuxSQL, p - 1)
        else
          lAuxSQL := lAuxSQL + C_NULL_WHERE;
      end;
    end;
  end;

  if Assigned(FProvider.OnBeforeDataSetSelectSQL) then
    FProvider.OnBeforeDataSetSelectSQL(Self, lAuxSQL, Result);

  if Result then
    Executor.Select(lAuxSQL, XMLData);
end;

{ TRpDataFlashCustomDataSetProvider }

procedure TRpDataFlashCustomDataSetProvider.SetSelectSQL(const Value: TStrings);
begin
  FSelectSQL.Assign( Value );
end;

procedure TRpDataFlashCustomDataSetProvider.SetServer(const Value: TRpDataFlashServerConnection);
begin
  if (FServer <> nil) then
    FServer.Providers.Remove(Self);

  FServer := Value;

  if (FServer <> nil) and (FServer.Providers.IndexOf(Self) = -1) then
    FServer.Providers.Add(Self);
end;

procedure TRpDataFlashCustomDataSetProvider.SetUpdateSQL(const Value: TStrings);
begin
  FUpdateSQL.Assign( Value );
end;

constructor TRpDataFlashCustomDataSetProvider.Create(AOwner: TComponent);
begin
  inherited;
  FGroupName := C_WITHOUT_GROUP;
  FProcessType := prtRemote;
  FLifeCycle := tlfInstance;

  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TRpDataFlashCustomDataSetProvider.Destroy;
begin
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSelectSQL);
  FreeAndNil(FDeleteSQL);
  inherited;
end;

procedure TRpDataFlashCustomDataSetProvider.EditarProvider;
var
  lView: TfrmEditorComandosProvider;
begin
  if (csDesigning in ComponentState) then
  begin
    lView := TfrmEditorComandosProvider.Create(nil);
    // passa os valores para os mesmos
    lView.MemoDelete.Text := Trim( Self.DeleteSQL.Text );
    lView.MemoInsert.Text := Trim( Self.InsertSQL.Text );
    lView.MemoSelect.Text := Trim( Self.SelectSQL.Text );
    lView.MemoUpdate.Text := Trim( Self.UpdateSQL.Text );
    if lView.ShowModal = mrOk then
    begin
      Self.DeleteSQL.Text := Trim(lView.MemoDelete.Text);
      Self.InsertSQL.Text := Trim(lView.MemoInsert.Text);
      Self.SelectSQL.Text := Trim(lView.MemoSelect.Text);
      Self.UpdateSQL.Text := Trim(lView.MemoUpdate.Text);
    end;
  end
  else
    raise Exception.Create('Não é permitido editar um provider em Runtime !');
end;

procedure TRpDataFlashCustomDataSetProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FServer) and (Operation = opRemove) then
    FServer := nil;
end;

procedure TRpDataFlashCustomDataSetProvider.SetDeleteSQL(const Value: TStrings);
begin
  FDeleteSQL.Assign( Value );
end;

procedure TRpDataFlashCustomDataSetProvider.SetGroupName(const Value: string);
begin
  FGroupName := TRpDataFlashValidations.NameValidation( Value );
  if FGroupName = EmptyStr then
    FGroupName := C_WITHOUT_GROUP;
end;

procedure TRpDataFlashCustomDataSetProvider.SetInsertSQL(const Value: TStrings);
begin
  FInsertSQL.Assign( Value );
end;

end.
