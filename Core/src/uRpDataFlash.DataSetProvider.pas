unit uRpDataFlash.DataSetProvider;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Command, Classes, uRpDataFlash.Components, SysUtils,
  uRpDataFlash.Types, uRpDataFlash.Utils;

type
  TLRDataFlashCustomDataSetProvider = class;
//  TLRDataFlashMasterDataSetProvider = class;

  TLRDataFlashOnProviderSendCallback = function (const AComando: IRpDataFlashCommandInterfaced; var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean of object;

  TLRDataFlashDataSetCommandProvider = class(TLRDataFlashComandoDataSetProvider)
  protected
    FProvider: TLRDataFlashCustomDataSetProvider;
    // from TLRDataFlashComando
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
    constructor Create(const AItem : TLRDataFlashCustomDataSetProvider); reintroduce;
  end;

  TLRDataFlashCustomDataSetProvider = class(TComponent)
  private
    FGrupo: string;
    FServer: TRpDataFlashServerConnection;
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    FSelectSQL: TStrings;
    FTipoProcessamento: TRpDataFlashProcessType;
    FOnSendCallback: TLRDataFlashOnProviderSendCallback;
//    FOnCommit: TLRDataFlashOnTransactionEvent;
//    FOnRollback: TLRDataFlashOnTransactionEvent;
//    FOnStartTransaction: TLRDataFlashOnStartTransactionEvent;
//    FMasterProvider: TLRDataFlashMasterDataSetProvider;
    FLifeCycle: TRpDataFlashLifeCycle;
    FOnBeforeDataSetSelectSQL: TLRDataFlashOnSelect;
    FOnBeforeDataSetExecuteSQL: TLRDataFlashOnExecSQL;
    FDescricao: string;
    procedure SetGrupo(const Value: string);
    procedure SetServer(const Value: TRpDataFlashServerConnection);
    procedure SetInsertSQL(const Value: TStrings);
    procedure SetUpdateSQL(const Value: TStrings);
    procedure SetDeleteSQL(const Value: TStrings);
    procedure SetSelectSQL(const Value: TStrings);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property Server : TRpDataFlashServerConnection read FServer write SetServer;
    property TipoProcessamento : TRpDataFlashProcessType read FTipoProcessamento write FTipoProcessamento default prtRemote;
    property LifeCycle : TRpDataFlashLifeCycle read FLifeCycle write FLifeCycle default tlfInstance;

    property InsertSQL : TStrings read FInsertSQL write SetInsertSQL;
    property UpdateSQL : TStrings read FUpdateSQL write SetUpdateSQL;
    property DeleteSQL : TStrings read FDeleteSQL write SetDeleteSQL;
    property SelectSQL : TStrings read FSelectSQL write SetSelectSQL;

//    property MasterProvider : TLRDataFlashMasterDataSetProvider read FMasterProvider write FMasterProvider;

    property OnSendCallback : TLRDataFlashOnProviderSendCallback read FOnSendCallback write FOnSendCallback;
    property OnBeforeDataSetExecuteSQL : TLRDataFlashOnExecSQL read FOnBeforeDataSetExecuteSQL write FOnBeforeDataSetExecuteSQL;
    property OnBeforeDataSetSelectSQL : TLRDataFlashOnSelect read FOnBeforeDataSetSelectSQL write FOnBeforeDataSetSelectSQL;

//    property OnCommit : TLRDataFlashOnTransactionEvent read FOnCommit write FOnCommit;
//    property OnRollback : TLRDataFlashOnTransactionEvent read FOnRollback write FOnRollback;
//    property OnStartTransaction : TLRDataFlashOnStartTransactionEvent read FOnStartTransaction write FOnStartTransaction;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EditarProvider;

    property Grupo : string read FGrupo write SetGrupo;
    property Descricao : string read FDescricao write FDescricao;
  end;

{  TLRDataFlashMasterDataSetProvider = class(TLRDataFlashCustomDataSetProvider)
  published
    property OnSendCallback;
    property OnExecuteSQL;
    property OnSelect;
    property OnCommit;
    property OnRollback;
    property OnStartTransaction;
  end; }

  TLRDataFlashDataSetProvider = class(TLRDataFlashCustomDataSetProvider)
  published
    property Grupo;
    property Descricao;
    property Server;
    property TipoProcessamento;
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
  fLRDF.EditorComandosProvider,
  Controls;

{ TLRDataFlashDataSetCommandProvider }

constructor TLRDataFlashDataSetCommandProvider.Create(const AItem: TLRDataFlashCustomDataSetProvider);
begin
  FProvider := AItem;
  inherited Create;
end;

function TLRDataFlashDataSetCommandProvider.DoCallBack(
  var AParamsCallback: TRpDataFlashCommandParameterList): Boolean;
begin
  Result := Assigned(FProvider.OnSendCallback)
        and FProvider.OnSendCallback(Self, AParamsCallback);
end;

function TLRDataFlashDataSetCommandProvider.DoCommit(const pRetaining: Boolean): Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetCommitTransaction) then
    GetServer.OnBeforeDataSetCommitTransaction(Self, pRetaining, Result);

  if Result then
    Result := Executor.Commit(pRetaining);
end;

function TLRDataFlashDataSetCommandProvider.DoRollback(const pRetaining: Boolean): Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetRollbackTransaction) then
    GetServer.OnBeforeDataSetRollbackTransaction(Self, pRetaining, Result);

  if Result then
    Result := Executor.Rollback(pRetaining);
end;

function TLRDataFlashDataSetCommandProvider.DoStartTransaction: Boolean;
begin
  Result := True;
  if (GetServer <> nil) and Assigned(GetServer.OnBeforeDataSetStartTransaction) then
    GetServer.OnBeforeDataSetStartTransaction(Self, Result);

  if Result then
    Result := Executor.StartTransaction;
end;

function TLRDataFlashDataSetCommandProvider.ExecSQL(const pSQL: string): Boolean;
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

function TLRDataFlashDataSetCommandProvider.GetCommand: string;
begin
  Result := FProvider.Name;
end;

function TLRDataFlashDataSetCommandProvider.GetDeleteSQL: string;
begin
  Result := FProvider.DeleteSQL.Text;
end;

function TLRDataFlashDataSetCommandProvider.GetInsertSQL: string;
begin
  Result := FProvider.InsertSQL.Text;
end;

function TLRDataFlashDataSetCommandProvider.GetLifeCycle: TRpDataFlashLifeCycle;
begin
  Result := FProvider.LifeCycle;
end;

function TLRDataFlashDataSetCommandProvider.GetSelectSQL: string;
begin
  Result := FProvider.SelectSQL.Text;
end;

function TLRDataFlashDataSetCommandProvider.GetProcessType: TRpDataFlashProcessType;
begin
  Result := FProvider.TipoProcessamento;
end;

function TLRDataFlashDataSetCommandProvider.GetUpdateSQL: string;
begin
  Result := FProvider.UpdateSQL.Text;
end;

function TLRDataFlashDataSetCommandProvider.Select(const pSelectSQL: string;
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

{ TLRDataFlashDataSetProvider }

procedure TLRDataFlashCustomDataSetProvider.SetSelectSQL(const Value: TStrings);
begin
  FSelectSQL.Assign( Value );
end;

procedure TLRDataFlashCustomDataSetProvider.SetServer(const Value: TRpDataFlashServerConnection);
begin
  if (FServer <> nil) then
    FServer.Providers.Remove(Self);

  FServer := Value;

  if (FServer <> nil) and (FServer.Providers.IndexOf(Self) = -1) then
    FServer.Providers.Add(Self);
end;

procedure TLRDataFlashCustomDataSetProvider.SetUpdateSQL(const Value: TStrings);
begin
  FUpdateSQL.Assign( Value );
end;

constructor TLRDataFlashCustomDataSetProvider.Create(AOwner: TComponent);
begin
  inherited;
  FGrupo := C_WITHOUT_GROUP;
  FTipoProcessamento := prtRemote;
  FLifeCycle := tlfInstance;

  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TLRDataFlashCustomDataSetProvider.Destroy;
begin
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FSelectSQL);
  FreeAndNil(FDeleteSQL);
  inherited;
end;

procedure TLRDataFlashCustomDataSetProvider.EditarProvider;
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

procedure TLRDataFlashCustomDataSetProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FServer) and (Operation = opRemove) then
    FServer := nil;
end;

procedure TLRDataFlashCustomDataSetProvider.SetDeleteSQL(const Value: TStrings);
begin
  FDeleteSQL.Assign( Value );
end;

procedure TLRDataFlashCustomDataSetProvider.SetGrupo(const Value: string);
begin
  FGrupo := TRpDataFlashValidations.NameValidation( Value );
  if FGrupo = EmptyStr then
    FGrupo := C_WITHOUT_GROUP;
end;

procedure TLRDataFlashCustomDataSetProvider.SetInsertSQL(const Value: TStrings);
begin
  FInsertSQL.Assign( Value );
end;

end.
