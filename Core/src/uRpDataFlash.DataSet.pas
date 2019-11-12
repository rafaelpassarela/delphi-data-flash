unit uRpDataFlash.DataSet;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  uRpDataFlash.Components, Classes, DB, DBClient, SysUtils, uRpDataFlash.Command,
  uRpDataFlash.Types, uRpDataFlash.FieldFormatter, Variants, Provider, StrUtils,
  Dialogs, Windows, uRpAlgorithms, uRpStringFunctions;

type
  TLRDataFlashDataSet = class;

  ELRDataFlashDataSetNoClient = class(Exception);
  ELRDataFlashDataSetNoConnection = class(Exception);
  ELRDataFlashDataSetConexaoResult = class(Exception);
  ELRDataFlashDataSetNoCommand = class(Exception);

  TLRDataFlashDataSetFormatter = class(TComponent, IFormaterMaskValues)
  private
    FDateMask: string;
    FDateTimeMask: string;
    FTimeMask: string;
    FBoolFalse: string;
    FBoolTrue: string;
    FQuoteChr: Char;
    function DateFormat: string;
    function DateTimeFormat: string;
    function TimeFormat: string;
    function BoolFalseValue: string;
    function BoolTrueValue: string;
    function QuoteChar: Char;
  public
    constructor Create(AOwner : TComponent); override;
  published
    property DateMask : string read FDateMask write FDateMask;
    property DateTimeMask : string read FDateTimeMask write FDateTimeMask;
    property TimeMask : string read FTimeMask write FTimeMask;
    property BoolFalse : string read FBoolFalse write FBoolFalse;
    property BoolTrue : string read FBoolTrue write FBoolTrue;
    property QuoteChr : Char read FQuoteChr write FQuoteChr;
  end;

  TFormatValuesDefault = class(TInterfacedObject, IFormaterMaskValues)
  public
    function DateFormat: string;
    function DateTimeFormat: string;
    function TimeFormat: string;
    function BoolFalseValue: string;
    function BoolTrueValue: string;
    function QuoteChar: Char;
  end;

  TLRDataFlashDataSet = class(TCustomClientDataSet)
  private
    FLockData : Boolean; // quando true, n�o dispara os eventos DoAfter... e DoBefore...
    FComandoEnviar: TRpDataFlashSendCommand;
    FConexaoCliente: TLRDataFlashConexaoCliente;
    FProviderCustom: TRpDataFlashCustomProvider;
    FInternalProvider: TRpDataFlashCustomProvider;
    FProviderClass: string;
    FPrepared: Boolean;
    FAutoCreateParams: Boolean;
    FParams: TRpDataSetParams;
    FOpenWhere: string;
    FFormatter: IFormaterMaskValues;
    FInfoQuery: Boolean;
    function PreparaComando(const pOperacao : TRpDataFlashDataSetOperations) : Boolean;
    function PreparaSQL(const pSQLBase : string; const pFields : TFields;
      pFormatterMask : IFormaterMaskValues) : string;
    function PreparaSelect(const pSQLBase : string; pFormatterMask : IFormaterMaskValues) : string;
    function DoInternalApplyUpdates(MaxErrors: Integer; AFormatter : IFormaterMaskValues) : Integer;
    function GetSQL(const pOperacao : TRpDataFlashDataSetOperations) : string;
    function GetProviderClass: string;
    function GetDefaultFormatter : IFormaterMaskValues; //TFormatValuesDefault;
    function ValidServer : Boolean;
    procedure Prepare;
    procedure SetParams(const Value: TRpDataSetParams);
    function GetParams: TRpDataSetParams;
    procedure PrepararComandoEnvio;
  protected
    // override to init fields on designing
    procedure InternalInitFieldDefs; override;
    procedure OpenCursor(InfoQuery: Boolean); override;

    procedure DoCommit(const pRetaining : Boolean);
    procedure DoRollback(const pRetaining : Boolean);
    procedure DoBeforeOpen; override;
    procedure DoBeforeRefresh; override;
//    function GetAppServer: IAppServer; override;

//    procedure DoAfterCancel; virtual;
//    procedure DoAfterClose; virtual;
//    procedure DoAfterDelete; virtual;
//    procedure DoAfterEdit; virtual;
//    procedure DoAfterInsert; virtual;
//    procedure DoAfterOpen; virtual;
//    procedure DoAfterPost; virtual;
//    procedure DoAfterRefresh; virtual;
//    procedure DoAfterScroll; virtual;
//    procedure DoBeforeCancel; virtual;
//    procedure DoBeforeClose; virtual;
//    procedure DoBeforeDelete; virtual;
//    procedure DoBeforeEdit; virtual;
//    procedure DoBeforeInsert; virtual;

//    procedure DoBeforePost; virtual;
//    procedure DoBeforeScroll; virtual;
//    procedure DoOnCalcFields; virtual;
//    procedure DoOnNewRecord; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ApplyUpdates(MaxErrors: Integer): Integer; reintroduce; overload;
    function ApplyUpdates(MaxErrors: Integer; AFormatter : IFormaterMaskValues): Integer; reintroduce; overload;

    procedure StartTransaction;
    procedure Commit;
    procedure CommitRetaining;
    procedure Rollback;
    procedure RollbackRetaining;
    procedure FetchParams;

    procedure Open(const AWhereCondition : string = ''); reintroduce;
  published
//    property DataSetField;
//    property CommandText;
//    property ConnectionBroker;
//    property FileName;
//    property ProviderName;
//    property RemoteServer;
    property Active;
    property Aggregates;
    property AggregatesActive;
    property AutoCalcFields;
    property Constraints;
    property DisableStringTrim;
    property Filter;
    property Filtered;
    property FilterOptions;
    property FieldDefs;
    property IndexDefs;
    property IndexFieldNames;
    property IndexName;
    property FetchOnDemand;
    property MasterFields;
    property MasterSource;
    property ObjectView;
    property PacketRecords;
    property ReadOnly;
    property StoreDefs;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
    property OnReconcileError;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property BeforeGetRecords;
    property AfterGetRecords;
    property BeforeRowRequest;
    property AfterRowRequest;
    property BeforeExecute;
    property AfterExecute;
    property BeforeGetParams;
    property AfterGetParams;
    property Ranged;
    // custom propertyes
    property ConexaoCliente : TLRDataFlashConexaoCliente read FConexaoCliente write FConexaoCliente;
    property ProviderCustom : TRpDataFlashCustomProvider read FProviderCustom write FProviderCustom;
    property ProviderClass : string read GetProviderClass write FProviderClass;
    property AutoCreateParams : Boolean read FAutoCreateParams write FAutoCreateParams default True;
    property Params : TRpDataSetParams read GetParams write SetParams;
    property Formatter : IFormaterMaskValues read FFormatter write FFormatter;
  end;

implementation

{ TLRDataFlashDataSet }

function TLRDataFlashDataSet.ApplyUpdates(MaxErrors: Integer): Integer;
begin
  Result := DoInternalApplyUpdates(MaxErrors, GetDefaultFormatter);
end;

function TLRDataFlashDataSet.ApplyUpdates(MaxErrors: Integer;
  AFormatter: IFormaterMaskValues): Integer;
begin
  Result := DoInternalApplyUpdates(MaxErrors, AFormatter);
end;

procedure TLRDataFlashDataSet.Commit;
begin
  DoCommit(False);
end;

procedure TLRDataFlashDataSet.CommitRetaining;
begin
  DoCommit(True);
end;

constructor TLRDataFlashDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLockData := False;
  AutoCreateParams := True;
  FParams := TRpDataSetParams.Create(Self);
  FOpenWhere := EmptyStr;
  FProviderCustom := TRpDataFlashCustomProvider.Create;
  FInternalProvider := TRpDataFlashCustomProvider.Create;
//  FDummyProvider := TDummyProvider.Create(Self);

  PrepararComandoEnvio;
end;

destructor TLRDataFlashDataSet.Destroy;
begin
  FreeAndNil(FProviderCustom);
  FreeAndNil(FInternalProvider);
  FreeAndNil(FComandoEnviar);
  FreeAndNil(FParams);
//  FreeAndNil(FDummyProvider);
  inherited Destroy;  //ainda fala sobrescrever alguns m�todos para evitar erro
end;

function TLRDataFlashDataSet.DoInternalApplyUpdates(MaxErrors: Integer;
  AFormatter: IFormaterMaskValues): Integer;
var
  lDelta: TCustomClientDataSet;
  lChange: TPacketDataSet;
  lPrepared: Boolean;
  lSQL: string;
  lOwnerData: OleVariant;
  lCommandList: TStringList;
  lRecNo: Integer;
begin
  Result := MaxErrors + 1;

  CheckBrowseMode;
//  if Self.State in dsEditModes then
//    Self.Post;

  if Self.ChangeCount > 0 then
  begin
    lOwnerData := Self.Delta;
    lCommandList := TStringList.Create;

    if Assigned(BeforeApplyUpdates) then
      BeforeApplyUpdates(Self, lOwnerData);

    lChange := TPacketDataSet.Create(nil);

    lChange.Data := Self.Delta;
    while not lChange.Eof do
    begin
      lChange.InitAltRecBuffers(False);
      if lChange.UpdateStatus = usUnmodified then
        lChange.InitAltRecBuffers(True);

      lPrepared := True;
      case lChange.UpdateStatus of
        usModified: lSQL := PreparaSQL(GetSQL(opdsUpdate), lChange.Fields, AFormatter);
        usInserted: lSQL := PreparaSQL(GetSQL(opdsInsert), lChange.Fields, AFormatter);
        usDeleted:  lSQL := PreparaSQL(GetSQL(opdsDelete), lChange.Fields, AFormatter);
      else
        lPrepared := False;
      end;

      if lPrepared then
        lCommandList.Append( IntToStr(lCommandList.Count) + '=' + Algorithms.Base64CompressedString(lSQL) );
      lChange.Next;
    end;

    // se existem comandos, envia a lista para o servidor
    if lCommandList.Count > 0 then
    begin
      PreparaComando(opdsApplyUpdates);
      // adiciona o total de comandos na primeira linha
      lCommandList.Insert(0, IntToStr(lCommandList.Count));
      FComandoEnviar.Param['SQLInstruct'].AsBase64 := lCommandList.Text;
      FConexaoCliente.Comunicar(FComandoEnviar);
      if FComandoEnviar.ReturnStatus then
      begin
        Result := FComandoEnviar.ResultParam['ApplyErrCount'].AsInteger;
        if Result > MaxErrors then
          raise ELRDataFlashDataSetConexaoResult.Create('Erro aplicando altera��es no banco de dados. ' + FComandoEnviar.LastError);
        // marca internamente as altera��es como resolvidas, para n�o entrarem em um novo apply
        lRecNo := Self.RecNo;

//        DSBase.AcceptChanges;
//        Resync([]);
        Self.MergeChangeLog;

        // mantem a posicao aproximada
        try
          if lRecNo > Self.RecordCount then
            Self.First
          else
            Self.RecNo := lRecNo;
        except
          Self.First;
        end;
      end
      else
        raise ELRDataFlashDataSetConexaoResult.Create('Erro aplicando altera��es no banco de dados. ' + FComandoEnviar.LastError);
    end;
    FreeAndNil(lChange);
    FreeAndNil(lDelta);
    FreeAndNil(lCommandList);

    if Assigned(AfterApplyUpdates) then
      AfterApplyUpdates(Self, lOwnerData);
  end;
end;

procedure TLRDataFlashDataSet.DoBeforeOpen;
begin
  if FLockData then
    Exit;

  inherited;

  if not ValidServer then
    Exit;

  if not FPrepared then
    Prepare;

  if PreparaComando(opdsSelect) then
  begin
    FComandoEnviar.Param['SQLInstruct'].AsBase64 := PreparaSelect( GetSQL(opdsSelect), GetDefaultFormatter);
    FConexaoCliente.Comunicar(FComandoEnviar);

    if FComandoEnviar.ReturnStatus then
    begin
      FLockData := True;
      try
        if Active then
        begin
          EmptyDataSet;
          Close;
        end;
        Self.XMLData := FComandoEnviar.ResultParam['XMLData'].AsBase64
      finally
        FLockData := False;
      end;
    end
    else
    begin
      FPrepared := False;
      raise ELRDataFlashDataSetConexaoResult.Create('Erro abrindo dataset. ' + FComandoEnviar.LastError);
    end;
  end;
end;

procedure TLRDataFlashDataSet.DoBeforeRefresh;
begin
  inherited;

  if Self.Active then
  begin
    CheckBrowseMode;

    if Self.ChangeCount > 0 then
      raise ELRDataFlashDataSetConexaoResult.Create('Deve aplicar as atualiza��es antes de atualizar os dados.')
    else
    begin
      if Active then
      begin
        EmptyDataSet;
        Close;
      end;

      try
        Open(FOpenWhere); // mantem o filtro utilizado na abertura
      finally
        FLockData := False;
      end;
    end;
  end
  else
    raise ELRDataFlashDataSetConexaoResult.Create('O dataset n�o est� aberto.');

  // verificar um meio mais "elegante" de evitar o AfterRefresh do TCustomClintDataSet
  SysUtils.Abort;
end;

procedure TLRDataFlashDataSet.DoCommit(const pRetaining: Boolean);
begin
  if pRetaining then
    PreparaComando(opdsCommitRetaining)
  else
    PreparaComando(opdsCommit);

  FConexaoCliente.Comunicar(FComandoEnviar);

  if not FComandoEnviar.ReturnStatus then
    raise ELRDataFlashDataSetConexaoResult.Create('Erro finalizando transa��o. ' + FComandoEnviar.LastError);
end;

//function TLRDataFlashDataSet.GetAppServer: IAppServer;
//begin
//  try
//    Result := inherited GetAppServer;
//  except
//    Result := FDummyProvider;
//  end;
//end;

function TLRDataFlashDataSet.GetDefaultFormatter: IFormaterMaskValues;
begin
  if Assigned(FFormatter) then
    Result := FFormatter
  else
    Result := TFormatValuesDefault.Create;
end;

function TLRDataFlashDataSet.GetParams: TRpDataSetParams;
begin
  FParams.AutoCreateParam := FAutoCreateParams;
  Result := FParams;
end;

function TLRDataFlashDataSet.GetProviderClass: string;
begin
  Result := Trim(FProviderClass);
end;

function TLRDataFlashDataSet.GetSQL(const pOperacao: TRpDataFlashDataSetOperations): string;
var
  lTemProvider: Boolean;
begin
  lTemProvider := ProviderClass <> EmptyStr;
  case pOperacao of
    opdsSelect: Result := StrUtils.IfThen(lTemProvider, FInternalProvider.SelectSQL.Text, FProviderCustom.SelectSQL.Text );
    opdsInsert: Result := StrUtils.IfThen(lTemProvider, FInternalProvider.InsertSQL.Text, FProviderCustom.InsertSQL.Text);
    opdsDelete: Result := StrUtils.IfThen(lTemProvider, FInternalProvider.DeleteSQL.Text, FProviderCustom.DeleteSQL.Text);
    opdsUpdate: Result := StrUtils.IfThen(lTemProvider, FInternalProvider.UpdateSQL.Text, FProviderCustom.UpdateSQL.Text);
  end;
end;

procedure TLRDataFlashDataSet.InternalInitFieldDefs;
var
  lCds : TCustomClientDataSet;
  lOldState: Boolean;
begin
  if FInfoQuery then
  begin
    if FLockData then
      Exit;

    if not ValidServer then
      Exit;

    if not FPrepared then
      Prepare;

    if PreparaComando(opdsSelect) then
    begin
      FComandoEnviar.Param['SQLInstruct'].AsBase64 := GetSQL(opdsSelect);
      FConexaoCliente.Comunicar(FComandoEnviar);
      if FComandoEnviar.ReturnStatus then
      begin
        lCds := nil;
        try
          { ?? Rever esta parte, pois ele ir� trazer todos os dados da tabela ?? }
          lCds := TCustomClientDataSet.Create(Self);
          lCds.XMLData := FComandoEnviar.ResultParam['XMLData'].AsBase64;

          FLockData := True;

          lOldState := Self.Active;

          Self.FieldDefs.Assign( lCds.FieldDefs );
          Self.CreateDataSet;

          if Self.Active and (not lOldState) then
            Self.Close;
        finally
          FLockData := False;
          if lCds.Active then
          begin
            lCds.EmptyDataSet;
            lCds.Close;
          end;
          FreeAndNil(lCds);
        end;
      end
      else
        raise ELRDataFlashDataSetConexaoResult.Create('Erro abrindo dataset. ' + FComandoEnviar.LastError);
    end;

    FPrepared := False;
    FInternalProvider.Clear;
  end
  else
    inherited;
end;

procedure TLRDataFlashDataSet.Open(const AWhereCondition : string);
begin
  FOpenWhere := AWhereCondition;
  if not FInfoQuery then
    inherited Open;
//  FOpenWhere := '';  //nao limpa a var para nao perder o efeito no refresh
end;

procedure TLRDataFlashDataSet.OpenCursor(InfoQuery: Boolean);
begin
  FInfoQuery := InfoQuery;
  if FInfoQuery then
    InternalInitFieldDefs
  else
    inherited;
(*
[203A0720]{dbrtl100.bpl}   DB.DatabaseError (Line 2478, "DB.pas" + 2) + $35
[203481CF]{dsnap100.bpl}   DBClient.TCustomClientDataSet.GetAppServer (Line 1684, "DBClient.pas" + 22) + $1D
[2034F135]{dsnap100.bpl}   DBClient.TCustomClientDataSet.DoGetRecords (Line 4598, "DBClient.pas" + 4) + $37
[2034676B]{dsnap100.bpl}   DBClient.TCustomClientDataSet.OpenCursor (Line 1147, "DBClient.pas" + 25) + $3F
[1870E54C]{LRDataFlashcomm.bpl} ULRDataFlashdataset.TLRDataFlashDataSet.OpenCursor + $0
[203AF789]{dbrtl100.bpl}   DB.TDataSet.InitFieldDefs (Line 9355, "DB.pas" + 4) + $9
[203AF7A1]{dbrtl100.bpl}   DB.TDataSet.InitFieldDefs (Line 9356, "DB.pas" + 5) + $13
[203A11F3]{dbrtl100.bpl}   DB.TDefCollection.UpdateDefs (Line 2811, "DB.pas" + 5) + $5
[203A1E64]{dbrtl100.bpl}   DB.TFieldDefs.Update (Line 3167, "DB.pas" + 0) + $8
[225F6845]{dcldb100.bpl}   DSDesign.TFieldsEditor.DoAddFields (Line 813, "DSDesign.pas" + 5) + $C
[225F6862]{dcldb100.bpl}   DSDesign.TFieldsEditor.DoAddFields (Line 815, "DSDesign.pas" + 7) + $B
[225F67D5]{dcldb100.bpl}   DSDesign.TFieldsEditor.AddFields (Line 798, "DSDesign.pas" + 1) + $2
[225F7996]{dcldb100.bpl}   DSDesign.TFieldsEditor.AddAllFields (Line 1340, "DSDesign.pas" + 0) + $2
[201305E7]{vcl100.bpl  }   Menus.TMenuItem.Click (Line 2283, "Menus.pas" + 14) + $8
[20131B0F]{vcl100.bpl  }   Menus.TMenu.DispatchCommand (Line 3008, "Menus.pas" + 5) + $2
[20132C6E]{vcl100.bpl  }   Menus.TPopupList.WndProc (Line 3798, "Menus.pas" + 4) + $E
[20132BBD]{vcl100.bpl  }   Menus.TPopupList.MainWndProc (Line 3779, "Menus.pas" + 2) + $5
[20040E4C]{rtl100.bpl  }   Classes.StdWndProc (Line 11583, "common\Classes.pas" + 8) + $0
(000039E3){IDEFixPack.dll} [076D49E3]
[201625F0]{vcl100.bpl  }   Forms.TApplication.ProcessMessage (Line 8105, "Forms.pas" + 23) + $1
[2016262A]{vcl100.bpl  }   Forms.TApplication.HandleMessage (Line 8124, "Forms.pas" + 1) + $4
[2016291F]{vcl100.bpl  }   Forms.TApplication.Run (Line 8223, "Forms.pas" + 20) + $3
(00003312){IDEFixPack.dll} [076D4312]
[0042297A]{bds.exe     }   bds.bds (Line 195, "" + 7) + $7
*)
end;

function TLRDataFlashDataSet.PreparaComando(const pOperacao: TRpDataFlashDataSetOperations): Boolean;
begin
  // vai no servidor buscar o XML Data
  if ProviderClass <> EmptyStr then
    FComandoEnviar.SetComando(ProviderClass)
  else
  begin
    if FProviderCustom.CustomCommand = EmptyStr then
      raise ELRDataFlashDataSetNoCommand.Create('Nenhum comando foi informado para a comunica��o com o servidor.');
    // deve conter uma classe gen�rica, mas cada banco trata de uma forma a conex�o
    // logo a classe generica deve ser implementada no servidor
    FComandoEnviar.SetComando( FProviderCustom.CustomCommand );
//    FComandoEnviar.Parametro['CustomParams'].AsBase64 := ProviderCustom.GetAsString;
  end;
  FComandoEnviar.Param['Operacao'].AsInteger := Ord(pOperacao);
  FComandoEnviar.Param['ProviderClass'].AsString := ProviderClass;
  FComandoEnviar.Param['SQLInstruct'].AsBase64 := EmptyStr;
  FComandoEnviar.Param['MaxErrors'].AsInteger := 0;
  FComandoEnviar.Param['csDesigning'].AsBoolean := FInfoQuery; //csDesigning in ComponentState;

  Result := True;
end;

procedure TLRDataFlashDataSet.PrepararComandoEnvio;
begin
  FComandoEnviar := TRpDataFlashSendCommand.Create;

  FComandoEnviar.Params.AddParam('Operacao', 0, tvpInteger);

  FComandoEnviar.Params.AddParam('Operacao', 0, tvpInteger);
  FComandoEnviar.Params.AddParam('SQLInstruct', EmptyStr, tvpBase64);
  FComandoEnviar.Params.AddParam('MaxErrors', 0, tvpInteger);
  FComandoEnviar.Params.AddParam('csDesigning', False, tvpBoolean);
  // passa cada SQL
//  FComandoEnviar.Parametros.AddParametro('CustomParams', '', tvpBase64);
  // ou a classe registrada no servidor
  FComandoEnviar.Params.AddParam('ProviderClass', EmptyStr, tvpString);

  // Retornos
  FComandoEnviar.Params.AddResult('XMLData', EmptyStr, tvpBase64);
  FComandoEnviar.Params.AddResult('ApplyErrCount', 0, tvpInteger);
end;

function TLRDataFlashDataSet.PreparaSelect(const pSQLBase: string;
  pFormatterMask: IFormaterMaskValues): string;
var
  lSqlTratado: string;
  lWhere: string;
  lOldAutoCreate: Boolean;
  lFormatter : TFieldFormatter;
  i: Integer;
begin
  lSqlTratado := pSQLBase;
  lFormatter := nil;
  lWhere := EmptyStr;
  try
    if Trim(FOpenWhere) <> EmptyStr then
    begin
      // adiciona um parametro para a condicao de abertura
      lOldAutoCreate := FAutoCreateParams;
      FAutoCreateParams := True;
      Params.ParamByName(C_WHERE_DATASET).AsString := FOpenWhere;
      FAutoCreateParams := lOldAutoCreate;
    end;

    if FParams.Count > 0 then
    begin
      lFormatter := TFieldFormatter.Create(pFormatterMask);

      for i := 0 to FParams.Count - 1 do
      begin
        lFormatter.DataType := FParams[i].DataType;
        lFormatter.FieldName := FParams[i].Name;
        lFormatter.Value := FParams[i].Value;
        // verifica se j� existe o parametro no where
        if Pos(':' + UpperCase(FParams[i].Name), UpperCase(lSqlTratado)) > 0 then
        begin
          // quando ja existir, troca somente o valor
          lSqlTratado := TRpStrings.StringReplaceWholeWord(
            lSqlTratado, ':' + FParams[i].Name, lFormatter.SQLValue, [rfIgnoreCase, rfReplaceAll]);
        end
        else
        begin
          if lWhere <> EmptyStr then
            lWhere := lWhere + ' and ';

          if FParams[i].Name = C_WHERE_DATASET then
            lWhere := lWhere + FParams[i].AsString
          else
            lWhere := lWhere + FParams[i].Name + ' = ' + lFormatter.SQLValue;
        end;
      end;

      if (lWhere <> EmptyStr) then
        lSqlTratado := TRpStrings.InsertWhereCondition(lSqlTratado, lWhere);

      // remove o parametro com a condicao de abertura
      if Trim(FOpenWhere) <> '' then
        Params.Delete( Params.ParamByName(C_WHERE_DATASET).Index );
    end;
  finally
    FreeAndNil(lFormatter);
  end;
  Result := lSqlTratado;
end;

function TLRDataFlashDataSet.PreparaSQL(const pSQLBase : string; const pFields : TFields;
  pFormatterMask : IFormaterMaskValues) : string;
var
  i: Integer;
  lFormatter: TFieldFormatter;

  procedure DoReplaceValue(const pFieldName : string);
  var
    lValue: string;
  begin
    // testa se o campo existe
    if Pos(pFieldName, UpperCase(Result)) > 0 then
    begin
      // se o valor do Delta for null, pode ser pq ele n�o foi alterado, ent�o neste
      // caso pega o valor do data (oldValue)
      if (pFields[i].IsNull) or (Pos('OLD_', pFieldName) > 0) then
        lFormatter.Value := pFields[i].OldValue
      else
        lFormatter.Value := pFields[i].Value;

      lValue := lFormatter.SQLValue;

      Result := TRpStrings.StringReplaceWholeWord(
        Result, ':' + pFieldName, lValue, [rfIgnoreCase, rfReplaceAll]);
    end;
  end;

begin
  Result := pSQLBase;
  lFormatter := TFieldFormatter.Create(pFormatterMask);
  // b�sicamente pega o SQL com par�metros e troca pelos valores fornecidos pelo Delta
  for i := 0 to pFields.Count - 1 do
  begin
    lFormatter.DataType := pFields[i].DataType;
    lFormatter.FieldName := pFields[i].FieldName;

    DoReplaceValue(UpperCase(pFields[i].FieldName));
    DoReplaceValue(UpperCase('OLD_' + pFields[i].FieldName));
  end;
  FreeAndNil(lFormatter);
  Result := Trim(Result);
end;

procedure TLRDataFlashDataSet.Prepare;
begin
  if FProviderClass <> EmptyStr then
  begin
    PreparaComando(opdsPrepare);
    FConexaoCliente.Comunicar(FComandoEnviar);

    if FComandoEnviar.ReturnStatus then
      FInternalProvider.SetFromString( FComandoEnviar.ResultParam['XMLData'].AsBase64 )
    else
      raise ELRDataFlashDataSetConexaoResult.Create('Erro cancelando transa��o. ' + FComandoEnviar.LastError);
  end;

  FPrepared := True;
end;

procedure TLRDataFlashDataSet.Rollback;
begin
  DoRollback(False);
end;

procedure TLRDataFlashDataSet.DoRollback(const pRetaining: Boolean);
begin
  if pRetaining then
    PreparaComando(opdsRollbackRetaining)
  else
    PreparaComando(opdsRollback);

  FConexaoCliente.Comunicar(FComandoEnviar);

  if not FComandoEnviar.ReturnStatus then
    raise ELRDataFlashDataSetConexaoResult.Create('Erro cancelando transa��o. ' + FComandoEnviar.LastError);
end;

procedure TLRDataFlashDataSet.FetchParams;
begin
// DUMMY -> don't make any action
end;

procedure TLRDataFlashDataSet.RollbackRetaining;
begin
  DoRollback(True);
end;

procedure TLRDataFlashDataSet.SetParams(const Value: TRpDataSetParams);
begin
//  FParams := Value;
  FParams.Assign(Value);
end;

procedure TLRDataFlashDataSet.StartTransaction;
begin
  if PreparaComando(opdsStartTrans) then
  begin
    FConexaoCliente.Comunicar(FComandoEnviar);
    if not FComandoEnviar.ReturnStatus then
      raise ELRDataFlashDataSetConexaoResult.Create('Erro iniciando transa��o. ' + FComandoEnviar.LastError);
  end;
end;

function TLRDataFlashDataSet.ValidServer: Boolean;
begin
  if not Assigned(FConexaoCliente) then
  begin
    FPrepared := False;
    raise ELRDataFlashDataSetNoClient.Create('A conex�o cliente n�o foi informada.');
  end
  else
  begin
    // se n�o esta conectado, tenta conectar
    if not FConexaoCliente.Conectado then
    begin
      FConexaoCliente.Conectar;
      if not FConexaoCliente.Conectado then
      begin
        FPrepared := False;
        raise ELRDataFlashDataSetNoConnection.Create('N�o foi poss�vel estabelecer uma conex�o com o servidor ('
        + FConexaoCliente.Servidor + ':' + IntToStr(FConexaoCliente.Porta) + ').');
      end;
    end;
  end;

  Result := True;
end;

{ TFormatValuesDefault }

function TFormatValuesDefault.BoolFalseValue: string;
begin
  Result := 'F';
end;

function TFormatValuesDefault.BoolTrueValue: string;
begin
  Result := 'T';
end;

function TFormatValuesDefault.DateFormat: string;
begin
  Result := 'dd.mm.yyyy';
end;

function TFormatValuesDefault.DateTimeFormat: string;
begin
  Result := DateFormat + ' ' + TimeFormat;
end;

function TFormatValuesDefault.QuoteChar: Char;
begin
  Result := '''';
end;

function TFormatValuesDefault.TimeFormat: string;
begin
  Result := 'hh:nn:ss';
end;

{ TLRDataFlashDataSetFormater }

function TLRDataFlashDataSetFormatter.BoolFalseValue: string;
begin
  Result := FBoolFalse;
end;

function TLRDataFlashDataSetFormatter.BoolTrueValue: string;
begin
  Result := FBoolTrue;
end;

constructor TLRDataFlashDataSetFormatter.Create(AOwner: TComponent);
begin
  inherited;
  FDateMask     := 'dd.mm.yyyy';
  FDateTimeMask := 'dd.mm.yyyy hh:nn:ss';
  FTimeMask     := 'hh:nn:ss';
  FBoolFalse    := 'F';
  FBoolTrue     := 'T';
  FQuoteChr     := '''';
end;

function TLRDataFlashDataSetFormatter.DateFormat: string;
begin
  Result := FDateMask;
end;

function TLRDataFlashDataSetFormatter.DateTimeFormat: string;
begin
  Result := FDateTimeMask;
end;

function TLRDataFlashDataSetFormatter.QuoteChar: Char;
begin
  Result := FQuoteChr;
end;

function TLRDataFlashDataSetFormatter.TimeFormat: string;
begin
  Result := FTimeMask;
end;

end.
