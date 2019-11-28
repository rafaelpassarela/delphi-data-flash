unit uRpDataFlash.CommandController;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, Contnrs, SysUtils, uRpDataFlash.Command, uRpDataFlash.Types, Variants,
  uRpAlgorithms, uRpStringFunctions, uRpDataFlash.Utils;

type
  TRpDataFlashCommandItemBase = class;

  TRpDataFlashComandoItem = class(TRpDataFlashCommand)
  private
    FItem : TRpDataFlashCommandItemBase;
  protected
    function GetCommand: string; override;
    function DoExecute : Boolean; override;
    function DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean; override;
    function GetProcessType : TRpDataFlashProcessType; override;
    function GetLifeCycle: TRpDataFlashLifeCycle; override;

    procedure DoExecuteBridgeError(var AContinue : Boolean); override;
    procedure DoExecuteBridgeSuccessfully(var AContinue : Boolean); override;
    procedure DoRegisterParams; overload; override;

    procedure DoSerialize; override;
    procedure DoLoad; override;
    procedure DoExecutionError(const AErrorMsg : string); override;
    procedure DoValidateParams; override;
    procedure DoExecuteBeforeBridgeConnection(var AContinue : Boolean); override;
  public
    constructor Create(const AItem : TRpDataFlashCommandItemBase); reintroduce;
  end;

  TRpDataFlashNotifyEvent = procedure(const AComando : IRpDataFlashCommandInterfaced) of object;
  TRpDataFlashOnCarregarEvent = TRpDataFlashNotifyEvent;
  TRpDataFlashOnSerializarEvent = TRpDataFlashNotifyEvent;
  TRpDataFlashOnValidarParametrosEvent = TRpDataFlashNotifyEvent;
  TRpDataFlashValidateExecuteEvent = procedure (const AComando : IRpDataFlashCommandInterfaced; var AContinue : Boolean) of object;
  TRpDataFlashOnExecutarPonteInvalida = TRpDataFlashValidateExecuteEvent;
  TRpDataFlashOnExecutarPonteBemSucedidaEvent = TRpDataFlashValidateExecuteEvent;
  TRpDataFlashOnExecutarAntesComunicarPonteEvent = TRpDataFlashValidateExecuteEvent;
  TRpDataFlashOnErroExecucaoEvent = procedure(const AComando : IRpDataFlashCommandInterfaced; const AErrorMsg : string) of object;
  TRpDataFlashOnExecutarComandItemEvent = function(const AComando : IRpDataFlashCommandInterfaced) : Boolean of object;
  TRpDataFlashOnSendCallback = function (const AComando: IRpDataFlashCommandInterfaced; var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean of object;

  TRpDataFlashBaseParametroItem = class(TCollectionItem)
  private
    FName: string;
    FTipo: TRpDataFlashParamType;
    FTipoValor: TRpDataFlashParamValueType;
    FBaseClass: string;
    procedure SetNome(const Value: string);
  protected
    function GetDisplayName: string; override;
    property Name : string read FName write SetNome;
    property Tipo : TRpDataFlashParamType read FTipo write FTipo;
    property TipoValor : TRpDataFlashParamValueType read FTipoValor write FTipoValor;
    property BaseClass : string read FBaseClass write FBaseClass;
  end;

  TRpDataFlashParametroItem = class(TRpDataFlashBaseParametroItem)
  published
    property Name;
    property Tipo;
    property TipoValor;
    property BaseClass;
  end;

  TRpDataFlashParametrosCollection = class(TOwnedCollection)
  end;

  TRpDataFlashParametroValueItem = class(TRpDataFlashBaseParametroItem)
  private
    FValor: Variant;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsString: string;
    function GetAsBase64: string;
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsString(const Value: string);
    procedure SetAsBase64(const Value: string);
  public
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsString : string read GetAsString write SetAsString;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsBase64 : string read GetAsBase64 write SetAsBase64;

    function IsNull : Boolean;
  published
    property Name;
    property TipoValor;
    property BaseClass;
    property Valor : Variant read FValor write FValor;
  end;

  TRpDataFlashParametrosValueCollection = class(TRpDataFlashParametrosCollection)
  private
    function StrIndexToIntIndex(const AIndexStr : string) : Integer;
    function GetItem(Index: string): TRpDataFlashParametroValueItem;
    procedure SetItem(Index: string; const Value: TRpDataFlashParametroValueItem);
  protected
    procedure SetTipoParametro(Item: TCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    property Items[Index: string]: TRpDataFlashParametroValueItem read GetItem write SetItem; default;
    function ByIndex(const Index : Integer) : TRpDataFlashParametroValueItem;
  end;

  TRpDataFlashRetornosValueCollection = class(TRpDataFlashParametrosValueCollection)
  protected
    procedure SetTipoParametro(Item: TCollectionItem); override;
  end;

  TRpDataFlashCommandItemBase = class(TCollectionItem)
  private
    FParams: TRpDataFlashParametrosCollection;
    FCommand : IRpDataFlashCommandInterfaced;
    FName: string;
    FDescription: string;
    FProcessType: TRpDataFlashProcessType;
    FOnExecute: TRpDataFlashOnExecutarComandItemEvent;
    FOnExecutarPonteInvalida: TRpDataFlashOnExecutarPonteInvalida;
    FOnExecutarPonteBemSucedida: TRpDataFlashOnExecutarPonteBemSucedidaEvent;
    FOnSendCallback: TRpDataFlashOnSendCallback;
    FLifeCycle: TRpDataFlashLifeCycle;
    FOnExecutarAntesComunicarPonte: TRpDataFlashOnExecutarAntesComunicarPonteEvent;
    FOnCarregar: TRpDataFlashOnCarregarEvent;
    FOnSerializar: TRpDataFlashOnSerializarEvent;
    FOnErroExecucao: TRpDataFlashOnErroExecucaoEvent;
    FOnValidarParametros: TRpDataFlashOnValidarParametrosEvent;
  protected
    function GetDisplayName: string; override;
    procedure SetNome(const Value: string); virtual;
  public
    function IsCsDesigning : Boolean;

    property Command : IRpDataFlashCommandInterfaced read FCommand write FCommand;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Name : string read FName write SetNome;
    property Description : string read FDescription write FDescription;
    property ProcessType : TRpDataFlashProcessType read FProcessType write FProcessType default prtRemote;
    property Params : TRpDataFlashParametrosCollection read FParams write FParams;
    property LifeCycle : TRpDataFlashLifeCycle read FLifeCycle write FLifeCycle default tlfInstance;

    property OnExecute : TRpDataFlashOnExecutarComandItemEvent read FOnExecute write FOnExecute;
    property OnExecutarPonteInvalida : TRpDataFlashOnExecutarPonteInvalida read FOnExecutarPonteInvalida write FOnExecutarPonteInvalida;
    property OnExecutarPonteBemSucedida : TRpDataFlashOnExecutarPonteBemSucedidaEvent read FOnExecutarPonteBemSucedida write FOnExecutarPonteBemSucedida;
    property OnExecutarAntesComunicarPonte : TRpDataFlashOnExecutarAntesComunicarPonteEvent read FOnExecutarAntesComunicarPonte write FOnExecutarAntesComunicarPonte;
    property OnSendCallback : TRpDataFlashOnSendCallback read FOnSendCallback write FOnSendCallback;

    property OnCarregar : TRpDataFlashOnCarregarEvent read FOnCarregar write FOnCarregar;
    property OnSerializar : TRpDataFlashOnSerializarEvent read FOnSerializar write FOnSerializar;
    property OnErroExecucao : TRpDataFlashOnErroExecucaoEvent read FOnErroExecucao write FOnErroExecucao;
    property OnValidarParametros : TRpDataFlashOnValidarParametrosEvent read FOnValidarParametros write FOnValidarParametros;
  end;

  TRpDataFlashCommandItem = class (TRpDataFlashCommandItemBase)
  published
    property Name;
    property Description;
    property ProcessType;
    property Params;
    property LifeCycle;

    property OnExecute;
    property OnExecutarPonteInvalida;
    property OnExecutarPonteBemSucedida;
    property OnExecutarAntesComunicarPonte;
    property OnSendCallback;
    property OnCarregar;
    property OnSerializar;
    property OnErroExecucao;
    property OnValidarParametros;
  end;

  TRpDataFlashCommandItemClass = class of TRpDataFlashCommandItemBase;

  TRpDataFlashCommandList = class(TOwnedCollection)
  public type
    TIteratorComand = class
    private
      FOwner : TOwnedCollection;
      FIndex : Integer;
    public
      constructor Create(const AOwner : TOwnedCollection);

      function TemProximo : Boolean;
      function Proximo : TRpDataFlashCommandItemBase;
    end;
  public
    function IsCsDesigning : Boolean;
    function Novo : TRpDataFlashCommandItemBase;
    function Iterator : TIteratorComand;
    function Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
  end;

  TRpDataFlashCommandListClass = class of TRpDataFlashCommandList;

implementation

{ TRpDataFlashCommandList }

function TRpDataFlashCommandList.IsCsDesigning: Boolean;
begin
  Result := (GetOwner <> nil)
        and (csDesigning in TComponent(GetOwner).ComponentState);
end;

function TRpDataFlashCommandList.Iterator: TIteratorComand;
begin
  Result := TIteratorComand.Create(Self);
end;

function TRpDataFlashCommandList.Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
var
  lEnum: TCollectionEnumerator;
  lItem: TRpDataFlashCommandItemBase;
begin
  Result := False;
  AObjComando := nil;
  lEnum := GetEnumerator;
  try
    while (lEnum.MoveNext) and (not Result) do
    begin
      lItem := TRpDataFlashCommandItemBase(lEnum.Current);
      if lItem.Name = ANome then
      begin
        AObjComando := TRpDataFlashComandoItem.Create(lItem);
        Result := True;
      end;
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

function TRpDataFlashCommandList.Novo: TRpDataFlashCommandItemBase;
begin
  Result := TRpDataFlashCommandItemBase(Add);
end;

{ TRpDataFlashCommandList.TIteratorComand }

constructor TRpDataFlashCommandList.TIteratorComand.Create(const AOwner: TOwnedCollection);
begin
  FOwner := AOwner;
  FIndex := -1;
end;

function TRpDataFlashCommandList.TIteratorComand.Proximo: TRpDataFlashCommandItemBase;
begin
  FIndex  := FIndex + 1;
  Result := TRpDataFlashCommandItemBase(FOwner.Items[FIndex]);
end;

function TRpDataFlashCommandList.TIteratorComand.TemProximo: Boolean;
begin
  Result := ((FOwner.Count - 1) > (FIndex));
end;

{ TRpDataFlashCommandItemBase }

constructor TRpDataFlashCommandItemBase.Create(Collection: TCollection);
begin
  inherited;
  FParams := TRpDataFlashParametrosCollection.Create(Self, TRpDataFlashParametroItem);
  FProcessType := prtRemote;
  FLifeCycle := tlfInstance;
end;

destructor TRpDataFlashCommandItemBase.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TRpDataFlashCommandItemBase.GetDisplayName: string;
var
  lTipo: string;
  lLife: string;
begin
// Teste (Ponte/Session) - > Comando de Teste do Servidor

  case FLifeCycle of
    tlfInstance: lLife := 'Instance';
    tlfSession: lLife := 'Session';
    tlfServer: lLife := 'Server';
  else
    lLife := 'Erro LifeCycle';
  end;

  case FProcessType of
    prtLocal: lTipo := 'Local';
    prtRemote: lTipo := 'Remote';
    prtRemoteOnly: lTipo := 'RemoteOnly';
  else
    lTipo := 'Erro TipoProc';
  end;

  Result := Format('%s (%s/%s)', [FName, lTipo, lLife, FDescription]);
  if FDescription <> EmptyStr then
  begin
    Result := Result + '  ' + Copy(FDescription, 1, 50);
    if Length(FDescription) > 50 then
      Result := Result + '...';
  end;
end;

function TRpDataFlashCommandItemBase.IsCsDesigning: Boolean;
begin
  Result := (GetOwner <> nil)
        and (TRpDataFlashCommandList(GetOwner).IsCsDesigning);
end;

procedure TRpDataFlashCommandItemBase.SetNome(const Value: string);
begin
  FName := TRpDataFlashValidations.NameValidation( Value );
end;

{ TRpDataFlashComandoItem }

constructor TRpDataFlashComandoItem.Create(const AItem: TRpDataFlashCommandItemBase);
begin
  FItem := AItem;
  inherited Create;
end;

function TRpDataFlashComandoItem.DoCallBack(var AParamsCallback: TRpDataFlashCommandParameterList): Boolean;
begin
  Result := Assigned(FItem.OnSendCallback) and FItem.OnSendCallback(Self, AParamsCallback);
end;

procedure TRpDataFlashComandoItem.DoLoad;
begin
  inherited;
  if Assigned(FItem.OnCarregar) then
    FItem.OnCarregar(Self);
end;

procedure TRpDataFlashComandoItem.DoExecutionError(const AErrorMsg: string);
begin
  inherited;
  if Assigned(FItem.OnErroExecucao) then
    FItem.OnErroExecucao(Self, AErrorMsg);
end;

function TRpDataFlashComandoItem.DoExecute: Boolean;
begin
  Result := Assigned(FItem.OnExecute) and FItem.OnExecute(Self);
end;

procedure TRpDataFlashComandoItem.DoExecuteBeforeBridgeConnection(
  var AContinue: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarAntesComunicarPonte) then
    FItem.OnExecutarAntesComunicarPonte(Self, AContinue);
end;

procedure TRpDataFlashComandoItem.DoExecuteBridgeSuccessfully(
  var AContinue: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarPonteBemSucedida) then
    FItem.OnExecutarPonteBemSucedida(Self, AContinue);
end;

procedure TRpDataFlashComandoItem.DoExecuteBridgeError(var AContinue: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarPonteInvalida) then
    FItem.OnExecutarPonteInvalida(Self, AContinue);
end;

procedure TRpDataFlashComandoItem.DoRegisterParams;
var
  lEnum: TCollectionEnumerator;
  lItem: TRpDataFlashParametroItem;
begin
  inherited;
  lEnum := FItem.Params.GetEnumerator;
  try
    while lEnum.MoveNext do
    begin
      lItem := TRpDataFlashParametroItem(lEnum.Current);
      Params.AddNew(lItem.Name, EmptyStr, lItem.Tipo, lItem.TipoValor);
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

procedure TRpDataFlashComandoItem.DoSerialize;
begin
  inherited;
  if Assigned(FItem.OnSerializar) then
    FItem.OnSerializar(Self);
end;

procedure TRpDataFlashComandoItem.DoValidateParams;
begin
  inherited;
  if Assigned(FItem.OnValidarParametros) then
    FItem.OnValidarParametros(Self);
end;

function TRpDataFlashComandoItem.GetCommand: string;
begin
  Result := FItem.Name;
end;

function TRpDataFlashComandoItem.GetLifeCycle: TRpDataFlashLifeCycle;
begin
  Result := FItem.LifeCycle;
end;

function TRpDataFlashComandoItem.GetProcessType: TRpDataFlashProcessType;
begin
  Result := FItem.ProcessType;
end;

{ TRpDataFlashBaseParametroItem }

function TRpDataFlashBaseParametroItem.GetDisplayName: string;
begin
  Result := Format('%s - %s / %s', [
    FName,
    TRpStrings.EnumToStr(TypeInfo(TRpDataFlashParamType), Ord(FTipo)),
    TRpStrings.EnumToStr(TypeInfo(TRpDataFlashParamValueType), Ord(FTipoValor)) ]);
end;

procedure TRpDataFlashBaseParametroItem.SetNome(const Value: string);
begin
  FName := TRpDataFlashValidations.NameValidation( Value );
end;

{ TRpDataFlashParametrosValueCollection }

function TRpDataFlashParametrosValueCollection.ByIndex(
  const Index: Integer): TRpDataFlashParametroValueItem;
begin
  Result :=  TOwnedCollection(Self).Items[ Index ] as TRpDataFlashParametroValueItem;
end;

function TRpDataFlashParametrosValueCollection.GetItem(
  Index: string): TRpDataFlashParametroValueItem;
begin
  Result := TRpDataFlashParametroValueItem(inherited Items[ StrIndexToIntIndex(Index) ]);
end;

procedure TRpDataFlashParametrosValueCollection.SetItem(Index: string;
  const Value: TRpDataFlashParametroValueItem);
begin
//  inherited TRpDataFlashParametroValueItem(FItems[Index]).Assign(Value);
  inherited Items[ StrIndexToIntIndex(Index) ] := Value;
end;

procedure TRpDataFlashParametrosValueCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if (Action = cnAdded) and Assigned(Item) then
    SetTipoParametro(Item);

  inherited Notify(Item, Action);
end;

procedure TRpDataFlashParametrosValueCollection.SetTipoParametro(Item: TCollectionItem);
begin
  TRpDataFlashParametroValueItem(Item).Tipo := tpInput;
end;

function TRpDataFlashParametrosValueCollection.StrIndexToIntIndex(
  const AIndexStr: string): Integer;
var
  i: Integer;
  lItem: TCollectionItem;
  lValueItem: TRpDataFlashParametroValueItem absolute lItem;
begin
  Result := -1;
  for i := 0 to Count do
  begin
    lItem := TOwnedCollection(Self).Items[i];
    if lValueItem.Name = AIndexStr then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TRpDataFlashRetornosValueCollection }

procedure TRpDataFlashRetornosValueCollection.SetTipoParametro(
  Item: TCollectionItem);
begin
  TRpDataFlashParametroValueItem(Item).Tipo := tpOutput;
end;

{ TRpDataFlashParametroValueItem }

function TRpDataFlashParametroValueItem.GetAsBase64: string;
begin
  Result := Algorithms.Base64DecompressedString( VarToStrDef(FValor, EmptyStr) );
end;

function TRpDataFlashParametroValueItem.GetAsBoolean: Boolean;
begin
  try
    Result := FValor;
  except
    Result := False;
  end;
end;

function TRpDataFlashParametroValueItem.GetAsDateTime: TDateTime;
begin
  try
    Result := FValor;
  except
    Result := StrToDate('01/01/1900');
  end;
end;

function TRpDataFlashParametroValueItem.GetAsFloat: Double;
begin
  try
    Result := FValor;
  except
    Result := 0;
  end;
end;

function TRpDataFlashParametroValueItem.GetAsInteger: Integer;
begin
  try
    Result := FValor;
  except
    Result := 0;
  end;
end;

function TRpDataFlashParametroValueItem.GetAsString: string;
begin
  try
    Result := VarToStrDef(FValor, '');
  except
    Result := '';
  end;
end;

function TRpDataFlashParametroValueItem.IsNull: Boolean;
begin
  Result := (FValor = Null)
         or (FValor = Unassigned);
end;

procedure TRpDataFlashParametroValueItem.SetAsBase64(const Value: string);
begin
  FValor := Algorithms.Base64CompressedString(Value);
end;

procedure TRpDataFlashParametroValueItem.SetAsBoolean(const Value: Boolean);
begin
  FValor := Value;
end;

procedure TRpDataFlashParametroValueItem.SetAsDateTime(const Value: TDateTime);
begin
  FValor := Value;
end;

procedure TRpDataFlashParametroValueItem.SetAsFloat(const Value: Double);
begin
  FValor := Value;
end;

procedure TRpDataFlashParametroValueItem.SetAsInteger(const Value: Integer);
begin
  FValor := Value;
end;

procedure TRpDataFlashParametroValueItem.SetAsString(const Value: string);
begin
  FValor := Value;
end;

end.
