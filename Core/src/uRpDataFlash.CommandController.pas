unit uRpDataFlash.CommandController;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, Contnrs, SysUtils, uRpDataFlash.Command, uRpDataFlash.Types, Variants,
  uRpAlgorithms, uRpStringFunctions;

type
  TLRDataFlashComandItemBase = class;

  TLRDataFlashComandoItem = class(TRpDataFlashCommand)
  private
    FItem : TLRDataFlashComandItemBase;
  protected
    function GetComando: string; override;
    function DoExecutar : Boolean; override;
    function DoCallBack(var AParamsCallback : TRPDataFlashCommandParameters) : Boolean; override;
    function GetTipoProcessamento : TRpDataFlashProcessType; override;
    function GetLifeCycle: TLRDataFlashLifeCycle; override;

    procedure DoExecutarPonteInvalida(var AContinuar : Boolean); override;
    procedure DoExecutarPonteBemSucedida(var AContinuar : Boolean); override;
    procedure DoRegistrarParametros; overload; override;

    procedure DoSerializar; override;
    procedure DoCarregar; override;
    procedure DoErroExecucao(const AErrorMsg : string); override;
    procedure DoValidarParametros; override;
    procedure DoExecutarAntesComunicarPonte(var AContinuar : Boolean); override;
  public
    constructor Create(const AItem : TLRDataFlashComandItemBase); reintroduce;
  end;

  TLRDataFlashNotifyEvent = procedure(const AComando : IRpDataFlashCommandInterfaced) of object;
  TLRDataFlashOnCarregarEvent = TLRDataFlashNotifyEvent;
  TLRDataFlashOnSerializarEvent = TLRDataFlashNotifyEvent;
  TLRDataFlashOnValidarParametrosEvent = TLRDataFlashNotifyEvent;
  TLRDataFlashValidateExecuteEvent = procedure (const AComando : IRpDataFlashCommandInterfaced; var AContinuar : Boolean) of object;
  TLRDataFlashOnExecutarPonteInvalida = TLRDataFlashValidateExecuteEvent;
  TLRDataFlashOnExecutarPonteBemSucedidaEvent = TLRDataFlashValidateExecuteEvent;
  TLRDataFlashOnExecutarAntesComunicarPonteEvent = TLRDataFlashValidateExecuteEvent;
  TLRDataFlashOnErroExecucaoEvent = procedure(const AComando : IRpDataFlashCommandInterfaced; const AErrorMsg : string) of object;
  TLRDataFlashOnExecutarComandItemEvent = function(const AComando : IRpDataFlashCommandInterfaced) : Boolean of object;
  TLRDataFlashOnSendCallback = function (const AComando: IRpDataFlashCommandInterfaced; var AParamsCallback : TRPDataFlashCommandParameters) : Boolean of object;

  TLRDataFlashBaseParametroItem = class(TCollectionItem)
  private
    FNome: string;
    FTipo: TRpDataFlashParamType;
    FTipoValor: TRpDataFlashParamValueType;
    FBaseClass: string;
    procedure SetNome(const Value: string);
  protected
    function GetDisplayName: string; override;
    property Nome : string read FNome write SetNome;
    property Tipo : TRpDataFlashParamType read FTipo write FTipo;
    property TipoValor : TRpDataFlashParamValueType read FTipoValor write FTipoValor;
    property BaseClass : string read FBaseClass write FBaseClass;
  end;

  TLRDataFlashParametroItem = class(TLRDataFlashBaseParametroItem)
  published
    property Nome;
    property Tipo;
    property TipoValor;
    property BaseClass;
  end;

  TLRDataFlashParametrosCollection = class(TOwnedCollection)
  end;

  TLRDataFlashParametroValueItem = class(TLRDataFlashBaseParametroItem)
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
    property Nome;
    property TipoValor;
    property BaseClass;
    property Valor : Variant read FValor write FValor;
  end;

  TLRDataFlashParametrosValueCollection = class(TLRDataFlashParametrosCollection)
  private
    function StrIndexToIntIndex(const AIndexStr : string) : Integer;
    function GetItem(Index: string): TLRDataFlashParametroValueItem;
    procedure SetItem(Index: string; const Value: TLRDataFlashParametroValueItem);
  protected
    procedure SetTipoParametro(Item: TCollectionItem); virtual;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    property Items[Index: string]: TLRDataFlashParametroValueItem read GetItem write SetItem; default;
    function ByIndex(const Index : Integer) : TLRDataFlashParametroValueItem;
  end;

  TLRDataFlashRetornosValueCollection = class(TLRDataFlashParametrosValueCollection)
  protected
    procedure SetTipoParametro(Item: TCollectionItem); override;
  end;

  TLRDataFlashComandItemBase = class(TCollectionItem)
  private
    FParametros: TLRDataFlashParametrosCollection;
    FComando : IRpDataFlashCommandInterfaced;
    FNome: string;
    FDescricao: string;
    FTipoProcessamento: TRpDataFlashProcessType;
    FOnExecute: TLRDataFlashOnExecutarComandItemEvent;
    FOnExecutarPonteInvalida: TLRDataFlashOnExecutarPonteInvalida;
    FOnExecutarPonteBemSucedida: TLRDataFlashOnExecutarPonteBemSucedidaEvent;
    FOnSendCallback: TLRDataFlashOnSendCallback;
    FLifeCycle: TLRDataFlashLifeCycle;
    FOnExecutarAntesComunicarPonte: TLRDataFlashOnExecutarAntesComunicarPonteEvent;
    FOnCarregar: TLRDataFlashOnCarregarEvent;
    FOnSerializar: TLRDataFlashOnSerializarEvent;
    FOnErroExecucao: TLRDataFlashOnErroExecucaoEvent;
    FOnValidarParametros: TLRDataFlashOnValidarParametrosEvent;
  protected
    function GetDisplayName: string; override;
    procedure SetNome(const Value: string); virtual;
  public
    function IsCsDesigning : Boolean;

    property Comando : IRpDataFlashCommandInterfaced read FComando write FComando;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Nome : string read FNome write SetNome;
    property Descricao : string read FDescricao write FDescricao;
    property TipoProcessamento : TRpDataFlashProcessType read FTipoProcessamento write FTipoProcessamento default prtRemote;
    property Parametros : TLRDataFlashParametrosCollection read FParametros write FParametros;
    property LifeCycle : TLRDataFlashLifeCycle read FLifeCycle write FLifeCycle default tlfInstance;

    property OnExecute : TLRDataFlashOnExecutarComandItemEvent read FOnExecute write FOnExecute;
    property OnExecutarPonteInvalida : TLRDataFlashOnExecutarPonteInvalida read FOnExecutarPonteInvalida write FOnExecutarPonteInvalida;
    property OnExecutarPonteBemSucedida : TLRDataFlashOnExecutarPonteBemSucedidaEvent read FOnExecutarPonteBemSucedida write FOnExecutarPonteBemSucedida;
    property OnExecutarAntesComunicarPonte : TLRDataFlashOnExecutarAntesComunicarPonteEvent read FOnExecutarAntesComunicarPonte write FOnExecutarAntesComunicarPonte;
    property OnSendCallback : TLRDataFlashOnSendCallback read FOnSendCallback write FOnSendCallback;

    property OnCarregar : TLRDataFlashOnCarregarEvent read FOnCarregar write FOnCarregar;
    property OnSerializar : TLRDataFlashOnSerializarEvent read FOnSerializar write FOnSerializar;
    property OnErroExecucao : TLRDataFlashOnErroExecucaoEvent read FOnErroExecucao write FOnErroExecucao;
    property OnValidarParametros : TLRDataFlashOnValidarParametrosEvent read FOnValidarParametros write FOnValidarParametros;
  end;

  TLRDataFlashComandItem = class (TLRDataFlashComandItemBase)
  published
    property Nome;
    property Descricao;
    property TipoProcessamento;
    property Parametros;
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

  TLRDataFlashComandItemClass = class of TLRDataFlashComandItemBase;

  TLRDataFlashComandList = class(TOwnedCollection)
  public type
    TIteratorComand = class
    private
      FOwner : TOwnedCollection;
      FIndex : Integer;
    public
      constructor Create(const AOwner : TOwnedCollection);

      function TemProximo : Boolean;
      function Proximo : TLRDataFlashComandItemBase;
    end;
  public
    function IsCsDesigning : Boolean;
    function Novo : TLRDataFlashComandItemBase;
    function Iterator : TIteratorComand;
    function Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
  end;

  TLRDataFlashComandListClass = class of TLRDataFlashComandList;

implementation

{ TLRDataFlashComandList }

function TLRDataFlashComandList.IsCsDesigning: Boolean;
begin
  Result := (GetOwner <> nil)
        and (csDesigning in TComponent(GetOwner).ComponentState);
end;

function TLRDataFlashComandList.Iterator: TIteratorComand;
begin
  Result := TIteratorComand.Create(Self);
end;

function TLRDataFlashComandList.Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
var
  lEnum: TCollectionEnumerator;
  lItem: TLRDataFlashComandItemBase;
begin
  Result := False;
  AObjComando := nil;
  lEnum := GetEnumerator;
  try
    while (lEnum.MoveNext) and (not Result) do
    begin
      lItem := TLRDataFlashComandItemBase(lEnum.Current);
      if lItem.Nome = ANome then
      begin
        AObjComando := TLRDataFlashComandoItem.Create(lItem);
        Result := True;
      end;
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

function TLRDataFlashComandList.Novo: TLRDataFlashComandItemBase;
begin
  Result := TLRDataFlashComandItemBase(Add);
end;

{ TLRDataFlashComandList.TIteratorComand }

constructor TLRDataFlashComandList.TIteratorComand.Create(const AOwner: TOwnedCollection);
begin
  FOwner := AOwner;
  FIndex := -1;
end;

function TLRDataFlashComandList.TIteratorComand.Proximo: TLRDataFlashComandItemBase;
begin
  FIndex  := FIndex + 1;
  Result := TLRDataFlashComandItemBase(FOwner.Items[FIndex]);
end;

function TLRDataFlashComandList.TIteratorComand.TemProximo: Boolean;
begin
  Result := ((FOwner.Count - 1) > (FIndex));
end;

{ TLRDataFlashComandItem }

constructor TLRDataFlashComandItemBase.Create(Collection: TCollection);
begin
  inherited;
  FParametros := TLRDataFlashParametrosCollection.Create(Self, TLRDataFlashParametroItem);
  FTipoProcessamento := prtRemote;
  FLifeCycle := tlfInstance;
end;

destructor TLRDataFlashComandItemBase.Destroy;
begin
  FreeAndNil(FParametros);
  inherited;
end;

function TLRDataFlashComandItemBase.GetDisplayName: string;
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

  case FTipoProcessamento of
    prtLocal: lTipo := 'Local';
    prtRemote: lTipo := 'Remote';
    prtRemoteOnly: lTipo := 'RemoteOnly';
  else
    lTipo := 'Erro TipoProc';
  end;

  Result := Format('%s (%s/%s)', [FNome, lTipo, lLife, FDescricao]);
  if FDescricao <> EmptyStr then
  begin
    Result := Result + '  ' + Copy(FDescricao, 1, 50);
    if Length(FDescricao) > 50 then
      Result := Result + '...';
  end;
end;

function TLRDataFlashComandItemBase.IsCsDesigning: Boolean;
begin
  Result := (GetOwner <> nil)
        and (TLRDataFlashComandList(GetOwner).IsCsDesigning);
end;

procedure TLRDataFlashComandItemBase.SetNome(const Value: string);
begin
  FNome := TLRDataFlashValidations.ValidarNome( Value );
end;

{ TLRDataFlashComandoItem }

constructor TLRDataFlashComandoItem.Create(const AItem: TLRDataFlashComandItemBase);
begin
  FItem := AItem;
  inherited Create;
end;

function TLRDataFlashComandoItem.DoCallBack(var AParamsCallback: TRPDataFlashCommandParameters): Boolean;
begin
  Result := Assigned(FItem.OnSendCallback) and FItem.OnSendCallback(Self, AParamsCallback);
end;

procedure TLRDataFlashComandoItem.DoCarregar;
begin
  inherited;
  if Assigned(FItem.OnCarregar) then
    FItem.OnCarregar(Self);
end;

procedure TLRDataFlashComandoItem.DoErroExecucao(const AErrorMsg: string);
begin
  inherited;
  if Assigned(FItem.OnErroExecucao) then
    FItem.OnErroExecucao(Self, AErrorMsg);
end;

function TLRDataFlashComandoItem.DoExecutar: Boolean;
begin
  Result := Assigned(FItem.OnExecute) and FItem.OnExecute(Self);
end;

procedure TLRDataFlashComandoItem.DoExecutarAntesComunicarPonte(
  var AContinuar: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarAntesComunicarPonte) then
    FItem.OnExecutarAntesComunicarPonte(Self, AContinuar);
end;

procedure TLRDataFlashComandoItem.DoExecutarPonteBemSucedida(
  var AContinuar: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarPonteBemSucedida) then
    FItem.OnExecutarPonteBemSucedida(Self, AContinuar);
end;

procedure TLRDataFlashComandoItem.DoExecutarPonteInvalida(var AContinuar: Boolean);
begin
  inherited;
  if Assigned(FItem.OnExecutarPonteInvalida) then
    FItem.OnExecutarPonteInvalida(Self, AContinuar);
end;

procedure TLRDataFlashComandoItem.DoRegistrarParametros;
var
  lEnum: TCollectionEnumerator;
  lItem: TLRDataFlashParametroItem;
begin
  inherited;
  lEnum := FItem.Parametros.GetEnumerator;
  try
    while lEnum.MoveNext do
    begin
      lItem := TLRDataFlashParametroItem(lEnum.Current);
      Parametros.Novo(lItem.Nome, EmptyStr, lItem.Tipo, lItem.TipoValor);
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

procedure TLRDataFlashComandoItem.DoSerializar;
begin
  inherited;
  if Assigned(FItem.OnSerializar) then
    FItem.OnSerializar(Self);
end;

procedure TLRDataFlashComandoItem.DoValidarParametros;
begin
  inherited;
  if Assigned(FItem.OnValidarParametros) then
    FItem.OnValidarParametros(Self);
end;

function TLRDataFlashComandoItem.GetComando: string;
begin
  Result := FItem.Nome;
end;

function TLRDataFlashComandoItem.GetLifeCycle: TLRDataFlashLifeCycle;
begin
  Result := FItem.LifeCycle;
end;

function TLRDataFlashComandoItem.GetTipoProcessamento: TRpDataFlashProcessType;
begin
  Result := FItem.TipoProcessamento;
end;

{ TLRDataFlashBaseParametroItem }

function TLRDataFlashBaseParametroItem.GetDisplayName: string;
begin
  Result := Format('%s - %s / %s', [
    FNome,
    TRpStrings.EnumToStr(TypeInfo(TRpDataFlashParamType), Ord(FTipo)),
    TRpStrings.EnumToStr(TypeInfo(TRpDataFlashParamValueType), Ord(FTipoValor)) ]);
end;

procedure TLRDataFlashBaseParametroItem.SetNome(const Value: string);
begin
  FNome := TLRDataFlashValidations.ValidarNome( Value );
end;

{ TLRDataFlashParametrosValueCollection }

function TLRDataFlashParametrosValueCollection.ByIndex(
  const Index: Integer): TLRDataFlashParametroValueItem;
begin
  Result :=  TOwnedCollection(Self).Items[ Index ] as TLRDataFlashParametroValueItem;
end;

function TLRDataFlashParametrosValueCollection.GetItem(
  Index: string): TLRDataFlashParametroValueItem;
begin
  Result := TLRDataFlashParametroValueItem(inherited Items[ StrIndexToIntIndex(Index) ]);
end;

procedure TLRDataFlashParametrosValueCollection.SetItem(Index: string;
  const Value: TLRDataFlashParametroValueItem);
begin
//  inherited TLRDataFlashParametroValueItem(FItems[Index]).Assign(Value);
  inherited Items[ StrIndexToIntIndex(Index) ] := Value;
end;

procedure TLRDataFlashParametrosValueCollection.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  if (Action = cnAdded) and Assigned(Item) then
    SetTipoParametro(Item);

  inherited Notify(Item, Action);
end;

procedure TLRDataFlashParametrosValueCollection.SetTipoParametro(Item: TCollectionItem);
begin
  TLRDataFlashParametroValueItem(Item).Tipo := tpInput;
end;

function TLRDataFlashParametrosValueCollection.StrIndexToIntIndex(
  const AIndexStr: string): Integer;
var
  i: Integer;
  lItem: TCollectionItem;
  lValueItem: TLRDataFlashParametroValueItem absolute lItem;
begin
  Result := -1;
  for i := 0 to Count do
  begin
    lItem := TOwnedCollection(Self).Items[i];
    if lValueItem.Nome = AIndexStr then
    begin
      Result := i;
      Break;
    end;
  end;
end;

{ TLRDataFlashRetornosValueCollection }

procedure TLRDataFlashRetornosValueCollection.SetTipoParametro(
  Item: TCollectionItem);
begin
  TLRDataFlashParametroValueItem(Item).Tipo := tpOutput;
end;

{ TLRDataFlashParametroValueItem }

function TLRDataFlashParametroValueItem.GetAsBase64: string;
begin
  Result := Algorithms.Base64DecompressedString( VarToStrDef(FValor, EmptyStr) );
end;

function TLRDataFlashParametroValueItem.GetAsBoolean: Boolean;
begin
  try
    Result := FValor;
  except
    Result := False;
  end;
end;

function TLRDataFlashParametroValueItem.GetAsDateTime: TDateTime;
begin
  try
    Result := FValor;
  except
    Result := StrToDate('01/01/1900');
  end;
end;

function TLRDataFlashParametroValueItem.GetAsFloat: Double;
begin
  try
    Result := FValor;
  except
    Result := 0;
  end;
end;

function TLRDataFlashParametroValueItem.GetAsInteger: Integer;
begin
  try
    Result := FValor;
  except
    Result := 0;
  end;
end;

function TLRDataFlashParametroValueItem.GetAsString: string;
begin
  try
    Result := VarToStrDef(FValor, '');
  except
    Result := '';
  end;
end;

function TLRDataFlashParametroValueItem.IsNull: Boolean;
begin
  Result := (FValor = Null)
         or (FValor = Unassigned);
end;

procedure TLRDataFlashParametroValueItem.SetAsBase64(const Value: string);
begin
  FValor := Algorithms.Base64CompressedString(Value);
end;

procedure TLRDataFlashParametroValueItem.SetAsBoolean(const Value: Boolean);
begin
  FValor := Value;
end;

procedure TLRDataFlashParametroValueItem.SetAsDateTime(const Value: TDateTime);
begin
  FValor := Value;
end;

procedure TLRDataFlashParametroValueItem.SetAsFloat(const Value: Double);
begin
  FValor := Value;
end;

procedure TLRDataFlashParametroValueItem.SetAsInteger(const Value: Integer);
begin
  FValor := Value;
end;

procedure TLRDataFlashParametroValueItem.SetAsString(const Value: string);
begin
  FValor := Value;
end;

end.
