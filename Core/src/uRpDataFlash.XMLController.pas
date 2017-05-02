unit uRpDataFlash.XMLController;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Contnrs, SysUtils, Classes, Variants, DBClient, DB, XMLDoc, XMLIntf,
  uRpEncryption;

const
  VERSION_XML_1_0 = '1.0';
  ENCODE_XML_UTF_8 = 'UTF-8';
  VERSION_XML_DEFAULT = VERSION_XML_1_0;
  ENCODE_XML_DEFAULT = ENCODE_XML_UTF_8;

type
  TCampo = class;
  TRegistro = class;
  TGrupo = class;
  TItemDataPacket = class;
  TListaItemDataPacket = class;
  TItemAtributo = class;
  TListaAtributos = class;
  TConjunto = class;

  TListasVinculadas = class(TObjectList);

  TNovoValorGrupoEvent = procedure (Sender, ANovoValor : TGrupo) of object;

  TBaseGrupoRegistro = class
  private
    FNome: string;
    FAtributos : TListaAtributos;
//    FListasVinculadas: TListasVinculadas;
    function GetAtributos: TListaAtributos;

    function GetChave: string;
    procedure SetChave(const Valor : string);

    function GetCriptografado: Boolean; overload;
    function GetCriptografado(pNodo : IXMLNode): Boolean; overload;
  protected
    procedure SetCriptografado(const Value: Boolean); virtual;
//    property ListasVinculadas : TListasVinculadas read FListasVinculadas;
  public
    constructor Create(const pNome : string); virtual;
    destructor Destroy; override;

    property Nome : string read FNome write FNome;
    property Atributos : TListaAtributos read GetAtributos;

    procedure GerarAtributos(pNodo : IXMLNode);
    procedure CarregarAtributos(const pNodo : IXMLNode);
    function NovoAtributo(const pNome, pValor : string) : Integer;

    function SetVisivel(const pVisivel : Boolean = True) : TBaseGrupoRegistro;
    function SetNomeAmigavel(const pNome : string) : TBaseGrupoRegistro;
    function SetCriptografia : TBaseGrupoRegistro;

    function ToTexto(const pCaminhoCompleto : Boolean = False) : string; virtual;
    function ToXML(pNodoMestre : IXMLNode = nil): string; virtual;

    property Criptografado : Boolean read GetCriptografado write SetCriptografado;
    property Chave : string read GetChave;
  end;

  TListaAtributos = class(TObjectList)
  protected
    function GetItem(Index: Integer): TItemAtributo;
    procedure SetItem(Index: Integer; AObject: TItemAtributo);
  public
    function Add(AObject: TItemAtributo): Integer;
    function IndexOf(AObject: TItemAtributo): Integer;
    procedure Insert(Index: Integer; AObject: TItemAtributo);

    property Items[Index: Integer]: TItemAtributo read GetItem write SetItem; default;
    function Novo(const pNome, pValor : string) : Integer;
    procedure Clonar(const pAtributos : TListaAtributos);

    function PorNome(const pNome : string) : TItemAtributo;
  end;

  TListaItemDataPacket = class(TObjectList)
  protected
    function GetItem(Index: Integer): TItemDataPacket;
    procedure SetItem(Index: Integer; AObject: TItemDataPacket);
    function Find(const pNome : string) : Integer;
  public
    function Add(AObject: TItemDataPacket): Integer;
    function Extract(Item: TItemDataPacket): TItemDataPacket;
    function Remove(AObject: TItemDataPacket): Integer;
    function IndexOf(AObject: TItemDataPacket): Integer;
    procedure Insert(Index: Integer; AObject: TItemDataPacket);
    function First: TItemDataPacket;
    function Last: TItemDataPacket;

    property Items[Index: Integer]: TItemDataPacket read GetItem write SetItem; default;

    function New(const pNome : string; pItemDTP : TItemDataPacket) : Integer;
  end;

  TListaCampos = class(TObjectList)
  protected
    function GetItem(Index: Integer): TCampo;
    procedure SetItem(Index: Integer; AObject: TCampo);
    function Find(const pNome : string) : Integer;
  public
    function Add(AObject: TCampo): Integer;
    function Extract(Item: TCampo): TCampo;
    function Remove(AObject: TCampo): Integer;
    function IndexOf(AObject: TCampo): Integer;
    procedure Insert(Index: Integer; AObject: TCampo);
    function First: TCampo;
    function Last: TCampo;

    property Items[Index: Integer]: TCampo read GetItem write SetItem; default;

    function New(const pNome : string; pRegistro : TRegistro) : Integer;
  end;

  TListaRegistros = class(TObjectList)
  protected
    function GetItem(Index: Integer): TRegistro;
    procedure SetItem(Index: Integer; AObject: TRegistro);
    function Find(const pNome : string) : Integer;
  public
    function Add(AObject: TRegistro): Integer;
    function Extract(Item: TRegistro): TRegistro;
    function Remove(AObject: TRegistro): Integer;
    function IndexOf(AObject: TRegistro): Integer;
    procedure Insert(Index: Integer; AObject: TRegistro);
    function First: TRegistro;
    function Last: TRegistro;

    property Items[Index: Integer]: TRegistro read GetItem write SetItem; default;

    function New(const pNome : string; pGrupo : TGrupo) : Integer; overload;
    function New(const pCampo : TCampo; pGrupo : TGrupo) : Integer; overload;
    function PorNome(const pNome : string) : TRegistro;
    function Existe(const pNome : string) : Boolean;

    function ToTexto : string;
    procedure Clonar(pListaOrigem : TListaRegistros);
  end;

  TStatusListaChaves = (slcNaoInstaciada, slcInvalida, slcValida);
  TStatusListaChavesSet = array of TStatusListaChaves;

  TListaGrupos = class(TObjectList)
  protected
    function GetItem(Index: Integer): TGrupo;
    procedure SetItem(Index: Integer; AObject: TGrupo);
    function Find(const pNome : string; const pInitIndex : Integer = -1) : Integer;
  public
    function Add(AObject: TGrupo): Integer;
    function Extract(Item: TGrupo): TGrupo;
    function Remove(AObject: TGrupo): Integer;
    function IndexOf(AObject: TGrupo): Integer;
    procedure Insert(Index: Integer; AObject: TGrupo);
    function First: TGrupo;
    function Last: TGrupo;

    property Items[Index: Integer]: TGrupo read GetItem write SetItem; default;

    function New(const pNome : string; pGrupo : TGrupo) : Integer; overload;
    function New(const pConjunto : TConjunto; pGrupo : TGrupo) : Integer; overload;

    function StatusChaves : TStatusListaChavesSet;
    function ChavesValidas : Boolean;
    function QuantidadeItensNil : Integer;
  end;

  TItemAtributo = class
  private
    FNome: string;
    FValor: String;
  public
    constructor Create;
    procedure Limpar;
    property Nome : string read FNome write FNome;
    property Valor : String read FValor write FValor;
  end;

  TItemDataPacket = class
  private
    FClient : TClientDataSet;
    FNome: string;
    FGrupo: TGrupo;
    FProcessado : Boolean;
    function Processar(pDataSet : TClientDataSet = nil) : Boolean;
    function GetData: OleVariant;
    function GetString: string;
  public
    constructor Create(pGrupo : TGrupo);
    destructor Destroy; override;

    procedure Limpar;

    property Processado : Boolean read FProcessado;
    property Nome : string read FNome write FNome;
    property Grupo : TGrupo read FGrupo;
    property ToData : OleVariant read GetData;
    procedure ToDataSet(pDataSet : TClientDataSet);
    property ToTexto : string read GetString;

  end;

  TCampo = class(TBaseGrupoRegistro)
  private
    FRegistro : TRegistro;
    function GetRegistro: TRegistro;
  public
    constructor Create(const pNome : string; pRegistro : TRegistro); reintroduce;
    property Registro : TRegistro read GetRegistro;
  end;

  TRegistro = class(TBaseGrupoRegistro)
  private const
    NME_ATTR_TIPO_CAMPO = 'TipoCampo';
  private
    FValor: string;
    FGrupo: TGrupo;
    FCampo: TCampo;
    FTipoCampo: Word;
    function GetCaminho : string;
  public
    constructor Create(const pNome : string; pGrupo : TGrupo); reintroduce;
    destructor Destroy; override;

    property Grupo     : TGrupo read FGrupo write FGrupo;
    property Valor     : string read FValor write FValor;
    property Campo     : TCampo read FCampo;
    property TipoCampo : Word read FTipoCampo;

    function AsVariant : Variant;
    function AsInteger : Integer;
    function AsString : string;
    function AsDateTime : TDateTime;
    function AsBoolean : Boolean;
    function AsFloat : Double;
    function Visivel : Boolean;
    function NomeAmigavel : string;

    procedure DefinirTipo(const pTipo : Word);

    function ToTexto(const pCaminhoCompleto : Boolean = False) : string; override;
    function AddNodo(const pDocumento : IXMLDocument) : IXMLNode;
    function ToXML(pNodoRegistroMestre : IXMLNode) : string; override;
  end;

  TEnumeratorGrupo = class
  private
    FIndexAtual : Integer;
    FGrupoMestre : TGrupo;
    FNomePesquisa : string;
    FAtual : TGrupo;
  public
    constructor Create(const AGrupoMestre : TGrupo; const pNomePesquisa : string);
    function MoverAoProximo : Boolean;

    property Atual : TGrupo read FAtual;
    
  end;

  TGrupo = class(TBaseGrupoRegistro)
  private
    FRegistros : TListaRegistros;
    FGrupos: TListaGrupos;
    FMestre: TGrupo;
    FConjunto : TConjunto;
    FItemDTP : TItemDataPacket;
    FProfundidade: Integer;
    FVersao: string;
    FCodificacao: string;
    FChaveValida : Boolean;
    FValidarChave: Boolean;
    FOnNovoValorGrupo: TNovoValorGrupoEvent;
    function GetCaminho : string;
    procedure CriarRegistros;
    procedure FromXML(pNodoEquivalente : IXMLNode); overload;
    procedure DefinirProfundidade;
    function ToTreeViewString(const pProfundidadeInicial : Integer) : string; overload;
    procedure CarregarCodificacao(const pDocumentoXML : IXMLDocument); overload;
    procedure SetMestre(const Value: TGrupo);
  protected
    procedure SetCriptografado(const Value: Boolean); override;
    function GetChaveValida : Boolean; virtual;
    procedure ProcessarValidacaoChave(const pValorCampo, pChavePublica : string);
  public
    procedure Limpar;

    constructor Create(const pNome : string; pMestre : TGrupo; const pConjunto : TConjunto = nil); reintroduce;
    destructor Destroy; override;

    property Versao : string read FVersao write FVersao;
    property Codificacao : string read FCodificacao write FCodificacao;

    property Mestre : TGrupo read FMestre write SetMestre;
    property Registros : TListaRegistros read FRegistros;
    property Grupos : TListaGrupos read FGrupos;
    property Profundidade : Integer read FProfundidade;
    property ValidarChave : Boolean read FValidarChave write FValidarChave;
    property ChaveValida : Boolean read GetChaveValida;
    property OnNovoValorGrupo : TNovoValorGrupoEvent read FOnNovoValorGrupo write FOnNovoValorGrupo;

    function NovoGrupo(const AGrupo : TGrupo) : TGrupo; overload;
    function NovoGrupo(const pNome : string) : TGrupo; overload;
    function NovoConjunto(const pConjunto : TConjunto) : TGrupo;
    function NovoRegistro(const pNome : string; const  pValor : string = '') : TRegistro; overload;
    function NovoRegistro(const pNome : string; const  pValor : integer) : TRegistro; overload;
    function NovoRegistro(const pNome : string; const  pValor : TDateTime) : TRegistro; overload;
    function NovoRegistro(const pNome : string; const  pValor : Boolean) : TRegistro; overload;
    function NovoRegistro(const pNome : string; const  pValor : Variant) : TRegistro; overload;
    function NovoRegistro(const pCampo : TCampo) : TRegistro; overload;

    function NovoValor(const pNomeRegistro, pValorRegistro : string) : TRegistro; overload;
    function NovoValor(pValorRegistro : string) : Boolean; overload;

    function ToDataPacket(const pDataSet : TClientDataSet = nil) : TItemDataPacket; overload;
    function ToDataPacket(const pNomeGrupo : string) : TItemDataPacket; overload;
    function ToTexto(const pCaminhoCompleto : Boolean = False) : string; override;
    function ToTreeViewString : string; overload;

    function Primeiro(const pNome : string) : TGrupo; overload;
    function Primeiro(const pNome : string; out pIndex : Integer; const pPesquisaGruposFilhos: Boolean = False) : TGrupo; overload;
    function Proximo(const pNome : string; const pAnterior : Integer; out pIndex : Integer) : TGrupo; overload;
    function Proximo(const pAnterior : Integer; out pIndex : Integer) : TGrupo; overload;

    function GetEnumerador(const pNome : string) : TEnumeratorGrupo;

    function PesquisaPorNomeValorRegistro(const pNomeGrupo, pNomeRegistro, pValor : string) : TGrupo; overload;
    function PesquisaPorNomeValorRegistro(const pNomeGrupo : string; pNomesRegistro, pValoresRegistros : array of string) : TGrupo; overload;

    function IsEmpty : Boolean;

    procedure CarregarCodificacao(const pVersao : string = VERSION_XML_DEFAULT; const pCodificacao : string = ENCODE_XML_DEFAULT); overload;
    procedure CarregarXML(const pXML : WideString); overload;
    function ToXML(pNodoMestre : IXMLNode = nil): string; override;
    procedure FromData(const pNome : string; pData : OleVariant);
    procedure FromDataSet(const pNome : string; const pDataSet : TClientDataSet);
    procedure ToFile(const pNomeArquivoXML : string);

    class function FromXML(const pXML: WideString; const pExpandirErro : Boolean = True): TGrupo; overload;
    class function FromFile(const pNomeArquivoXML : string) : TGrupo;
    class function FromStream(const pStream: TStream): TGrupo; overload;
//    class function FromFile(const pNomeArquivoXML : string; const pValidarChaves : Boolean = False) : TGrupo;
  end;

  TConjunto = class(TBaseGrupoRegistro)
  private
    FCampos: TListaCampos;
  public
    constructor Create(const pNome : string); override;
    destructor Destroy; override;

    property Campos : TListaCampos read FCampos;

    function NovoCampo(const pNome : string) : TCampo;
    function NovoAtributo(pCampo : TCampo; const pNome, pValor : string) : TItemAtributo;
  end;

const
  ATRIBUTE_DISPLAY_NAME = 'DisplayName'; // ["Nome Amigável"]
  ATRIBUTE_VISIBLE = 'Visible'; //["True","False"]
  ATRIBUTE_CRIPTO = 'Criptografado'; //["True","False"]
  ATRIBUTE_CHAVE_PUBLICA = 'ChavePublica'; //["Chave pública gerada pelo sistema"]

implementation

{ TListaCampos }

function TListaCampos.Add(AObject: TCampo): Integer;
begin
  Result := inherited Add(AObject);
end;

function TListaCampos.Extract(Item: TCampo): TCampo;
begin
  Result := TCampo(inherited Extract(Item));
end;

function TListaCampos.Find(const pNome: string): Integer;
var
  lItem: Integer;
  lCampo: TCampo;
begin
  lItem := 0;
  Result := -1;
  while (Result = -1) and (lItem < Count) do
  begin
    lCampo := Items[lItem];
    if lCampo.Nome = pNome then
    begin
      Result := lItem;
    end;
    Inc(lItem);
  end;
end;

function TListaCampos.First: TCampo;
begin
  Result := TCampo(inherited First);
end;

function TListaCampos.GetItem(Index: Integer): TCampo;
begin
  Result := TCampo(inherited GetItem(Index));
end;

function TListaCampos.IndexOf(AObject: TCampo): Integer;
begin
  Result := IndexOf(AObject);
end;

procedure TListaCampos.Insert(Index: Integer; AObject: TCampo);
begin
  inherited Insert(Index, AObject);
end;

function TListaCampos.Last: TCampo;
begin
  Result := TCampo(inherited Last);
end;

function TListaCampos.New(const pNome : string; pRegistro: TRegistro): Integer;
var
  lItem: Integer;
  lCampo: TCampo;
begin
  lItem := Find(pNome);
  if lItem = -1 then
  begin
    lCampo := TCampo.Create(pNome, pRegistro);
    lItem := Add(lCampo);
  end;
  Result := lItem;
end;


function TListaCampos.Remove(AObject: TCampo): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TListaCampos.SetItem(Index: Integer; AObject: TCampo);
begin
  inherited SetItem(Index, AObject);
end;

{ TListaRegistros }

function TListaRegistros.Add(AObject: TRegistro): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TListaRegistros.Clonar(pListaOrigem: TListaRegistros);
var
  I: Integer;
  lIndexNovoRegistro: Integer;
  lNovoRegistro: TRegistro;
  lRegistroOrigem: TRegistro;
begin
  Clear;
  for I := 0 to pListaOrigem.Count - 1 do
  begin
    lRegistroOrigem := pListaOrigem[I];
    lIndexNovoRegistro := New(lRegistroOrigem.Nome, lRegistroOrigem.Grupo);
    lNovoRegistro := Items[lIndexNovoRegistro];
    lNovoRegistro.Valor := lRegistroOrigem.Valor;
    lNovoRegistro.Atributos.Clonar(lRegistroOrigem.Atributos);
  end;
end;

function TListaRegistros.Existe(const pNome: string): Boolean;
begin
  Result := (Find(pNome) <> -1);
end;

function TListaRegistros.Extract(Item: TRegistro): TRegistro;
begin
  Result := TRegistro(inherited Extract(Item));
end;

function TListaRegistros.Find(const pNome : string): Integer;
var
  lItem: Integer;
  lCampo: TRegistro;
begin
  lItem := 0;
  Result := -1;
  while (Result = -1) and (lItem < Count) do
  begin
    lCampo := Items[lItem];
    if UpperCase(lCampo.Nome) = UpperCase(pNome) then
    begin
      Result := lItem;
    end;
    Inc(lItem);
  end;
end;

function TListaRegistros.First: TRegistro;
begin
  Result := TRegistro(inherited First);
end;

function TListaRegistros.GetItem(Index: Integer): TRegistro;
begin
  Result := TRegistro(inherited GetItem(Index));
end;

function TListaRegistros.IndexOf(AObject: TRegistro): Integer;
begin
  Result := IndexOf(AObject);
end;

procedure TListaRegistros.Insert(Index: Integer; AObject: TRegistro);
begin
  inherited Insert(Index, AObject);
end;

function TListaRegistros.Last: TRegistro;
begin
  Result := TRegistro(inherited Last);
end;

function TListaRegistros.New(const pCampo: TCampo; pGrupo: TGrupo): Integer;
begin
  Result := New(pCampo.Nome, pGrupo);
  if Result <> -1 then
  begin
    Items[Result].Atributos.Clonar(pCampo.Atributos);
  end;
end;

function TListaRegistros.New(const pNome : string; pGrupo: TGrupo): Integer;
var
  lItem: Integer;
  lRegistro: TRegistro;
begin
  lItem := Find(pNome);
  if lItem = -1 then
  begin
    lRegistro := TRegistro.Create(pNome, pGrupo);
    lRegistro.Campo.Nome := pNome;
    lItem := Add(lRegistro);
  end;
  Result := lItem;
end;

function TListaRegistros.PorNome(const pNome: string): TRegistro;
var
  lProcura: Integer;
begin
  lProcura := Find(pNome);
  if lProcura <> -1 then
  begin
    Result := Items[lProcura];
  end else begin
    Result := nil;
  end;
end;

function TListaRegistros.Remove(AObject: TRegistro): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TListaRegistros.SetItem(Index: Integer; AObject: TRegistro);
begin
  inherited SetItem(Index, AObject);
end;

function TListaRegistros.ToTexto: string;
var
  I: Integer;
begin
  Result := EmptyStr;
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
    begin
      Result := Items[I].ToTexto;
    end else begin
      Result := Result + sLineBreak + Items[I].ToTexto;
    end;
  end;
end;

{ TCampo }

constructor TCampo.Create(const pNome : string; pRegistro: TRegistro);
begin
  inherited Create(pNome);
  FRegistro := pRegistro;
end;

function TCampo.GetRegistro: TRegistro;
begin
  Result := FRegistro;
end;

{ TRegistro }

function TRegistro.AddNodo(const pDocumento: IXMLDocument): IXMLNode;
begin
  if Assigned(pDocumento) then
  begin
    Result := pDocumento.AddChild(FNome);
    Result.NodeValue := FValor;
  end
  else
    Result := nil;
end;

function TRegistro.AsBoolean: Boolean;
begin
  Result := StrToBoolDef(FValor, False);
end;

function TRegistro.AsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(FValor, 0);
end;

function TRegistro.AsFloat: Double;
begin
  Result := StrToFloatDef(FValor, 0);
end;

function TRegistro.AsInteger: Integer;
begin
  Result := StrToIntDef(FValor, -1);
end;

function TRegistro.AsString: string;
begin
  Result := FValor;
end;

function TRegistro.AsVariant: Variant;
var
  lTipo: TItemAtributo;
begin
  lTipo := Atributos.PorNome(NME_ATTR_TIPO_CAMPO);
  if Assigned(lTipo) then
    Result := VarAsType(FValor, StrToInt(lTipo.Valor))
  else
    Result := EmptyStr;
end;

constructor TRegistro.Create(const pNome : string; pGrupo: TGrupo);
begin
  inherited Create(pNome);
  FGrupo  := pGrupo;
  FCampo := TCampo.Create(pNome, Self);
end;

procedure TRegistro.DefinirTipo(const pTipo: Word);
begin
  FTipoCampo := pTipo;
  NovoAtributo(NME_ATTR_TIPO_CAMPO, IntToStr(pTipo));
end;

destructor TRegistro.Destroy;
begin
  FreeAndNil(FCampo);
  inherited;
end;

function TRegistro.GetCaminho: string;
var
  lGrupoMestre: TGrupo;
  lCaminho: string;
begin
  lCaminho := EmptyStr;
  lGrupoMestre := FGrupo;
  while Assigned(lGrupoMestre) do
  begin
    lCaminho := '[' + lGrupoMestre.Nome + ']' + lCaminho;
    lGrupoMestre := lGrupoMestre.Mestre;
  end;
  Result := lCaminho;
end;

function TRegistro.NomeAmigavel: string;
var
  lNomeAmigavel: TItemAtributo;
begin
  Result := FNome;
  lNomeAmigavel := Atributos.PorNome(ATRIBUTE_DISPLAY_NAME);
  if Assigned(lNomeAmigavel) then
  begin
    Result := lNomeAmigavel.Valor;
  end;
end;

function TRegistro.ToTexto(const pCaminhoCompleto : Boolean): string;
var
  lResultado: string;
begin
  lResultado := FNome + '=' + FValor;
  if pCaminhoCompleto then
  begin
    Result := GetCaminho + lResultado;
  end else begin
    Result := lResultado;
  end;
end;

function TRegistro.ToXML(pNodoRegistroMestre : IXMLNode): string;
var
  lNodoRegistro: IXMLNode;
begin
  if Assigned(pNodoRegistroMestre) then
  begin
    lNodoRegistro := pNodoRegistroMestre.AddChild(FNome);
    lNodoRegistro.NodeValue := FValor;
    GerarAtributos(lNodoRegistro);
    Result := lNodoRegistro.XML;
  end
  else
    Result := EmptyStr;
end;

function TRegistro.Visivel: Boolean;
var
  lVisible: TItemAtributo;
begin
  Result := True;
  lVisible := Atributos.PorNome(ATRIBUTE_VISIBLE);
  if Assigned(lVisible) then
    Result := lVisible.Valor = 'True';
end;

{ TGrupo }

constructor TGrupo.Create(const pNome : string; pMestre: TGrupo; const pConjunto : TConjunto);
begin
  inherited Create(pNome);
  FMestre := pMestre;
  FConjunto := pConjunto;
  FRegistros := TListaRegistros.Create;
  FGrupos := TListaGrupos.Create;
  FItemDTP := TItemDataPacket.Create(Self);
  FVersao := EmptyStr;
  FCodificacao := EmptyStr;
  FValidarChave := True;
  CriarRegistros;
  DefinirProfundidade;

  FChaveValida := Assigned(FMestre) and (FMestre.ChaveValida);
  if Assigned(FMestre) then
    FOnNovoValorGrupo := FMestre.OnNovoValorGrupo;
end;

procedure TGrupo.CriarRegistros;
var
  I: Integer;
begin
  if Assigned(FConjunto) then
  begin
    FAtributos.Clonar(FConjunto.Atributos);
    for I := 0 to FConjunto.Campos.Count - 1 do
      NovoRegistro(FConjunto.Campos[I]);
  end;
end;

procedure TGrupo.DefinirProfundidade;
begin
  if Assigned(FMestre) then
  begin
    FProfundidade := FMestre.Profundidade + 1;
  end
  else
    FProfundidade := 0;
end;

destructor TGrupo.Destroy;
begin
  FreeAndNil(FItemDTP);
  FreeAndNil(FRegistros);
  FreeAndNil(FGrupos);
  inherited;
end;

procedure TGrupo.FromXML(pNodoEquivalente: IXMLNode);
var
  I: Integer;
  lNodoFilho: IXMLNode;
  lGrupoCriado: TGrupo;
  lRegistroCriado: TRegistro;
  lValoresDescripto: string;
  lDocumento: IXMLDocument;
  lGrupoNested : TGrupo;
  lChavePublica: string;

  function VerificarCriptografia(const pNodo : IXMLNode) : Boolean;
  var
    lValorCampo: string;
  begin
    Result := GetCriptografado(pNodo);
    if Result then
    begin
      lValorCampo := EmptyStr;
      lGrupoCriado := NovoGrupo(pNodo.NodeName);
      lGrupoCriado.CarregarAtributos(pNodo);
      lChavePublica := lGrupoCriado.Chave;
      lGrupoCriado.SetCriptografado(False);
      if not VarIsNull(pNodo.NodeValue) then
        lValorCampo := pNodo.NodeValue;

      lGrupoCriado.ProcessarValidacaoChave(lValorCampo, lChavePublica);
//      lGrupoCriado.ChaveValida := TRpCriptografiaMD5.Validar(lValorCampo, lChavePublica);
      if lGrupoCriado.ChaveValida then
      begin
        lValoresDescripto := TRpEncryptionCiffer.Decrypting(lValorCampo);
        if lValoresDescripto <> EmptyStr then
        begin
          lDocumento := TXMLDocument.Create(nil);
          try
            lDocumento.LoadFromXML(lValoresDescripto);
            lGrupoNested := lGrupoCriado.NovoGrupo(lDocumento.DocumentElement.NodeName);
            lGrupoNested.CarregarAtributos(lDocumento.DocumentElement);
            lGrupoNested.FromXML(lDocumento.DocumentElement);
          except
          on E: Exception do
            raise Exception.Create(E.Message);
          end;
        end;
      end;
    end;
  end;

begin
  if not VerificarCriptografia(pNodoEquivalente) then
  begin
    for I := 0 to pNodoEquivalente.ChildNodes.Count - 1 do
    begin
      lNodoFilho := pNodoEquivalente.ChildNodes[I];
      if not VerificarCriptografia(lNodoFilho) then
      begin
        if (lNodoFilho.HasChildNodes) and  (not lNodoFilho.IsTextElement) then
        begin
          lGrupoCriado := NovoGrupo(lNodoFilho.NodeName);
          lGrupoCriado.CarregarAtributos(lNodoFilho);
          lGrupoCriado.FromXML(lNodoFilho);
        end
        else
        begin
          lRegistroCriado := NovoRegistro(lNodoFilho.NodeName);
          lRegistroCriado.CarregarAtributos(lNodoFilho);
          if lNodoFilho.NodeValue <> Null then
            lRegistroCriado.Valor := lNodoFilho.NodeValue;
        end;
      end;
    end;
  end;
end;

procedure TGrupo.FromData(const pNome : string; pData: OleVariant);
var
  lClientLocal: TClientDataSet;
begin
  lClientLocal := TClientDataSet.Create(nil);
  try
    try
      lClientLocal.DisableControls;
      lClientLocal.Data := pData;
      FromDataSet(pNome, lClientLocal);
      lClientLocal.EnableControls;
    except
    end;
  finally
    lClientLocal.Free;
  end;
end;

procedure TGrupo.FromDataSet(const pNome : string; const pDataSet: TClientDataSet);
var
  I: Integer;
  lConjunto: TConjunto;
  lGrupoGerado: TGrupo;
  lCampoGerado: TCampo;
begin
  if Assigned(pDataSet) then
  begin
    pDataSet.DisableControls;
    try
      lConjunto := TConjunto.Create(pNome);

      for I := 0 to pDataSet.Fields.Count - 1 do
      begin
        lCampoGerado := lConjunto.NovoCampo(pDataSet.Fields[I].FieldName);
        if pDataSet.Fields[I].Visible then
        begin
          lCampoGerado.NovoAtributo(ATRIBUTE_VISIBLE, 'True');
        end else begin
          lCampoGerado.NovoAtributo(ATRIBUTE_VISIBLE, 'False');
        end;
        lCampoGerado.NovoAtributo(ATRIBUTE_DISPLAY_NAME, pDataSet.Fields[I].DisplayLabel);
      end;

      pDataSet.First;
      while not pDataSet.Eof do
      begin
        lGrupoGerado := NovoConjunto(lConjunto);
        for I := 0 to pDataSet.Fields.Count - 1 do
          lGrupoGerado.NovoValor(pDataSet.Fields[I].FieldName, pDataSet.Fields[I].AsString);

        pDataSet.Next;
      end;
     finally
      pDataSet.EnableControls;
    end;
  end;
end;

class function TGrupo.FromFile(const pNomeArquivoXML: string): TGrupo;
var
  lArquivo: TStringList;
begin
  if FileExists(pNomeArquivoXML) then
  begin
    lArquivo := TStringList.Create;
    try
      lArquivo.LoadFromFile(pNomeArquivoXML);
      Result := Self.FromXML(lArquivo.Text, True);
//      Result.FValidarChaves := pValidarChaves;
    finally
      lArquivo.Free;
    end;
  end
  else
    Result := nil;
end;

class function TGrupo.FromStream(const pStream: TStream): TGrupo;
var
  lDocumento: IXMLDocument;
  lGrupoPrincipal: TGrupo;
begin
  lDocumento := TXMLDocument.Create(nil);
  try
    if pStream <> nil then
    begin
      lDocumento.LoadFromStream(pStream);
      lGrupoPrincipal := Self.Create(lDocumento.DocumentElement.NodeName, nil);
      lGrupoPrincipal.CarregarAtributos(lDocumento.DocumentElement);
      lGrupoPrincipal.FromXML(lDocumento.DocumentElement);
      Result := lGrupoPrincipal;
    end
    else
      Result := nil;
  except
    on E: Exception do
      raise;
  end;
end;

procedure TGrupo.CarregarCodificacao(const pDocumentoXML: IXMLDocument);
begin
  FVersao := pDocumentoXML.Version;
  FCodificacao := pDocumentoXML.Encoding;
end;

procedure TGrupo.CarregarCodificacao(const pVersao, pCodificacao: string);
begin
  FVersao := pVersao;
  FCodificacao := pCodificacao;
end;

procedure TGrupo.CarregarXML(const pXML: WideString);
var
  lDocumento: IXMLDocument;
begin
  lDocumento := TXMLDocument.Create(nil);
  try
    lDocumento.LoadFromXML(pXML);
    Nome := lDocumento.DocumentElement.NodeName;
    CarregarCodificacao(lDocumento);
    CarregarAtributos(lDocumento.DocumentElement);
    FromXML(lDocumento.DocumentElement);
  except

  end;
end;

class function TGrupo.FromXML(const pXML: WideString; const pExpandirErro : Boolean) : TGrupo;
var
  lDocumento: IXMLDocument;
  lGrupoPrincipal: TGrupo;
begin
  lDocumento := TXMLDocument.Create(nil);
  try
    if pXML <> EmptyStr then
    begin
      lDocumento.LoadFromXML(pXML);
      lGrupoPrincipal := Self.Create(lDocumento.DocumentElement.NodeName, nil);
      lGrupoPrincipal.CarregarAtributos(lDocumento.DocumentElement);
      lGrupoPrincipal.FromXML(lDocumento.DocumentElement);
      Result := lGrupoPrincipal;
    end
    else
      Result := nil;
  except
    on E: Exception do
    begin
      Result := nil;
      if pExpandirErro then
        raise;
    end;
  end;
end;

function TGrupo.GetCaminho: string;
var
  lGrupoMestre: TGrupo;
  lCaminho: string;
begin
  lCaminho := EmptyStr;
  lGrupoMestre := FMestre;
  while Assigned(lGrupoMestre) do
  begin
    lCaminho := '[' + lGrupoMestre.Nome + ']' + lCaminho;
    lGrupoMestre := lGrupoMestre.Mestre;
  end;
  Result := lCaminho;
end;

function TGrupo.GetChaveValida: Boolean;
begin
  Result := FChaveValida;
end;

function TGrupo.GetEnumerador(const pNome : string): TEnumeratorGrupo;
begin
  Result := TEnumeratorGrupo.Create(Self, pNome);
end;

//function TGrupo.GetChaveValida: Boolean;
//begin
//  Result := TRpCriptografiaMD5.Validar(ToXML, Chave);
//end;

function TGrupo.IsEmpty: Boolean;
begin
  Result := (Grupos.Count = 0) and (Registros.Count = 0);
end;

procedure TGrupo.Limpar;
begin
  FRegistros.Clear;
  FGrupos.Clear;
  FMestre := nil;
  FConjunto := nil;
  FItemDTP.Limpar;
  FProfundidade := 0;
//  FValidarChaves := False;
end;

function TGrupo.NovoConjunto(const pConjunto: TConjunto): TGrupo;
var
  lGrupoGerado: Integer;
begin
  lGrupoGerado := FGrupos.New(pConjunto, Self);
  Result := FGrupos.Items[lGrupoGerado];
end;

function TGrupo.NovoGrupo(const AGrupo: TGrupo): TGrupo;
begin
  AGrupo.Mestre := Self;
  FGrupos.Add(AGrupo);
  Result := AGrupo;
end;

function TGrupo.NovoGrupo(const pNome: string): TGrupo;
var
  lGrupoGerado: Integer;
begin
  lGrupoGerado := FGrupos.New(pNome, Self);
  Result := FGrupos.Items[lGrupoGerado];
end;

function TGrupo.NovoRegistro(const pNome: string;
  const pValor: Boolean): TRegistro;
begin
  Result := NovoRegistro(pNome, BoolToStr(pValor));
end;

function TGrupo.NovoRegistro(const pNome: string;
  const pValor: TDateTime): TRegistro;
begin
  Result := NovoRegistro(pNome, DateTimeToStr(pValor));
end;

function TGrupo.NovoRegistro(const pCampo: TCampo): TRegistro;
var
  lRegistroGerado: Integer;
begin
  lRegistroGerado := FRegistros.New(pCampo, Self);
  Result := FRegistros.Items[lRegistroGerado];
end;

function TGrupo.NovoRegistro(const pNome: string;
  const pValor: Variant): TRegistro;
begin
  Result := NovoRegistro(pNome, VarToStrDef(pValor, EmptyStr));
  Result.DefinirTipo(VarType(pValor));
end;

function TGrupo.NovoRegistro(const pNome: string;
  const pValor: integer): TRegistro;
begin
  Result := NovoRegistro(pNome, IntToStr(pValor));
end;

function TGrupo.NovoValor(pValorRegistro: string): Boolean;
var
  lRegistros: TGrupo;
begin
  lRegistros := TGrupo.FromXML(pValorRegistro);
  Limpar;
  Grupos.Add(lRegistros);
  if Assigned(FOnNovoValorGrupo) then
  begin
    FOnNovoValorGrupo(Self, lRegistros);
  end;
  Result := True;
end;

function TGrupo.NovoRegistro(const pNome : string; const  pValor : string): TRegistro;
var
  lRegistroGerado: Integer;
begin
  lRegistroGerado := FRegistros.New(pNome, Self);
  Result := FRegistros.Items[lRegistroGerado];
  if pValor <> EmptyStr then
  begin
    Result.Valor := pValor;
  end;
end;

function TGrupo.NovoValor(const pNomeRegistro, pValorRegistro: string): TRegistro;
var
  lIndexRegistro: Integer;
begin
  Result := nil;
  lIndexRegistro := Registros.Find(pNomeRegistro);
  if lIndexRegistro <> -1 then
  begin
    Registros[lIndexRegistro].Valor := pValorRegistro;
    Result := Registros[lIndexRegistro];
  end;
end;

function TGrupo.PesquisaPorNomeValorRegistro(const pNomeGrupo, pNomeRegistro, pValor: string): TGrupo;
var
  lListaNomeCampoPesquisa : array of string;
  lListaValorCampoPesquisa : array of string;
begin
  SetLength(lListaNomeCampoPesquisa, 1);
  lListaNomeCampoPesquisa[0] := pNomeRegistro;

  SetLength(lListaValorCampoPesquisa, 1);
  lListaValorCampoPesquisa[0] := pValor;

  Result := PesquisaPorNomeValorRegistro(pNomeGrupo, lListaNomeCampoPesquisa, lListaValorCampoPesquisa);
end;

function TGrupo.PesquisaPorNomeValorRegistro(const pNomeGrupo: string; pNomesRegistro, pValoresRegistros: array of string): TGrupo;
var
  lIndexRegChave: Integer;
  lIndexRegistro: Integer;
  lItemSelecionado: TGrupo;
  lResultado: TGrupo;
  lRegChave: TRegistro;
  lNomeRegistro: string;
  lValorRegistro: string;
  lValorEncontrado: Boolean;
  lIndexNomeCampo: Integer;
  lValoresEncontrados : array of Boolean;
  lIndexValorEncontrado: Integer;
begin
  SetLength(lValoresEncontrados, length(pNomesRegistro));
  lResultado := nil;
  lItemSelecionado := Primeiro(pNomeGrupo, lIndexRegistro);
  while Assigned(lItemSelecionado) and not Assigned(lResultado) do
  begin
    lValorEncontrado := False;
    for lIndexNomeCampo := 0 to Length(pNomesRegistro) - 1 do
    begin
      lValoresEncontrados[lIndexNomeCampo] := False;
      lNomeRegistro := pNomesRegistro[lIndexNomeCampo];
      lIndexRegChave := lItemSelecionado.Registros.Find(lNomeRegistro);
      if lIndexRegChave <> -1 then
      begin
        lValorRegistro := pValoresRegistros[lIndexNomeCampo];
        lRegChave := lItemSelecionado.Registros[lIndexRegChave];
        lValoresEncontrados[lIndexNomeCampo] := lRegChave.Valor = pValoresRegistros[lIndexNomeCampo];
      end;
    end;

    for lIndexValorEncontrado := 0 to Length(lValoresEncontrados) - 1 do
    begin
      if lIndexValorEncontrado = 0 then
      begin
        lValorEncontrado := lValoresEncontrados[lIndexValorEncontrado];
      end else begin
        lValorEncontrado := lValorEncontrado and lValoresEncontrados[lIndexValorEncontrado];
      end;
    end;

    if lValorEncontrado then
    begin
      lResultado := lItemSelecionado;
    end;

    if lIndexRegistro <> -1 then
    begin
      lItemSelecionado := lItemSelecionado.Proximo(lIndexRegistro, lIndexRegistro);
    end else begin
      lItemSelecionado := nil;
    end;
  end;
  Result := lResultado;
end;

function TGrupo.Primeiro(const pNome: string; out pIndex: Integer; const pPesquisaGruposFilhos: Boolean): TGrupo;
var
  I: Integer;
  lResultado: TGrupo;
begin
  Result := nil;
  pIndex := -1;
  if pNome = FNome then
  begin
    Result := Self;
  end else begin
    pIndex := FGrupos.Find(pNome);
    if pIndex <> -1 then
    begin
      Result := FGrupos[pIndex];
    end else begin
      if pPesquisaGruposFilhos then
      begin
        for I := 0 to FGrupos.Count - 1 do
        begin
          lResultado := FGrupos[I].Primeiro(pNome, pIndex, True);
          if Assigned(lResultado) then
          begin
            Result := lResultado;
            pIndex := I;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TGrupo.Primeiro(const pNome: string): TGrupo;
var
  lIndex: Integer;
begin
  Result := Primeiro(pNome, lIndex, True);
end;

function TGrupo.Proximo(const pAnterior: Integer; out pIndex: Integer): TGrupo;
begin
  pIndex := FMestre.Grupos.Find(FNome, pAnterior);
  if pIndex <> -1 then
  begin
    Result := FMestre.Grupos[pIndex];
  end else begin
    Result := nil;
  end;
end;

procedure TGrupo.SetCriptografado(const Value: Boolean);
begin
  if (Assigned(FMestre)) and (not FMestre.Criptografado) then // Garante que o mestre terá filhos
    inherited;
end;

procedure TGrupo.SetMestre(const Value: TGrupo);
begin
  FMestre := Value;
end;

function TGrupo.Proximo(const pNome : string; const pAnterior : Integer; out pIndex : Integer) : TGrupo;
begin
  pIndex := FGrupos.Find(pNome, pAnterior);
  if pIndex <> -1 then
    Result := FGrupos[pIndex]
  else
    Result := nil;
end;

function TGrupo.ToDataPacket(const pDataSet : TClientDataSet): TItemDataPacket;
begin
  if FItemDTP = nil then
    FItemDTP := TItemDataPacket.Create(Self);

  FItemDTP.Processar(pDataSet);
  Result := FItemDTP;
end;

function TGrupo.ToDataPacket(const pNomeGrupo: string): TItemDataPacket;
var
  lIndex: Integer;
begin
  Result := Primeiro(pNomeGrupo, lIndex).ToDataPacket;
end;

procedure TGrupo.ToFile(const pNomeArquivoXML: string);
var
  lArquivo: TStringList;
begin
  lArquivo := TStringList.Create;
  try
    lArquivo.Add(ToXML);
    lArquivo.SaveToFile(pNomeArquivoXML);
  finally
    lArquivo.Free;
  end;
end;

function TGrupo.ToTexto(const pCaminhoCompleto : Boolean): string;
var
  lGrupo: TGrupo;
  lRegistro: TRegistro;
  I: Integer;

  lListaTexto : string;

  procedure Add(pValor : string);
  begin
    if lListaTexto <> EmptyStr then
    begin
      lListaTexto := lListaTexto + sLineBreak + pValor;
    end else begin
      lListaTexto := pValor;
    end;
  end;

begin
  lListaTexto := EmptyStr;

  if pCaminhoCompleto then
    Add(GetCaminho + '[' + FNome + ']')
  else
    Add('[' + FNome + ']');

  for I := 0 to FGrupos.Count - 1 do
  begin
    lGrupo := Grupos[I];
    Add(lGrupo.ToTexto(pCaminhoCompleto));
  end;

  for I := 0 to FRegistros.Count - 1 do
  begin
    lRegistro := Registros[I];
    Add(lRegistro.ToTexto(pCaminhoCompleto));
  end;
  Result := lListaTexto;
end;

function TGrupo.ToTreeViewString(const pProfundidadeInicial: Integer): string;
var
  lGrupo: TGrupo;
  lRegistro: TRegistro;
  I: Integer;

  lListaTexto : string;

  function GetSeparador : string;
  var
    lIndexProfundidade: Integer;
  begin
    for lIndexProfundidade := pProfundidadeInicial to FProfundidade do
    begin
      Result := Result + #9;
    end;
  end;

  procedure Add(pValor : string);
  begin
    if lListaTexto <> EmptyStr then
      lListaTexto := lListaTexto + sLineBreak + GetSeparador + pValor
    else
      lListaTexto := pValor;
  end;

begin
  lListaTexto := EmptyStr;
  Add(FNome);
  for I := 0 to FGrupos.Count - 1 do
  begin
    lGrupo := Grupos[I];
    Add(lGrupo.ToTreeViewString(pProfundidadeInicial));
  end;

  for I := 0 to FRegistros.Count - 1 do
  begin
    lRegistro := Registros[I];
    Add(lRegistro.ToTexto(False));
  end;
  Result := lListaTexto;
end;

function TGrupo.ToTreeViewString: string;
begin
  Result := ToTreeViewString(FProfundidade);
end;

function TGrupo.ToXML(pNodoMestre : IXMLNode): string;
var
  lDocumento: IXMLDocument;
  lNodoGrupo: IXMLNode;
  I: Integer;
  lGrupo: TGrupo;
  lRegistro: TRegistro;
  lTextoGeral: string;
  lRegistrosGeral: string;
  lChavePublica: string;
  lCriptografado: Boolean;
begin
  if not Assigned(pNodoMestre) then
  begin
    lDocumento := TXMLDocument.Create(nil);
    lDocumento.Active := True;
    if FVersao <> EmptyStr then
    begin
      lDocumento.Version := FVersao;
    end;
    if FCodificacao <> EmptyStr then
    begin
      lDocumento.Encoding := FCodificacao;
    end;
    lNodoGrupo := lDocumento.AddChild(FNome);
  end
  else
  begin
    lDocumento := pNodoMestre.OwnerDocument;
    lNodoGrupo := pNodoMestre.AddChild(FNome);
  end;

  lCriptografado := Criptografado;
  lTextoGeral := EmptyStr;
  lRegistrosGeral := EmptyStr;

  if lCriptografado then
  begin
    if (FGrupos.Count = 1) and (Assigned(FGrupos[0])) then
    begin
      lTextoGeral := FGrupos[0].ToXML;
    end;
  end
  else
  begin
    for I := 0 to FGrupos.Count - 1 do
    begin
      lGrupo := Grupos[I];
      lGrupo.ToXML(lNodoGrupo);
    end;
  end;

  for I := 0 to FRegistros.Count - 1 do
  begin
    lRegistro := Registros[I];
    lRegistrosGeral := lRegistro.ToXML(lNodoGrupo);
    lTextoGeral := lTextoGeral + lRegistrosGeral;
  end;

  if Criptografado then
  begin
    lNodoGrupo.ChildNodes.Clear;
    lNodoGrupo.NodeValue := TRpEncryptionCiffer.Encrypting(lTextoGeral, lChavePublica);
    SetChave(lChavePublica);
  end;

  GerarAtributos(lNodoGrupo);

  lDocumento.XML.LineBreak := EmptyStr;
  Result := lDocumento.XML.Text;
end;

procedure TGrupo.ProcessarValidacaoChave(const pValorCampo, pChavePublica: string);
begin
  if FValidarChave then
    FChaveValida := TRpEncryptionMD5.Validate(pValorCampo, pChavePublica)
  else
    FChaveValida := True;
end;

{ TListaGrupos }

function TListaGrupos.Add(AObject: TGrupo): Integer;
begin
  Result := inherited Add(AObject);
end;

function TListaGrupos.ChavesValidas: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
  begin
    Result := Assigned(Items[I]) and (Items[I].ChaveValida);
    if not Result then
      exit;
  end;
end;

function TListaGrupos.StatusChaves: TStatusListaChavesSet;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    if not (Assigned(Items[I])) then
    begin
      Result[I] := slcNaoInstaciada;
    end else begin
      if (Items[I].ChaveValida) then
      begin
        Result[I] := slcValida;
      end else begin
        Result[I] := slcInvalida;
      end;
    end;
  end;
end;

function TListaGrupos.Extract(Item: TGrupo): TGrupo;
begin
  Result := TGrupo(inherited Extract(Item));
end;

function TListaGrupos.Find(const pNome: string; const pInitIndex : Integer): Integer;
var
  lItem: Integer;
  lCampo: TGrupo;
begin
  lItem := 0;
  Result := -1;
  while (Result = -1) and (lItem < Count) do
  begin
    if (lItem > pInitIndex) then
    begin
      lCampo := Items[lItem];
      if (UpperCase(lCampo.Nome) = UpperCase(pNome)) then
      begin
        Result := lItem;
      end;
    end;
    Inc(lItem);
  end;
end;

function TListaGrupos.First: TGrupo;
begin
  Result := TGrupo(inherited First);
end;

function TListaGrupos.GetItem(Index: Integer): TGrupo;
begin
  try
    Result := TGrupo(inherited GetItem(Index));
  except
    Result := nil;
  end;
end;

function TListaGrupos.IndexOf(AObject: TGrupo): Integer;
begin
  Result := IndexOf(AObject);
end;

procedure TListaGrupos.Insert(Index: Integer; AObject: TGrupo);
begin
  inherited Insert(Index, AObject);
end;

function TListaGrupos.Last: TGrupo;
begin
  Result := TGrupo(inherited Last);
end;

function TListaGrupos.New(const pConjunto: TConjunto; pGrupo: TGrupo): Integer;
var
  lGrupo: TGrupo;
begin
  lGrupo := TGrupo.Create(pConjunto.Nome, pGrupo, pConjunto);
//  lGrupo.ListasVinculadas.Add(Self);
  Result := Add(lGrupo);
end;

function TListaGrupos.QuantidadeItensNil: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if not Assigned(Items[I]) then
    begin
      Inc(Result);
    end;
  end;
end;

function TListaGrupos.New(const pNome: string; pGrupo: TGrupo): Integer;
var
  lItem: Integer;
begin
  lItem := Add(TGrupo.Create(pNome, pGrupo));
//  Items[lItem].ListasVinculadas.Add(Self);
  Result := lItem;
end;

function TListaGrupos.Remove(AObject: TGrupo): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TListaGrupos.SetItem(Index: Integer; AObject: TGrupo);
begin
  inherited SetItem(Index, AObject);
end;

{ TBaseGrupoRegistro }

procedure TBaseGrupoRegistro.CarregarAtributos(const pNodo: IXMLNode);
var
  lIndexAtributo: Integer;
  lAtributo: TItemAtributo;
begin
  if pNodo <> nil then
  begin
    for lIndexAtributo := 0 to pNodo.AttributeNodes.Count - 1 do
    begin
      lAtributo := TItemAtributo.Create;
      lAtributo.Nome := pNodo.AttributeNodes.Nodes[lIndexAtributo].NodeName;
      if VarIsNull(pNodo.AttributeNodes.Nodes[lIndexAtributo].NodeValue) then
      begin
        lAtributo.Valor := EmptyStr;
      end else begin
        lAtributo.Valor := pNodo.AttributeNodes.Nodes[lIndexAtributo].NodeValue;
      end;
      Atributos.Add(lAtributo);
    end;
  end;
end;

constructor TBaseGrupoRegistro.Create(const pNome : string);
begin
  FNome := pNome;
  FAtributos := TListaAtributos.Create;
//  FListasVinculadas := TListasVinculadas.Create(False);
end;

//function TBaseGrupoRegistro.Criptografado(out pChave: string): string;
//begin
//  Result := TRpCriptografiaCifrada.Criptografa(ToXML, pChave);
//end;
//
//function TBaseGrupoRegistro.Criptografado : string;
//begin
//  Result := TRpCriptografiaCifrada.Criptografa(ToXML);
//end;

destructor TBaseGrupoRegistro.Destroy;
begin
  FAtributos.Free;
//  FListasVinculadas.Free;
  inherited;
end;

procedure TBaseGrupoRegistro.GerarAtributos(pNodo: IXMLNode);
var
  lIndexAtributo: Integer;
begin
  if Assigned(pNodo) then
  begin
    for lIndexAtributo := 0 to Atributos.Count - 1 do
    begin
      pNodo.SetAttributeNS(Atributos[lIndexAtributo].Nome, EmptyStr, Atributos[lIndexAtributo].Valor);
    end;
  end;
end;

function TBaseGrupoRegistro.GetAtributos: TListaAtributos;
begin
  Result := FAtributos;
end;

function TBaseGrupoRegistro.SetNomeAmigavel(const pNome: string) : TBaseGrupoRegistro;
begin
  Atributos.Novo(ATRIBUTE_DISPLAY_NAME, pNome);
  Result := Self;
end;

function TBaseGrupoRegistro.NovoAtributo(const pNome, pValor: string): Integer;
begin
  Result := Atributos.Novo(pNome, pValor);
end;

function TBaseGrupoRegistro.ToTexto(const pCaminhoCompleto: Boolean): string;
begin
  Result := FNome;
end;


function TBaseGrupoRegistro.ToXML(pNodoMestre: IXMLNode): string;
begin
  Result := EmptyStr;
end;

function TBaseGrupoRegistro.SetVisivel(const pVisivel: Boolean) : TBaseGrupoRegistro;
begin
  if pVisivel then
  begin
    Atributos.Novo(ATRIBUTE_VISIBLE, 'True');
  end else begin
    Atributos.Novo(ATRIBUTE_VISIBLE, 'False');
  end;
  Result := Self;
end;


function TBaseGrupoRegistro.GetChave: string;
var
  lAtributoCripto: TItemAtributo;
begin
  Result := EmptyStr;
  lAtributoCripto := Atributos.PorNome(ATRIBUTE_CHAVE_PUBLICA);
  if Assigned(lAtributoCripto) then
  begin
    Result := lAtributoCripto.Valor;
  end;
end;

function TBaseGrupoRegistro.GetCriptografado(pNodo: IXMLNode): Boolean;
var
  lAtributoCripto: OleVariant;
begin
  lAtributoCripto := pNodo.Attributes[ATRIBUTE_CRIPTO];
  Result := (not VarIsNull(lAtributoCripto)) and (lAtributoCripto = 'True');
end;

function TBaseGrupoRegistro.GetCriptografado: Boolean;
var
  lAtributoCripto: TItemAtributo;
begin
  lAtributoCripto := Atributos.PorNome(ATRIBUTE_CRIPTO);
  Result := (Assigned(lAtributoCripto)) and (lAtributoCripto.Valor = 'True');
end;


procedure TBaseGrupoRegistro.SetChave(const Valor: string);
var
  lAtributoCripto: TItemAtributo;
begin
  lAtributoCripto := Atributos.PorNome(ATRIBUTE_CHAVE_PUBLICA);
  if not Assigned(lAtributoCripto) then
  begin
    NovoAtributo(ATRIBUTE_CHAVE_PUBLICA, Valor);
  end else begin
    lAtributoCripto.Valor := Valor;
  end;
end;

procedure TBaseGrupoRegistro.SetCriptografado(const Value: Boolean);
var
  lAtributoCripto: TItemAtributo;
  lValor: string;
begin
  if Value then
    lValor := 'True'
  else
  begin
    lValor := 'False';
    SetChave(EmptyStr);
  end;

  lAtributoCripto := Atributos.PorNome(ATRIBUTE_CRIPTO);
  if not Assigned(lAtributoCripto) then
    NovoAtributo(ATRIBUTE_CRIPTO, lValor)
  else
    lAtributoCripto.Valor := lValor;
end;


function TBaseGrupoRegistro.SetCriptografia: TBaseGrupoRegistro;
begin
  Atributos.Novo(ATRIBUTE_CRIPTO, 'True');
  Result := Self;
end;

{ TItemDataPacket }

constructor TItemDataPacket.Create(pGrupo: TGrupo);
begin
  FGrupo := pGrupo;
  FClient := TClientDataSet.Create(nil);
  FProcessado := False;
end;

destructor TItemDataPacket.Destroy;
begin
  FreeAndNil(FClient);
  inherited;
end;

function TItemDataPacket.GetData: OleVariant;
begin
  Result := Null;
  if FProcessado then
    Result := FClient.Data;
end;

function TItemDataPacket.GetString: string;
begin
  Result := EmptyStr;
  if FProcessado then
    Result := FClient.XMLData;
end;

procedure TItemDataPacket.Limpar;
begin
  FProcessado := False;
end;

function TItemDataPacket.Processar(pDataSet : TClientDataSet): Boolean;
var
  lIndexGrupo: Integer;
  lRegistro: TRegistro;
  lListaGruposAcima : TListaGrupos;
  lRegistros: TListaRegistros;
  lIndexRegistro: Integer;
  lCampo: TField;
  lDataSet: TClientDataSet;
begin
  if pDataSet = nil then
  begin
    lDataSet := FClient;
  end else begin
    lDataSet := pDataSet;
  end;

  lDataSet.DisableControls;

  if not FProcessado then
  begin
    try
      lDataSet.Close;
      lDataSet.FieldDefs.Clear;

      for lIndexGrupo := 0 to Grupo.Registros.Count - 1 do
      begin
        lRegistro := Grupo.Registros[lIndexGrupo];
        if lRegistro.Visivel then
        begin
          lDataSet.FieldDefs.Add(lRegistro.Nome, ftString, 150);
        end;
      end;
      lDataSet.CreateDataSet;

      if Assigned(FGrupo.Mestre) then
      begin
        lListaGruposAcima := FGrupo.Mestre.Grupos;
      end else begin
        lListaGruposAcima := FGrupo.Grupos;
      end;
//      lListaGruposAcima := FGrupo.Mestre.Grupos;
      for lIndexGrupo := 0 to lListaGruposAcima.Count - 1 do
      begin
        lDataSet.Append;
        lRegistros := lListaGruposAcima[lIndexGrupo].Registros;
        for lIndexRegistro := 0 to lRegistros.Count - 1 do
        begin
          lRegistro := lRegistros[lIndexRegistro];
          if lRegistro.Visivel then
          begin
            lCampo := lDataSet.FindField(lRegistro.Nome);
            if Assigned(lCampo) then
            begin
              lCampo.DisplayLabel := lRegistro.NomeAmigavel;
              lCampo.Value := lRegistro.Valor;
            end;
          end;
        end;
        lDataSet.Post;
      end;
      FProcessado := True;
    except
      FProcessado := False;
    end;
  end;
  Result := FProcessado;
  lDataSet.EnableControls;
end;

procedure TItemDataPacket.ToDataSet(pDataSet: TClientDataSet);
begin
  Processar(pDataSet);
end;

{ TListaItemDataPacket }

function TListaItemDataPacket.Add(AObject: TItemDataPacket): Integer;
begin
  Result := inherited Add(AObject);
end;

function TListaItemDataPacket.Extract(Item: TItemDataPacket): TItemDataPacket;
begin
  Result := TItemDataPacket(inherited Extract(Item));
end;

function TListaItemDataPacket.Find(const pNome: string): Integer;
var
  lIndex: Integer;
  lItemDTP: TItemDataPacket;
begin
  lIndex := 0;
  Result := -1;
  while (Result = -1) and (lIndex < Count) do
  begin
    lItemDTP := Items[lIndex];
    if lItemDTP.Nome = pNome then
    begin
      Result := lIndex;
    end;
    Inc(lIndex);
  end;
end;

function TListaItemDataPacket.First: TItemDataPacket;
begin
  Result := TItemDataPacket(inherited First);
end;

function TListaItemDataPacket.GetItem(Index: Integer): TItemDataPacket;
begin
  Result := TItemDataPacket(inherited GetItem(Index));
end;

function TListaItemDataPacket.IndexOf(AObject: TItemDataPacket): Integer;
begin
  Result := IndexOf(AObject);
end;

procedure TListaItemDataPacket.Insert(Index: Integer; AObject: TItemDataPacket);
begin
  inherited Insert(Index, AObject);
end;

function TListaItemDataPacket.Last: TItemDataPacket;
begin
  Result := TItemDataPacket(inherited Last);
end;

function TListaItemDataPacket.New(const pNome : string; pItemDTP: TItemDataPacket): Integer;
var
  lIndex: Integer;
  lItemDTP: TItemDataPacket;
begin
  lIndex := Find(pNome);
  if lIndex = -1 then
  begin
    lItemDTP := TItemDataPacket.Create(nil);
    lItemDTP.Nome := pNome;
    lIndex := Add(lItemDTP);
  end;
  Result := lIndex;
end;


function TListaItemDataPacket.Remove(AObject: TItemDataPacket): Integer;
begin
  Result := inherited Remove(AObject);
end;

procedure TListaItemDataPacket.SetItem(Index: Integer; AObject: TItemDataPacket);
begin
  inherited SetItem(Index, AObject);
end;

{ TConjunto }

constructor TConjunto.Create;
begin
  inherited Create(pNome);
  FCampos := TListaCampos.Create;
end;

destructor TConjunto.Destroy;
begin
  FreeAndNil(FCampos);
  inherited;
end;

function TConjunto.NovoAtributo(pCampo : TCampo; const pNome, pValor: string): TItemAtributo;
var
  lIndexCampo: Integer;
  lIndexAtributo: Integer;
begin
  lIndexCampo := FCampos.IndexOf(pCampo);
  if lIndexCampo = -1 then
    raise Exception.Create('O campo não faz parte da lista de campos do conjunto');

  lIndexAtributo := pCampo.Atributos.Novo(pNome, pValor);
  if lIndexAtributo <> -1 then
  begin
    Result := Atributos[lIndexAtributo];
  end else begin
    Result := nil;
  end;
end;

function TConjunto.NovoCampo(const pNome: string): TCampo;
var
  lCampoGerado: Integer;
begin
  lCampoGerado := FCampos.New(pNome, nil);
  Result := FCampos[lCampoGerado];
end;

{ TItemAtributo }

constructor TItemAtributo.Create;
begin
  Limpar;
end;

procedure TItemAtributo.Limpar;
begin
  FNome := EmptyStr;
  FValor := EmptyStr;
end;

{ TListaAtributos }

function TListaAtributos.Add(AObject: TItemAtributo): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TListaAtributos.Clonar(const pAtributos: TListaAtributos);
var
  I: Integer;
begin
  Clear;
  for I := 0 to pAtributos.Count - 1 do
  begin
    Novo(pAtributos[I].Nome, pAtributos[I].Valor);
  end;
end;

function TListaAtributos.GetItem(Index: Integer): TItemAtributo;
begin
  Result := TItemAtributo(inherited GetItem(Index));
end;

function TListaAtributos.IndexOf(AObject: TItemAtributo): Integer;
begin
  Result := IndexOf(AObject);
end;

procedure TListaAtributos.Insert(Index: Integer; AObject: TItemAtributo);
begin
  inherited Insert(Index, AObject);
end;

function TListaAtributos.Novo(const pNome, pValor: string) : Integer;
var
  lAtributo: TItemAtributo;
begin
  lAtributo := TItemAtributo.Create;
  lAtributo.Nome := pNome;
  lAtributo.Valor := pValor;
  Result := Add(lAtributo);
end;

function TListaAtributos.PorNome(const pNome: string): TItemAtributo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if Items[I].Nome = pNome then
    begin
      Result := Items[I];
    end;
  end;
end;

procedure TListaAtributos.SetItem(Index: Integer; AObject: TItemAtributo);
begin
  inherited SetItem(Index, AObject);
end;

{ TEnumeratorGrupo }

constructor TEnumeratorGrupo.Create(const AGrupoMestre: TGrupo; const pNomePesquisa : string);
begin
  FGrupoMestre := AGrupoMestre;
  FNomePesquisa := pNomePesquisa;
  FAtual := nil;
  FIndexAtual := -1;
end;

function TEnumeratorGrupo.MoverAoProximo: Boolean;
var
  lIndex: Integer;
begin
  lIndex := FGrupoMestre.Grupos.Find(FNomePesquisa, FIndexAtual);
  if lIndex <> -1 then
  begin
    FAtual := FGrupoMestre.Grupos[lIndex];
    FIndexAtual := lIndex;
    Result := True;
  end else begin
    Result := False;
  end;
end;

end.
