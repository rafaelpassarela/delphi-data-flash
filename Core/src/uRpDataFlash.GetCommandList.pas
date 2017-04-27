unit uRpDataFlash.GetCommandList;

interface

uses
  SysUtils, uRpDataFlash.Command, uRpDataFlash.Types, uRpDataFlash.Components,
  Classes, uRpSerialization, XMLIntf, uRpDataFlash.DataSetProvider;

type
  TLRDFParametrosInfoComando = class(TCustomSerializableObject)
  private
    FNome: string;
    FTipoValor: TLRDataFlashTipoValorParametro;
    FTipo: TLRDataFlashTipoParametro;
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property Nome : string read FNome write FNome;
    property Tipo : TLRDataFlashTipoParametro read FTipo write FTipo;
    property TipoValor : TLRDataFlashTipoValorParametro read FTipoValor write FTipoValor;
  end;

  TLRDFParametrosTransportList = class(TCustomSerializableList)
  private
    function GetItems(Index: Integer): TLRDFParametrosInfoComando;
  protected
    function GetItemClass : TCustomSerializableObjectClass; override;
  public
    property Items[Index : Integer] : TLRDFParametrosInfoComando read GetItems; default;
    function AddParam : TLRDFParametrosInfoComando;
  end;

  TLRDFInfoComando = class(TCustomSerializableObject)
  private
    FNomeComando: string;
    FListaParametros: TLRDFParametrosTransportList;
    function GetValido: Boolean;
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
    procedure Initialize; override;
    procedure Finalize; override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property NomeComando : string read FNomeComando write FNomeComando;
    property ListaParametros : TLRDFParametrosTransportList read FListaParametros;
    property Valido : Boolean read GetValido;
  end;

implementation

{ TLRDFInfoComando }

procedure TLRDFInfoComando.DoLoadFromNode(const ANode: IXMLNode);
begin
  inherited;
  FromNode('NomeComando', FNomeComando);
  FromNode('ListaParametros', FListaParametros);
end;

procedure TLRDFInfoComando.DoSaveToNode;
begin
  inherited;
  ToNode('NomeComando', FNomeComando);
  ToNode('ListaParametros', FListaParametros);
end;

procedure TLRDFInfoComando.Finalize;
begin
  FreeAndNil( FListaParametros );
  inherited;
end;

procedure TLRDFInfoComando.FromOther(const AOther: ISerializableBase);
var
  lInfo : TLRDFInfoComando absolute AOther;
begin
  inherited;
  Reset;
  FNomeComando := lInfo.NomeComando;
  FListaParametros.FromOther(lInfo.ListaParametros);
end;

function TLRDFInfoComando.GetValido: Boolean;
begin
  Result := Trim(FNomeComando) <> EmptyStr;
end;

procedure TLRDFInfoComando.Initialize;
begin
  inherited;
  FListaParametros := TLRDFParametrosTransportList.Create(Self);
end;

procedure TLRDFInfoComando.Reset;
begin
  inherited;
  FNomeComando := '';
  FListaParametros.Clear;
end;

{ TLRDFParametrosInfoComando }

procedure TLRDFParametrosInfoComando.DoLoadFromNode(const ANode: IXMLNode);
var
  lIntAux: Integer;
begin
  inherited;
  FromNode('Nome', FNome);
  FromNode('TipoValor', lIntAux);
  FTipoValor := TLRDataFlashTipoValorParametro(lIntAux);

  FromNode('Tipo', lIntAux);
  FTipo := TLRDataFlashTipoParametro(lIntAux);
end;

procedure TLRDFParametrosInfoComando.DoSaveToNode;
begin
  inherited;
  ToNode('Nome', FNome);
  ToNode('TipoValor', Ord(FTipoValor));
  ToNode('Tipo', Ord(FTipo));
end;

procedure TLRDFParametrosInfoComando.FromOther(const AOther: ISerializableBase);
var
  lInfo : TLRDFParametrosInfoComando absolute AOther;
begin
  inherited;
  FNome := lInfo.Nome;
  FTipo := lInfo.Tipo;
  FTipoValor := lInfo.TipoValor;
end;

procedure TLRDFParametrosInfoComando.Reset;
begin
  inherited;
  FNome := '';
  FTipo := tpEntrada;
  FTipoValor := tvpInteger;
end;

{ TLRDFParametrosListXML }

function TLRDFParametrosTransportList.AddParam: TLRDFParametrosInfoComando;
begin
  Result := TLRDFParametrosInfoComando.Create(Self);
  Self.Add( Result );
end;

function TLRDFParametrosTransportList.GetItemClass : TCustomSerializableObjectClass;
begin
  Result := TLRDFParametrosInfoComando;
end;

function TLRDFParametrosTransportList.GetItems(
  Index: Integer): TLRDFParametrosInfoComando;
begin
  Result := TLRDFParametrosInfoComando(inherited Items[Index]);
end;

end.
