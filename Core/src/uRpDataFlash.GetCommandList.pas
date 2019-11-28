unit uRpDataFlash.GetCommandList;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  SysUtils, uRpDataFlash.Command, uRpDataFlash.Types, uRpDataFlash.Components,
  Classes, uRpSerialization, XMLIntf, uRpDataFlash.DataSetProvider;

type
  TRpDataFlashParamInfoComando = class(TBaseSerializableObject)
  private
    FNome: string;
    FTipoValor: TRpDataFlashParamValueType;
    FTipo: TRpDataFlashParamType;
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property Nome : string read FNome write FNome;
    property Tipo : TRpDataFlashParamType read FTipo write FTipo;
    property TipoValor : TRpDataFlashParamValueType read FTipoValor write FTipoValor;
  end;

  TRpDataFlashParamTransportList = class(TBaseSerializableList)
  private
    function GetItems(Index: Integer): TRpDataFlashParamInfoComando;
  protected
    function GetItemClass : TBaseSerializableObjectClass; override;
  public
    property Items[Index : Integer] : TRpDataFlashParamInfoComando read GetItems; default;
    function AddParam : TRpDataFlashParamInfoComando;
  end;

  TRpDataFlashInfoCommand = class(TBaseSerializableObject)
  private
    FNomeComando: string;
    FListaParametros: TRpDataFlashParamTransportList;
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
    property ListaParametros : TRpDataFlashParamTransportList read FListaParametros;
    property Valido : Boolean read GetValido;
  end;

implementation

{ TRpDataFlashInfoCommand }

procedure TRpDataFlashInfoCommand.DoLoadFromNode(const ANode: IXMLNode);
begin
  inherited;
  FromNode('NomeComando', FNomeComando);
  FromNode('ListaParametros', FListaParametros);
end;

procedure TRpDataFlashInfoCommand.DoSaveToNode;
begin
  inherited;
  ToNode('NomeComando', FNomeComando);
  ToNode('ListaParametros', FListaParametros);
end;

procedure TRpDataFlashInfoCommand.Finalize;
begin
  FreeAndNil( FListaParametros );
  inherited;
end;

procedure TRpDataFlashInfoCommand.FromOther(const AOther: ISerializableBase);
var
  lInfo : TRpDataFlashInfoCommand absolute AOther;
begin
  inherited;
  Reset;
  FNomeComando := lInfo.NomeComando;
  FListaParametros.FromOther(lInfo.ListaParametros);
end;

function TRpDataFlashInfoCommand.GetValido: Boolean;
begin
  Result := Trim(FNomeComando) <> EmptyStr;
end;

procedure TRpDataFlashInfoCommand.Initialize;
begin
  FListaParametros := TRpDataFlashParamTransportList.Create(Self);
  inherited;
end;

procedure TRpDataFlashInfoCommand.Reset;
begin
  inherited;
  FNomeComando := '';
  FListaParametros.Clear;
end;

{ TRpDataFlashParamInfoComando }

procedure TRpDataFlashParamInfoComando.DoLoadFromNode(const ANode: IXMLNode);
var
  lIntAux: Integer;
begin
  inherited;
  FromNode('Nome', FNome);
  FromNode('TipoValor', lIntAux);
  FTipoValor := TRpDataFlashParamValueType(lIntAux);

  FromNode('Tipo', lIntAux);
  FTipo := TRpDataFlashParamType(lIntAux);
end;

procedure TRpDataFlashParamInfoComando.DoSaveToNode;
begin
  inherited;
  ToNode('Nome', FNome);
  ToNode('TipoValor', Ord(FTipoValor));
  ToNode('Tipo', Ord(FTipo));
end;

procedure TRpDataFlashParamInfoComando.FromOther(const AOther: ISerializableBase);
var
  lInfo : TRpDataFlashParamInfoComando absolute AOther;
begin
  inherited;
  FNome := lInfo.Nome;
  FTipo := lInfo.Tipo;
  FTipoValor := lInfo.TipoValor;
end;

procedure TRpDataFlashParamInfoComando.Reset;
begin
  inherited;
  FNome := '';
  FTipo := tpInput;
  FTipoValor := tvpInteger;
end;

{ TRpDataFlashParamTransportList }

function TRpDataFlashParamTransportList.AddParam: TRpDataFlashParamInfoComando;
begin
  Result := TRpDataFlashParamInfoComando.Create(Self);
  Self.Add( Result );
end;

function TRpDataFlashParamTransportList.GetItemClass : TBaseSerializableObjectClass;
begin
  Result := TRpDataFlashParamInfoComando;
end;

function TRpDataFlashParamTransportList.GetItems(
  Index: Integer): TRpDataFlashParamInfoComando;
begin
  Result := TRpDataFlashParamInfoComando(inherited Items[Index]);
end;

end.
