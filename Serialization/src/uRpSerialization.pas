unit uRpSerialization;

{$I ..\..\Common\src\RpInc.inc}
//  RegisterClassAlias(TXMLCustomObject, 'TXMLCustomObject');

interface

uses
  XMLDoc, XMLIntf, Classes, SysUtils, Variants, DateUtils, uRpStringFunctions,
  {$IFDEF XE3UP}
  System.Generics.Collections,
    {$IFDEF ANDROID}

    {$ELSE}
      System.Contnrs, Dialogs,
    {$ENDIF} // ANDROID
  {$ELSE}
  Contnrs,
  {$ENDIF} // XE3UP
  TypInfo, uRpFields, uRpJSONBase, uRpAlgorithms, DB, StrUtils;

type
  TFileFormat = (ffXML, ffJSON, ffUnknown);

  ETipoBaseUnknown = Exception;

  TFileCustomObject = class;
  TFileCustomObjectClass = class of TFileCustomObject;
  TFileCustomList = class;
  TFileCustomListClass = class of TFileCustomList;

  IFileBase = interface
    ['{FC07098F-E2E1-47F7-A2FD-D72028DB71C3}']
    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True); overload;
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure LoadFromFile(const AFile : string; const AFormat : TFileFormat = ffUnknown);
    procedure LoadFromString(const AString : string; const AFormat : TFileFormat = ffUnknown);
    procedure LoadFromJSONString(const AJSONString : string);
    procedure LoadFromXMLString(const AXMLString : string);

    procedure SetFormatType(const Value : TFileFormat);
    function GetFormatType : TFileFormat;

    function GetArquivo: IXMLDocument;
    function GetNode: IXMLNode;
    function GetJSonObject: IJSONObject;
    function GetNodeName: string;
    procedure SetNodeName(const Value : string);
    function GetRaiseExceptions : Boolean;
    procedure SetParent(const Value: TObject);
    function GetParent: TObject;
    function GetClassName : string;

    procedure SetFileName(const Value : string);
    function GetFileName : string;

    procedure SetProxyMode(const AValue : Boolean);
    function GetProxyMode : Boolean;

    procedure SetIncludeClassName(const AValue : Boolean);
    function GetIncludeClassName : Boolean;

    procedure Save;
    procedure SaveToFile(const AFile : string; const AFormat : TFileFormat = ffUnknown);
    function SaveToXmlString : string;
    function SaveToJSONString : string;
    function SaveFileToXmlString : string;
    function GetXMLDoc(const AIncludeParent : Boolean = True) : IXMLDocument;

    property Arquivo : IXMLDocument read GetArquivo;
    property Node : IXMLNode read GetNode;
    property Parent : TObject read GetParent write SetParent;
    property FileName : string read GetFileName write SetFileName;
    property FormatType : TFileFormat read GetFormatType write SetFormatType;
    property JSonObject : IJSONObject read GetJSonObject;
    property ProxyMode : Boolean read GetProxyMode write SetProxyMode;
    property NodeName : string read GetNodeName write SetNodeName;
    property IncludeClassName : Boolean read GetIncludeClassName write SetIncludeClassName;
  end;

  IRpFileSupport = interface
    ['{16325BDB-62C8-433B-925D-445803DF0ECF}']
    function GetFileController : IFileBase;
    function GetFileNodeName: string;
  end;

  IFileBaseHelper = interface
    ['{539FE1C3-E805-491A-8AF2-E4411DD2B7D9}']
    function DoSave(const ADataComponent : TComponent) : Boolean;
    function DoLoad(const ADataComponent : TComponent) : Boolean;
    function DoDelete(const ADataComponent : TComponent) : Boolean;
    function DoExecute(const ADataComponent : TComponent) : Boolean;
  end;

  TFileCustomObject = class(TInterfacedPersistent, IFileBase)
  private
    // XML
    FArquivo : IXMLDocument;
    FNode : IXMLNode;
    // JSON
    FJSonObject : IJSONObject;

    FNodeName : string;
    FParent: TObject;
    FFileName: string;
    FFormatType: TFileFormat;
    FProxyMode : Boolean;
    FLastError: string;
    FIncludeClassName: Boolean;
    function GetParentFileName : string;
    function InternalGetField(const AFieldName : string) : TRpFieldsBase;
    function GetNodeName: string;
    procedure SetNodeName(const Value: string);
    procedure InternalGravarJSON(const AFieldName : string; const AValue : Variant);
    procedure InternalGravarNodeProxy(const AFieldName : string; const AValue : Variant;
      const AAttributeValue : string = '');
    procedure VerificarDependenciaPropriedades(const AClassParent : TFileCustomObjectClass);
    procedure InitializeJson;
  protected
    procedure SetFormatType(const Value: TFileFormat);
    function GetFormatType: TFileFormat;
    function GetArquivo: IXMLDocument;
    function GetParent: TObject;
    procedure SetParent(const Value: TObject);
    function GetNode: IXMLNode;
    function GetJSonObject: IJSONObject;
    procedure SetProxyMode(const AValue : Boolean);
    function GetProxyMode : Boolean;
    procedure SetIncludeClassName(const AValue : Boolean);
    function GetIncludeClassName : Boolean;
    // nativos
    procedure FromNode(const AFieldName : string; out AValue : Word); overload;
    procedure FromNode(const AFieldName : string; out AValue : Char); overload;
    procedure FromNode(const AFieldName : string; out AValue : Boolean); overload;
    procedure FromNode(const AFieldName : string; out AValue : Integer); overload;
    procedure FromNode(const AFieldName : string; out AValue : String; const ABase64 : Boolean = False); overload;
    procedure FromNode(const AFieldName : string; out AValue : TDateTime); overload;
    procedure FromNode(const AFieldName : string; out AValue : TTime); overload;
    procedure FromNode(const AFieldName : string; out AValue : Double); overload;
    procedure FromNode(const AFieldName : string; const AFieldClass : TStrings); overload;
    // sets e enumerados
    procedure FromNode(const AFieldName : string; ASetTypeInfo : PTypeInfo; out ASetVar); overload;
    procedure FromNode(const AFieldName : string; out AEnumOrdValue : Cardinal; ATypeInfo : PTypeInfo); overload;
    // outros objetos com suporte a geracao de arquivo
    procedure FromNode(const AFieldName : string; const AItem : TFileCustomObject); overload;
    procedure FromNode(const AFieldName : string; const AList : TFileCustomList); overload;

    procedure FromNode(const AFieldName : string; out AItem : IFileBase); overload;
    procedure FromNode(const ANode: IXMLNode;     out AItem : IFileBase); overload;
    procedure FromNode(const AJSon: IJSONObject;  out AItem : IFileBase); overload;

    function FromNodeAsInterface(const ANode : IXMLNode) : IFileBase; overload;
    function FromNodeAsInterface(const AFieldName : string) : IFileBase; overload;

    procedure ToNode(const AFieldName : string; const AValue : Variant); overload;
    procedure ToNode(const AFieldName : string; const AFieldClass : TStrings); overload;
    procedure ToNode(const AFieldName : string; const AValue : string; const ABase64 : Boolean); overload;
    procedure ToNode(const AFieldName : string; const AItem : IFileBase); overload;
    procedure ToNode(const AFieldName : string; ASetTypeInfo : PTypeInfo; const ASetValue); overload;
    procedure ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo); overload;
    procedure ToNode(const AFieldName : string; const AList : TFileCustomList); overload;
    procedure ToNode(const AFieldName : string; const AItem : IFileBase; const AGuid : TGuid); overload;
    procedure ToNode(const AFieldName : string; const AList : TInterfaceList; const AGuid : TGuid); overload;

    procedure DoSaveToNode; virtual; abstract;
    procedure DoLoadFromNode(const ANode : IXMLNode); virtual; abstract;

    procedure SetFileName(const Value : string);
    function GetFileName : string;

    function GetRaiseExceptions: Boolean;

    procedure Initialize; virtual;
    procedure Finalize; virtual;

    procedure Save;
    procedure LoadFromXmlString(const AXmlString : string); virtual;
    procedure LoadFromJSONString(const AJSONString : string); virtual;
  public
    constructor Create(AOwner: TObject; const ANodeName : string = ''); reintroduce; virtual;
    destructor Destroy; override;

    procedure Reset; virtual; abstract;
    procedure FromOther(const AOther : IFileBase); virtual; abstract;
    procedure Assign(const AOther : IFileBase); reintroduce;
    procedure SetLastError(const AError : string);

    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True);
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure SaveToFile(const AFile : string; const AFormat : TFileFormat = ffUnknown); virtual;

    // XML
    function SaveToXmlString : string; virtual;
    function SaveFileToXmlString : string; virtual;
    // JSON
    function SaveToJSONString : string; virtual;

    procedure LoadFromFile(const AFile : string; const AFormat : TFileFormat = ffUnknown); virtual;
    procedure LoadFromString(const AString : string; const AFormat : TFileFormat = ffUnknown); virtual;

    function GetXMLDoc(const AIncludeParent : Boolean = True) : IXMLDocument;
    function GetClassName: string;

    property Node : IXMLNode read GetNode;
    property Arquivo : IXMLDocument read GetArquivo;
    property JSonObject : IJSONObject read GetJSonObject;

    property Parent : TObject read GetParent write SetParent;
    property FileName : string read GetFileName write SetFileName;
    property NodeName : string read GetNodeName write SetNodeName;
    property FormatType : TFileFormat read GetFormatType write SetFormatType;
    property IncludeClassName : Boolean read GetIncludeClassName write SetIncludeClassName default True;
    property ProxyMode : Boolean read GetProxyMode write SetProxyMode;
    property LastError : string read FLastError;

    class function CreateFromNode(const ANode : IXMLNode; const AOwner : TFileCustomObject) : TFileCustomObject;
    class function CreateFromJson(const AJSonObject : IJSONObject; const AOwner : TFileCustomObject) : TFileCustomObject;
    class function CreateFromXML(const AXMLString : string; const AOwner : TFileCustomObject) : TFileCustomObject;
  end;

  TFileCustomList = class(TFileCustomObject)
  private
    FItens : TObjectList{$IFDEF XE3UP}<TFileCustomObject>{$ENDIF};
    procedure InitList;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    function GetItemClassName: string;
  protected
    function GetItem(AIndex: Integer): TFileCustomObject;
    function GetItemClass : TFileCustomObjectClass; virtual; abstract;

    procedure LoadFromXmlString(const AXmlValue : string); override;
    procedure LoadFromJSONString(const AJSONString : string); override;
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;

    procedure Initialize; override;
    procedure Finalize; override;
  public
    constructor Create(AOwner: TObject; const ANodeName : string = '';
      const AOwnsObjects: Boolean = True); reintroduce; virtual;
    destructor Destroy; override;

    function Add(const ACustomFileObject : TFileCustomObject) : Integer;
    function Insert(const ACustomFileObject : TFileCustomObject; const AIndex : Integer = -1) : Integer;
    function Delete(const AIndex : Integer) : Boolean;
    function Remove(const ACustomFileObject : TFileCustomObject) : Integer;
    function Count : Integer;

    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure Clear;
    procedure Reset; override;
    procedure FromOther(const AOther: IFileBase); override;

    property Items[AIndex : Integer] : TFileCustomObject read GetItem; default;
    property OwnsObjects : Boolean read GetOwnsObjects write SetOwnsObjects;
    property ItemClassName : string read GetItemClassName;
  end;

  TFileCustomObjectEx = class(TFileCustomObject, IFileBaseHelper)
  protected
    function DoDelete(const ADataComponent: TComponent): Boolean; virtual;
    function DoSave(const ADataComponent: TComponent): Boolean; virtual;
    function DoExecute(const ADataComponent: TComponent): Boolean; virtual;
    function DoLoad(const ADataComponent: TComponent): Boolean; virtual;
  end;

  TFileClassRegisterItem = class
  private
    FFileClass: TFileCustomObjectClass;
    FFileClassName: string;
  public
    property FileClass : TFileCustomObjectClass read FFileClass write FFileClass;
    property FileClassName : string read FFileClassName write FFileClassName;
  end;

  TFileClassRegister = class({$IFDEF XE3UP} System.Contnrs.TObjectList {$ELSE} Contnrs.TObjectList {$ENDIF})
  protected
    function GetItem(const Index : Integer) : TFileClassRegisterItem;
  public
    class var InternalClassRegister: TFileClassRegister;
    procedure Registrar(const AClass : TFileCustomObjectClass);
    function GetClass(const AClassName : string) : TFileCustomObjectClass; overload;
    function GetClass(const AIndex : Integer) : TFileCustomObjectClass; overload;
    property Items[const Index: Integer]: TFileClassRegisterItem read GetItem; default;
  end;

  FileClassRegistrer = class
  public
    class procedure Destruir;
    class procedure Registrar(const AClass : TFileCustomObjectClass);
    class function GetClass(const AClassName : string) : TFileCustomObjectClass; overload;
    class function GetClass(const AIndex : Integer) : TFileCustomObjectClass; overload;
    class function Count : Integer;
//    class procedure Instanciar(const AClass : TXMLCustomObjectClass; const ANomeInstancia : string);
  end;

  TRpBaseFileController = class(TFileCustomObject)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: IFileBase); override;
  end;

  // Usado para controlar o save dos forms, panels, groupBox, etc... que sejam
  // parent de outro componente
  TRpContainerFileController = class(TRpBaseFileController)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  end;

  // classe para listas (TList, TObjectList, TCollection...)
  TRpListFileController = class(TRpBaseFileController)
  private
    procedure LoadListFromJsonObject(const AJsonString : string);
  protected
    function GetNewItem(out AObject : TObject) : Boolean; virtual; abstract;
    function ClearList : Boolean;
    procedure DoLoadItem(const ANode : IXMLNode);
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  end;

const
  C_XML_ENCODING = 'iso-8859-1';
  C_DEFAULT_FORMAT = ffJSON;

implementation

{ TXMLCustomObject }

procedure TFileCustomObject.Assign(const AOther: IFileBase);
var
  lNodo: IXMLNode;
begin
  Reset;

  Self.FFormatType := C_DEFAULT_FORMAT;
  AOther.FormatType := C_DEFAULT_FORMAT;

  AOther.SaveToNode;
  lNodo := AOther.Node.CloneNode(True);
  LoadFromNode(lNodo);
end;

constructor TFileCustomObject.Create(AOwner: TObject; const ANodeName: string);
begin
  FFormatType := C_DEFAULT_FORMAT;
  FIncludeClassName := True;
  FNode := nil;
  FFileName := EmptyStr;
  FLastError := EmptyStr;
  FNodeName := ANodeName;

  if FNodeName = EmptyStr then
    FNodeName := Self.ClassName;

  inherited Create;
  FParent := AOwner;
  FProxyMode := False;
  Initialize;
end;

class function TFileCustomObject.CreateFromJson(const AJSonObject: IJSONObject;
  const AOwner: TFileCustomObject): TFileCustomObject;
var
  lClasse: TFileCustomObjectClass;
  lNomeClass: string;
  lPair: TJSONPair;
begin
  lPair := AJSonObject.Get('ClassName');
  if Assigned(lPair) then
    lNomeClass := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll])
  else
  begin
    if Assigned(AOwner) and (AOwner is TFileCustomList) then
      lNomeClass := TFileCustomList(AOwner).GetItemClass.ClassName;
  end;

  if lNomeClass = '' then
    raise Exception.Create('Não foi encontrado o identificador de classe !');

  lClasse := FileClassRegistrer.GetClass(lNomeClass);
  if lClasse = nil then
    raise Exception.Create('Classe ' +  lNomeClass + ' não registrada !');
  if not lClasse.InheritsFrom(TFileCustomObject) then
    raise Exception.Create('Classe ' + lNomeClass + ' inválida !');

  Result := lClasse.Create(AOwner);
  Result.LoadFromJSONString( AJSonObject.ToString );
end;

class function TFileCustomObject.CreateFromNode(const ANode: IXMLNode;
  const AOwner: TFileCustomObject): TFileCustomObject;
var
  lClasse: TFileCustomObjectClass;
  lNomeClass: string;
begin
  if (ANode.Attributes['ClassName'] <> Null) and (ANode.Attributes['ClassName'] <> Unassigned) then
    lNomeClass := ANode.Attributes['ClassName'];

  if (lNomeClass = EmptyStr) and (Assigned(AOwner)) and (AOwner is TFileCustomList) then
    lNomeClass := TFileCustomList(AOwner).GetItemClass.ClassName;

  lClasse := FileClassRegistrer.GetClass(lNomeClass);
  if lClasse = nil then
    raise Exception.Create('Classe ' +  lNomeClass + ' não registrada !');
  if not lClasse.InheritsFrom(TFileCustomObject) then
    raise Exception.Create('Classe ' + lNomeClass + ' inválida !');

  Result := lClasse.Create(AOwner, ANode.NodeName);
  Result.LoadFromNode(ANode);
end;

class function TFileCustomObject.CreateFromXML(const AXMLString : string;
  const AOwner: TFileCustomObject): TFileCustomObject;
var
  lNomeClass: string;
  lClasse: TFileCustomObjectClass;
begin
  if Assigned(AOwner) and (AOwner is TFileCustomList) then
    lNomeClass := TFileCustomList(AOwner).GetItemClassName
  else
    lNomeClass := Self.ClassName;

  if lNomeClass = '' then
    raise Exception.Create('Não foi encontrado o identificador de classe !');

  lClasse := Self;

  Result := lClasse.Create(nil);
  Result.LoadFromXmlString(AXmlString);
end;

destructor TFileCustomObject.Destroy;
begin
  Finalize;
  if FJSonObject <> nil then
    FJSonObject := nil;
  inherited;
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: Boolean);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsBoolean;
  FreeAndNil(lField);
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: Integer);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil(lField);
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: Word);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil( lField );
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: Char);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  if lField.AsString = EmptyStr then
    AValue := #0
  else
    AValue := lField.AsString[1];
  FreeAndNil(lField);
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: Double);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsFloat;
  FreeAndNil( lField );
end;

procedure TFileCustomObject.Finalize;
begin
// dummy
end;

procedure TFileCustomObject.FromNode(const AFieldName : string; out AItem: IFileBase);
var
  lNode: IXMLNode;
  lPair: TJSONPair;
  lTmpObject: TJSONObject;
begin
  if FFormatType = ffUnknown then
    raise Exception.Create('Formato de arquivo não identificado.');

  if FFormatType = ffXML then
  begin
    lNode := FNode.ChildNodes.FindNode(AFieldName);
    if lNode <> nil then
      FromNode(lNode, AItem);
  end
  else
    if FFormatType = ffJSON then
    begin
      lPair := FJSonObject.Get(AFieldName);
      if lPair <> nil then
      begin
        try
          lTmpObject := TJsonObject( lPair.JsonValue );
        except
          lTmpObject := nil;
        end;

        if lTmpObject <> nil then
          FromNode(lTmpObject, AItem);
      end;
    end;
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; const AItem: TFileCustomObject);
var
  lNode: IXMLNode;
  lNodeName: string;
  lPair: TJSONPair;
  lTmpObject: TJSONObject;
  lJsonStr: string;
begin
  AItem.FNodeName := AFieldName;

  if FFormatType = ffXML then
  begin
    lNode := FNode.ChildNodes.FindNode(AItem.FNodeName);
    AItem.FormatType := ffXML;
    if lNode <> nil then
      AItem.LoadFromNode(lNode);
  end
  else // objeto dentro de um objeto
    if FFormatType = ffJSON then
    begin
      lNodeName := AItem.FNodeName;
      AItem.FormatType := ffJSON;

      lPair := FJSonObject.Get(lNodeName);
      try
        lTmpObject := TJsonObject( lPair.JsonValue );
      except
        lTmpObject := nil;
      end;

      if lTmpObject <> nil then
      begin
        lJsonStr := lTmpObject.ToString;
        if lJsonStr = 'null' then
          Exit;
        if lJsonStr[1] <> '{' then
          lJsonStr := '{' + lJsonStr + '}';

        AItem.LoadFromJSONString(lJsonStr);
      end;
    end
    else
      raise ETipoBaseUnknown.CreateFmt('Não é possível determinar o tipo do objeto %s.', [AItem.ClassName]);
end;

procedure TFileCustomObject.FromNode(const ANode: IXMLNode; out AItem: IFileBase);
var
  lClasse: TFileCustomObjectClass;
  lNodoClasse: IXMLNode;
begin
  lNodoClasse := ANode.AttributeNodes.FindNode('ClassName');
  if lNodoClasse <> nil then
  begin
    lClasse := FileClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lClasse <> nil then
    begin
      AItem := lClasse.Create(Self);
      AItem.LoadFromNode(ANode);
    end;
  end;
end;

procedure TFileCustomObject.FromNode(const AFieldName: string;
  out AValue: String; const ABase64: Boolean);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsString;

  if ABase64 and (AValue <> '') then
    AValue := Algorithms.Base64DecompressedString(AValue);

  FreeAndNil(lField);
end;

procedure TFileCustomObject.FromNode(const AFieldName : string; const AList: TFileCustomList);
var
  lNode: IXMLNode;
  lPair: TJSONPair;
  lJsonStr: string;
  lTmpObject: TJSONObject;
begin
  AList.NodeName := AFieldName;
  if FFormatType = ffXML then
  begin
    lNode := FNode.ChildNodes.FindNode(AList.FNodeName);
    if lNode <> nil then
      AList.LoadFromNode(lNode);
  end
  else
  begin
    lPair := FJSonObject.Get(AList.FNodeName);
    if lPair <> nil then
    begin
      try
        lTmpObject := TJsonObject( lPair.JsonValue );
      except
        lTmpObject := nil;
      end;

      if lTmpObject <> nil then
      begin
        lJsonStr := lTmpObject.ToString;
        if lJsonStr = 'null' then
          Exit;
        if (lJsonStr[1] <> '{') and (lJsonStr[1] <> '[') then
          lJsonStr := '{' + lJsonStr + '}';

        AList.LoadFromJSONString(lJsonStr);
      end;
    end;
  end;
end;

procedure TFileCustomObject.FromNode(const AJSon: IJSONObject; out AItem: IFileBase);
var
  lClasse: TFileCustomObjectClass;
  lPair: TJSONPair;
  lNomeClass: string;
begin
  lPair := AJSon.Get('ClassName');
  if Assigned(lPair) then
    lNomeClass := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll])
  else
    lNomeClass := EmptyStr;

  if lNomeClass <> EmptyStr then
  begin
    lClasse := FileClassRegistrer.GetClass(lNomeClass);
    if lClasse <> nil then
    begin
      AItem := lClasse.Create(Self);
      AItem.LoadFromString( AJSon.ToString );
    end;
  end;
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; const AFieldClass: TStrings);
var
  lAux : string;
  lTmp: TStringList;
  i: Integer;
  lField : TRpFieldsBase;
begin
  if FFormatType = ffXML then
    FromNode(AFieldName, lAux)
  else
  begin
    lField := InternalGetField(AFieldName);
    lAux := lField.AsString;
    FreeAndNil(lField);
  end;

  AFieldClass.Clear;

  lAux := Trim(lAux);
  if lAux <> '[]' then
  begin
    Delete(lAux, 1, 1);
    Delete(lAux, Length(lAux), 1);

    lTmp := TStringList.Create;
    try
      lTmp.Delimiter := ',';
      lTmp.DelimitedText := lAux;

      for i := 0 to lTmp.Count - 1 do
        AFieldClass.Add( lTmp[i] );
    finally
      FreeAndNil(lTmp);
    end;
  end;
end;

function TFileCustomObject.FromNodeAsInterface(const ANode: IXMLNode): IFileBase;
var
  lClasse: TFileCustomObjectClass;
  lNodoClasse: IXMLNode;
begin
  lNodoClasse := ANode.AttributeNodes.FindNode('ClassName');
  if lNodoClasse <> nil then
  begin
    lClasse := FileClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lClasse <> nil then
    begin
      Result := lClasse.Create(Self);
      Result.LoadFromNode(ANode);
    end;
  end;
end;

function TFileCustomObject.FromNodeAsInterface(const AFieldName: string): IFileBase;
var
  lNode: IXMLNode;
begin
  lNode := FNode.ChildNodes.FindNode(AFieldName);
  if lNode <> nil then
    Result := FromNodeAsInterface(lNode);
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: TTime);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := Frac(lField.AsDateTime);
  FreeAndNil( lField );
end;

procedure TFileCustomObject.FromNode(const AFieldName : string; out AEnumOrdValue : Cardinal; ATypeInfo : PTypeInfo);
var
  lStr : string;
begin
  FromNode(AFieldName, lStr);
  if lStr = '' then
    AEnumOrdValue := 0
  else
    AEnumOrdValue := TRpStrings.StrToEnumOrd(ATypeInfo, lStr);
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; out AValue: TDateTime);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsDateTime;

  // 01/01/1900
  if AValue = 2 then
    AValue := 0;

  FreeAndNil( lField );
end;

function TFileCustomObject.GetArquivo: IXMLDocument;
begin
  Result := FArquivo;
end;

function TFileCustomObject.GetClassName: string;
begin
  Result := Self.ClassName;
end;

function TFileCustomObject.GetFileName: string;
begin
  Result := FFileName;
end;

function TFileCustomObject.GetFormatType: TFileFormat;
begin
  Result := FFormatType;
end;

function TFileCustomObject.GetIncludeClassName: Boolean;
begin
  Result := FIncludeClassName;
end;

function TFileCustomObject.GetJSonObject: IJSONObject;
begin
  Result := FJSonObject;
end;

function TFileCustomObject.GetNode: IXMLNode;
begin
  Result := FNode;
end;

function TFileCustomObject.GetNodeName: string;
begin
  Result := FNodeName;
end;

function TFileCustomObject.GetParent: TObject;
begin
  Result := FParent;
end;

function TFileCustomObject.GetParentFileName: string;
var
  lParent: TObject;
  lIntf: IFileBase;
  lIntf2: IRpFileSupport;
begin
  lParent := Self;
  Result := '';
  while (Result = '') and (lParent <> nil) do
  begin
    if lParent.InheritsFrom(TFileCustomObject) then
    begin
      Result := TFileCustomObject(lParent).FFileName;
      lParent := TFileCustomObject(lParent).Parent;
    end
    else
      if Supports(lParent, IFileBase, lIntf) then
      begin
        Result := lIntf.FileName;
        lParent := lIntf.Parent;
      end
      else
        if Supports(lParent, IRpFileSupport, lIntf2) then
        begin
          Result := lIntf2.GetFileController.FileName;
          lParent := lIntf2.GetFileController.Parent;
        end
        else
          lParent := nil;

    if FFileName = Result then
      Exit;
  end;
end;

function TFileCustomObject.GetProxyMode: Boolean;
begin
  Result := FProxyMode;
end;

function TFileCustomObject.GetRaiseExceptions: Boolean;
begin
  Result := False;
end;

function TFileCustomObject.GetXMLDoc(const AIncludeParent: Boolean): IXMLDocument;
begin
  SaveToNode('', AIncludeParent);
  Result := FArquivo;
end;

procedure TFileCustomObject.Initialize;
begin
  try
    Reset;
  except
    on E:EAbstractError do
      raise Exception.CreateFmt('Mótodo "Reset" da classe "%s" não está implementado.', [Self.ClassName]);
    on E:Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TFileCustomObject.InitializeJson;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;

  if FIncludeClassName or FProxyMode then
    FJSonObject.AddPair('ClassName', Self.ClassName);
end;

function TFileCustomObject.InternalGetField(const AFieldName: string): TRpFieldsBase;
begin
  if FFormatType = ffXML then
    Result := TXMLFieldNode.Create(FNode, AFieldName)
  else
    Result := TJSONFieldNode.Create(FJSonObject, AFieldName);
end;

procedure TFileCustomObject.InternalGravarJSON(const AFieldName: string; const AValue: Variant);
var
  lTipo: TFieldType;

  procedure DoInteger(const AIntValue : Integer);
  begin
    FJSonObject.AddPair(AFieldName, TJSONNumber.Create(AValue));
  end;

  procedure DoDouble(const AFloatValue : Double);
  begin
    FJSonObject.AddPair(AFieldName, TJSONString.Create( TJSONFieldNode.GetDoubleFormated(AFloatValue) ));
  end;

  procedure DoDateTime(const ADateValue : TDateTime);
  begin
    FJSonObject.AddPair(AFieldName, TJSONString.Create( TJSONFieldNode.GetDateFormated(ADateValue) ));
  end;

  procedure DoBoolean(const ABoolValue : Boolean);
  begin
    if ABoolValue then
      FJSonObject.AddPair(AFieldName, TJSONTrue.Create )
    else
      FJSonObject.AddPair(AFieldName, TJSONFalse.Create);
  end;

  procedure DoString(const AStrValue : String);
  var
    lValue : string;
  begin
    lValue := StringReplace(AStrValue, '\', '\\', [rfReplaceAll]);
    lValue := StringReplace(lValue, sLineBreak, '\n', [rfReplaceAll]);
    lValue := StringReplace(lValue, '"', '\"', [rfReplaceAll]);

    FJSonObject.AddPair(AFieldName, TJSONString.Create( lValue ));
  end;

begin
  lTipo := VarTypeToDataType(VarType(AValue));
  case lTipo of
    DB.ftSmallInt, DB.ftInteger, DB.ftLongWord, DB.ftShortint :
      DoInteger(AValue);

    DB.ftBCD, DB.ftFloat, DB.ftLargeInt, DB.ftCurrency, DB.ftExtended :
      DoDouble(AValue);

    DB.ftDateTime, DB.ftTimeStamp, DB.ftTime :
      DoDateTime(AValue);

    DB.ftBoolean :
      DoBoolean(AValue);

    DB.ftString, DB.ftWideString, DB.ftFixedChar, DB.ftFixedWideChar :
      DoString(AValue);
  else
    raise ETipoBaseUnknown.CreateFmt('O valor informado para o campo %s não é suportado (%d).', [AFieldName, Ord(lTipo) ]);
  end;
end;

procedure TFileCustomObject.InternalGravarNodeProxy(const AFieldName: string;
  const AValue: Variant;const AAttributeValue : string);
var
  lNode: IXMLNode;
begin
  case VarTypeToDataType(VarType(AValue)) of
    DB.ftSmallInt, DB.ftInteger :
      FNode[AFieldName] := 'Integer';

    DB.ftBCD, DB.ftFloat, DB.ftLargeInt, DB.ftCurrency :
      FNode[AFieldName] := 'Double';

    DB.ftDateTime, DB.ftTimeStamp, DB.ftTime :
      FNode[AFieldName] := 'TDateTime';

    DB.ftBoolean :
      FNode[AFieldName] := 'Boolean';

    DB.ftString, DB.ftWideString, DB.ftFixedChar, DB.ftFixedWideChar :
      FNode[AFieldName] := 'String';
  else
    raise ETipoBaseUnknown.CreateFmt('O valor informado para o campo %s não é suportado.', [AFieldName]);
  end;

  if AAttributeValue <> EmptyStr then
  begin
    lNode := FNode.ChildNodes.FindNode(AFieldName);
    if lNode <> nil then
      lNode.Attributes['info'] := AAttributeValue;
  end;
end;

procedure TFileCustomObject.LoadFromFile(const AFile: string; const AFormat : TFileFormat = ffUnknown);
var
  lNodeRoot: IXMLNode;
  lFormato : TFileFormat;
  lFileStr : TStrings;
  lChar: string;

  procedure InternalLoadFile;
  begin
    lFileStr := TStringList.Create;
    lFileStr.LoadFromFile( AFile );
    lFileStr.Text := Trim(lFileStr.Text);
  end;

begin
  lFileStr := nil;
  FFileName := AFile;

  try
    if AFormat = ffUnknown then
    begin
      InternalLoadFile;
      lChar := Copy(lFileStr.Text, 1, 1);
      if lChar = '<' then
        lFormato := ffXML
      else
        if lChar = '{' then
          lFormato := ffJSON
        else
          raise ETipoBaseUnknown.Create('A string informada não pode ser identificada durante o load.');
    end
    else
      lFormato := AFormat;

    if lFormato = ffXML then
    begin
      FFormatType := ffXML;

      FArquivo := TXMLDocument.Create(nil);
      FArquivo.LoadFromFile(FFileName);
      lNodeRoot := FArquivo.ChildNodes.FindNode('root');
      LoadFromNode(lNodeRoot.ChildNodes.FindNode(FNodeName));
      lNodeRoot := nil;
    end
    else
      if lFormato = ffJSON then
      begin
        if lFileStr = nil then
          InternalLoadFile;
        LoadFromJSONString( lFileStr.Text );
      end
      else
        raise ETipoBaseUnknown.CreateFmt('O tipo do arquivo não foi reconhecido para efetuar load. %s%s', [sLineBreak, AFile]);
  finally
    if lFileStr <> nil then
      FreeAndNil( lFileStr );
  end;
end;

procedure TFileCustomObject.LoadFromJSONString(const AJSONString: string);
begin
  FFormatType := ffJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse( BytesOf(AJSONString), 0);
  DoLoadFromNode(nil);
end;

procedure TFileCustomObject.LoadFromNode(const ANode: IXMLNode);
var
  lIsNil : Boolean;
begin
  lIsNil := ANode = nil;

  if lIsNil and GetRaiseExceptions then
    raise Exception.Create('Node não encontrado!!');

  if (not lIsNil) and GetRaiseExceptions and (ANode.Attributes['ClassName'] <> Self.ClassName) then
  begin
    raise Exception.CreateFmt(
      'Classe de carga XML (%s) incompatível com o node atual (%s)!',
      [ANode.Attributes['ClassName'], Self.ClassName]);
  end;

  if not lIsNil then
  begin
    FFormatType := ffXML;
    FNode := ANode;
    DoLoadFromNode(FNode);
  end;
end;

procedure TFileCustomObject.LoadFromString(const AString: string; const AFormat: TFileFormat);
var
  lFormato : TFileFormat;
  lChar: string;
  lStr: string;
begin
  lStr := Trim( AString );

  if AFormat = ffUnknown then
  begin
    lChar := Copy(lStr, 1, 1);
    if lChar = '<' then
      lFormato := ffXML
    else
      if lChar = '{' then
        lFormato := ffJSON
      else
        raise ETipoBaseUnknown.Create('A string informada não pode ser identificada durante o load.');
  end
  else
    lFormato := AFormat;

  if lFormato = ffXML then
    LoadFromXmlString(lStr)
  else
    LoadFromJSONString(lStr);
end;

procedure TFileCustomObject.LoadFromXmlString(const AXmlString: string);
var
  lNodeRoot: IXMLNode;
  lStream: TStringStream;
begin
  FFormatType := ffXML;
  lStream := TStringStream.Create(AXmlString);
  try
    FArquivo := TXMLDocument.Create(nil);
    FArquivo.LoadFromStream(lStream);
    lNodeRoot := FArquivo.ChildNodes.FindNode('root');
    LoadFromNode(lNodeRoot.ChildNodes[0]);
  finally
    lStream.Free;
  end;
end;

procedure TFileCustomObject.Save;
var
  lFile: TStringList;
begin
  FFileName := GetParentFileName;

  if FFileName = '' then
    raise Exception.Create('Nome do arquivo é nescessário para este procedimento !');

  SaveToNode;

  if FFormatType = ffXML then
    FArquivo.SaveToFile(FFileName)
  else
  begin
    lFile := TStringList.Create;
    try
      lFile.Text := FJSonObject.ToString;
      lFile.SaveToFile( FFileName );
    finally
      FreeAndNil(lFile);
    end;
  end;
end;

function TFileCustomObject.SaveFileToXmlString: string;
begin
  FFormatType := ffXML;
  DoSaveToNode;
  Result := FArquivo.XML.Text;
end;

procedure TFileCustomObject.SaveToFile(const AFile: string; const AFormat : TFileFormat = ffUnknown);
var
  lExt : string;
begin
  if AFormat <> ffUnknown then
    FFormatType := AFormat;

  if FFormatType = ffUnknown then
  begin
    lExt := AnsiUpperCase(ExtractFileExt(AFile));
    if (lExt = '.JSON') or (lExt = '.JSN') or (lExt = '.JSO') or (lExt = '.J') then
      FFormatType := ffJSON
    else
      FFormatType := ffXML
  end;

  FFileName := AFile;
  Save;
end;

function TFileCustomObject.SaveToJSONString: string;
begin
  FFormatType := ffJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;

  SaveToNode;
  Result := FJSonObject.ToString;
end;

//procedure TFileCustomObject.SaveToNode(const AArquivo: IXMLDocument;
//  const ANode: IXMLNode; const ANodeName: string);
//begin
//  if ANodeName <> '' then
//    FNodeName := ANodeName;
//
//  FNode := ANode.AddChild(FNodeName);
//  FNode.Attributes['ClassName'] := Self.ClassName;
//
//  DoSaveToNode;
//end;

procedure TFileCustomObject.SaveToNode(const ANodeName: string; const ASaveParent: Boolean);
var
  lNode: IXMLNode;
  lCriarNovoArquivo: Boolean;
  lIntf: IFileBase;
  lIntf2: IRpFileSupport;
begin
  lNode := nil;
  lCriarNovoArquivo := True;
  if (FParent <> nil) and ASaveParent then
  begin
    // teste de herança
    if FParent.InheritsFrom(TFileCustomObject) then
    begin
      FArquivo := TFileCustomObject(FParent).GetArquivo;
      FFormatType := TFileCustomObject(FParent).FormatType;
      FIncludeClassName := TFileCustomObject(FParent).IncludeClassName;
      lNode := TFileCustomObject(FParent).GetNode;
    end
    else
      if FParent.InheritsFrom(TFileCustomList) then
      begin
        FArquivo := TFileCustomList(FParent).GetArquivo;
        FFormatType := TFileCustomList(FParent).FormatType;
        FIncludeClassName := TFileCustomList(FParent).IncludeClassName;
        lNode := TFileCustomList(FParent).GetNode;
      end
      else
        // testa se tem interface
        if Supports(FParent, IFileBase, lIntf) then
        begin
          FArquivo := lIntf.Arquivo;
          FFormatType := lIntf.FormatType;
          FIncludeClassName := lIntf.IncludeClassName;
          lNode := lIntf.Node;
        end
        else
          if Supports(FParent, IRpFileSupport, lIntf2) then
          begin
            FArquivo := lIntf2.GetFileController.Arquivo;
            FFormatType := lIntf2.GetFileController.FormatType;
            FIncludeClassName := lIntf2.GetFileController.IncludeClassName;
            lNode := lIntf2.GetFileController.Node;
          end;

    lCriarNovoArquivo := not Assigned(FArquivo);
  end;

  if ANodeName <> '' then
    FNodeName := ANodeName;

  if FFormatType = ffUnknown then
    FFormatType := C_DEFAULT_FORMAT;

  case FFormatType of
  ffXML:
    begin
      if lCriarNovoArquivo then
      begin
        FArquivo := TXMLDocument.Create(nil);
        FArquivo.Active := True;
        FArquivo.Encoding := C_XML_ENCODING;
        FArquivo.Version := '1.0';
        FArquivo.StandAlone := 'yes';
      end;

      if (lNode = nil) then
        lNode := FArquivo.AddChild('root');

      FNode := lNode.AddChild(FNodeName);
      if FIncludeClassName or FProxyMode then
        FNode.Attributes['ClassName'] := Self.ClassName;
    end;

  ffJSON:
    InitializeJson;

  ffUnknown:
    raise ETipoBaseUnknown.CreateFmt('Não é possível realizar a escrita da classe %s. Tipo de formato não foi informado.', [Self.ClassName]);
  end;

  DoSaveToNode;
end;

function TFileCustomObject.SaveToXmlString: string;
var
  lArquivo: IXMLDocument;
  lNode: IXMLNode;
begin
  FFormatType := ffXML;

  lArquivo := TXMLDocument.Create(nil);
  lArquivo.Active := True;
  lArquivo.Encoding := C_XML_ENCODING;
  lArquivo.Version := '1.0';
  lArquivo.StandAlone := 'yes';
  lNode := lArquivo.AddChild('root');

  FNode := lNode.AddChild(FNodeName);
  if FIncludeClassName or FProxyMode then
    FNode.Attributes['ClassName'] := Self.ClassName;

  DoSaveToNode;

  if FProxyMode then
  begin
    VerificarDependenciaPropriedades(TFileCustomObjectClass(Self.ClassType));

    FNode.Attributes['ParentClass'] := Self.ClassParent.ClassName;

    if Self is TFileCustomList then
    begin
      FNode.Attributes['isList'] := 'true';
      FNode.Attributes['itemClass'] := TFileCustomList(Self).ItemClassName;
    end;
  end;

  Result := lArquivo.XML.Text;
  // verifica se o texto possui o Encoding
  if Pos(C_XML_ENCODING, Result) <= 0 then
    Insert(' encoding="' + C_XML_ENCODING + '"', Result, Pos('"1.0"', Result) + 5);
end;

procedure TFileCustomObject.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TFileCustomObject.SetFormatType(const Value: TFileFormat);
begin
  FFormatType := Value;
end;

procedure TFileCustomObject.SetIncludeClassName(const AValue: Boolean);
begin
  FIncludeClassName := AValue;
end;

procedure TFileCustomObject.SetLastError(const AError: string);
begin
  FLastError := AError;
end;

procedure TFileCustomObject.SetNodeName(const Value: string);
begin
  FNodeName := TRpStrings.RemoveAcentos( Value );
end;

procedure TFileCustomObject.SetParent(const Value: TObject);
begin
  FParent := Value;
end;

procedure TFileCustomObject.SetProxyMode(const AValue: Boolean);
begin
  FProxyMode := AValue;
end;

procedure TFileCustomObject.ToNode(const AFieldName: string; const AFieldClass: TStrings);
var
  lAux : string;
  i: Integer;
begin
  if AFieldClass.Count > 0 then
  begin
    lAux := EmptyStr;
    for i := 0 to AFieldClass.Count - 1 do
    begin
      if lAux <> EmptyStr then
        lAux := lAux + ',';
      lAux := lAux + '"' + AFieldClass[i] + '"';
    end;
    lAux := '[' + lAux + ']';
  end
  else
    lAux := '[]';

  if FormatType = ffXML then
  begin
    ToNode(AFieldName, lAux);
  end else
    FJSonObject.AddPair(AFieldName, TJSONStrinNoQuote.Create( lAux ));
end;

procedure TFileCustomObject.ToNode(const AFieldName, AValue: string;
  const ABase64: Boolean);
begin
  if FProxyMode then
  begin
    InternalGravarNodeProxy(AFieldName, AValue, IfThen(ABase64, 'AsBase64', ''));
  end else
    if ABase64 and (AValue <> '') then
      ToNode(AFieldName, Algorithms.Base64CompressedString(AValue))
    else
      ToNode(AFieldName, AValue);
end;

procedure TFileCustomObject.ToNode(const AFieldName : string; const AList: TFileCustomList);
var
  lItensPair : TJSONPair;
begin
  AList.NodeName := AFieldName;
  if FProxyMode then
    AList.SaveToNode
  else
    if AList.Count > 0 then
    begin
      AList.FormatType := Self.FormatType;
      AList.IncludeClassName := Self.IncludeClassName;
      AList.SaveToNode;

      if FFormatType = ffJSON then
      begin
        lItensPair := AList.FJSonObject.Get('Itens');
        if lItensPair <> nil then
        {$IFDEF XE8UP}
          FJSonObject.AddPair(TJSONPair.Create(AList.FNodeName, lItensPair.JsonValue.Clone as TJSONValue));
        {$ELSE}
          FJSonObject.AddPair(TJSONPair.Create(AList.FNodeName, lItensPair.JsonValue.Clone as TJSONObject));
        {$ENDIF}
      end;
    end;
end;

procedure TFileCustomObject.ToNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; const ASetValue);
var
  lStr : string;
  lItensInfo: PTypeInfo;
  lNode: IXMLNode;
begin
  if FProxyMode then
  begin
    // salva o enum do set
    lItensInfo := GetTypeData(ASetTypeInfo)^.CompType^;
    ToNode(AFieldName, 0, lItensInfo);

    FNode[AFieldName] := ASetTypeInfo.Name;
    lNode := FNode.ChildNodes.FindNode(AFieldName);
    lNode.Attributes['ex'] := 'set';
    FNode.Attributes[String(ASetTypeInfo.Name)] := 'set of ' + lItensInfo.Name;
  end
  else
  begin
    try
      lStr := TRpStrings.SetToString(ASetTypeInfo, ASetValue, True);
    except
      lStr := '';
    end;

    if (lStr <> '') and (lStr <> '[]') then
      ToNode(AFieldName, lStr);
  end;
end;

procedure TFileCustomObject.ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo);
var
  lStr : string;
  lValores: string;
  I: Integer;
  T: PTypeData;
  lNode: IXMLNode;
begin
  if FProxyMode then
  begin
    FNode[AFieldName] := ASetTypeInfo.Name;
    lNode := FNode.ChildNodes.FindNode(AFieldName);
    lNode.Attributes['ex'] := 'enum';

    T := GetTypeData(GetTypeData(ASetTypeInfo)^.BaseType^);

    lValores := '';
    for I := T^.MinValue to T^.MaxValue do
    begin
      if lValores <> '' then
        lValores := lValores + ',';
      lValores := lValores  + GetEnumName(ASetTypeInfo, I);
    end;

    FNode.Attributes[string(ASetTypeInfo.Name)] := lValores;
  end
  else
  begin
    try
      lStr := TRpStrings.EnumToStr(ASetTypeInfo, AEnumValue);
    except
      lStr := '';
    end;

    if lStr <> '' then
      ToNode(AFieldName, lStr);
  end;
end;

procedure TFileCustomObject.ToNode(const AFieldName : string; const AItem: IFileBase);
begin
  AItem.NodeName := AFieldName;

  if FProxyMode then
  begin
    if (AItem.GetNodeName <> '') then
    begin
      AItem.ProxyMode := True;
      AItem.SaveToNode;
    end;
  end
  else
  begin
    AItem.SaveToNode;

    if AItem.FormatType = ffJSON then
      FJSonObject.AddPair(AItem.GetNodeName, TJSONObject(AItem.JSonObject.Clone));
  end;
end;

procedure TFileCustomObject.ToNode(const AFieldName: string; const AValue: Variant);
var
  lLimpa: Boolean;
begin
  if FFormatType = ffUnknown then
    FFormatType := C_DEFAULT_FORMAT;

  if FProxyMode then
    InternalGravarNodeProxy(AFieldName, AValue)
  else
  begin
    lLimpa := (VarIsStr(AValue) and (Trim(AValue) = ''))
           or (VarIsNumeric(AValue) and (AValue = 0))
           or ((FindVarData(AValue)^.VType = varDate) and (AValue <= 2));

    if not lLimpa then
    begin
      if FFormatType = ffXML then
      begin
        if FNode <> nil then
        begin
          case VarType(AValue) of
            varDate: FNode[AFieldName] := TXMLFieldNode.GetDateFormated(AValue);
            varDouble: FNode[AFieldName] := TXMLFieldNode.GetDoubleFormated(AValue);
          else
            FNode[AFieldName] := AValue;
          end;
        end;
      end
      else
        if FFormatType = ffJSON then
          InternalGravarJSON(AFieldName, AValue)
        else
          raise ETipoBaseUnknown.CreateFmt('Não é possível escrever para a classe %s. Formato desconhecido.', [Self.ClassName]);
    end;
  end;
end;

procedure TFileCustomObject.FromNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; out ASetVar);
var
  lStr : string;
begin
  FromNode(AFieldName, lStr);
  TRpStrings.StringToSet(ASetTypeInfo, ASetVar, lStr);
end;

{ TRpContainerXMLController }

procedure TRpContainerFileController.DoLoadFromNode(const ANode: IXMLNode);
var
  i: Integer;
  lIntf: IRpFileSupport;
  lNode: IXMLNode;
begin
  if Assigned(FParent) and Parent.InheritsFrom(TComponent) then
    for i := 0 to TComponent(FParent).ComponentCount - 1 do
    begin
      if Supports(TComponent(FParent).Components[i], IRpFileSupport, lIntf) then
      begin
        lNode := ANode.ChildNodes.FindNode(lIntf.GetFileController.GetnodeName);
        lIntf.GetFileController.LoadFromNode(lNode);
      end;
    end;
end;

procedure TRpContainerFileController.DoSaveToNode;
var
  i: Integer;
  lIntf: IRpFileSupport;
begin
  if Assigned(FParent) and FParent.InheritsFrom(TComponent) then
    for i := 0 to TComponent(FParent).ComponentCount - 1 do
    begin
      if Supports(TComponent(FParent).Components[i], IRpFileSupport, lIntf) then
        lIntf.GetFileController.SaveToNode(lIntf.GetFileNodeName, True);
//        lIntf.GetFileController.SaveToNode(FArquivo, FNode, lIntf.GetXMLNodeName );
    end;
//  else
//    if Assigned(FParent) and FParent.InheritsFrom(TWinControl) then
end;

{ TXMLCustomList }

function TFileCustomList.Add(const ACustomFileObject: TFileCustomObject) : Integer;
begin
  InitList;

  Result := FItens.Add(ACustomFileObject);
  ACustomFileObject.Parent := Self;
end;

procedure TFileCustomList.Clear;
var
  i: Integer;
begin
  if Assigned(FItens) then
  begin
    for i := FItens.Count downto 1 do
    begin
      FItens.Delete(i - 1);
    end;
  end;
  FNode := nil;
end;

function TFileCustomList.Count: Integer;
begin
  InitList;
  Result := FItens.Count;
end;

constructor TFileCustomList.Create(AOwner: TObject; const ANodeName: string;
  const AOwnsObjects: Boolean);
begin
  inherited Create(AOwner, ANodeName);
  InitList;
  FItens.OwnsObjects := AOwnsObjects;

  InitializeJson;
end;

function TFileCustomList.Delete(const AIndex: Integer): Boolean;
begin
  InitList;
  if (AIndex >= 0) and (AIndex <= (FItens.Count - 1) ) then
  begin
    FItens.Delete(AIndex);
    Result := True;
  end
  else
    Result := False;
end;

destructor TFileCustomList.Destroy;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  inherited;
end;

procedure TFileCustomList.DoLoadFromNode(const ANode: IXMLNode);
begin
// dummy
end;

procedure TFileCustomList.DoSaveToNode;
var
  i: Integer;
  lJsonList: TJSONArray;
  lStr: string;
begin
  inherited;

  if FFormatType = ffJSON then
    lJsonList := TJSONArray.Create;

  for i := 0 to FItens.Count - 1 do
  begin
    {$IFDEF XE3UP}
    FItens[i].SaveToNode;
    {$ELSE}
    TFileCustomObject(FItens[i]).SaveToNode;
    {$ENDIF}

    if FFormatType = ffJSON then
    begin
      lStr := TFileCustomObject(Items[i]).FJSonObject.ToString;
      {$IFDEF XE8UP}
      lJsonList.AddElement( TFileCustomObject(Items[i]).FJSonObject.Clone as TJSONValue);
      {$ELSE}
      lJsonList.AddElement( TFileCustomObject(Items[i]).FJSonObject.Clone as TJSONObject );
      {$ENDIF}
    end;
  end;

  if FFormatType = ffJSON then
  begin
    FJSonObject.AddPair('Itens', TJSONArray(lJsonList.Clone));
    FreeAndNil( lJsonList );
  end;
end;

procedure TFileCustomList.Finalize;
begin
  Clear;
  if Assigned(FItens) then
  begin
    FItens.Clear;
    FreeAndNil(FItens);
  end;

  inherited;
end;

procedure TFileCustomList.FromOther(const AOther: IFileBase);
var
  lItem: TFileCustomObject;
  i: Integer;
begin
  if (AOther <> nil) and (AOther is TFileCustomList) and (TFileCustomList(AOther).Count > 0) then
  begin
    for i := 0 to TFileCustomList(AOther).Count - 1 do
    begin
      lItem := TFileCustomList(AOther).GetItemClass.Create(Self);
      lItem.FromOther( TFileCustomList(AOther).Items[i] );
      Self.Add(lItem);
    end;
  end;
end;

function TFileCustomList.GetItem(AIndex: Integer): TFileCustomObject;
begin
  InitList;
  Result := nil;
  if (FItens.Count > 0) and (AIndex >= 0) then
  begin
    {$IFDEF XE3UP}
    Result := FItens[AIndex];
    {$ELSE}
    Result := TFileCustomObject(FItens[AIndex]);
    {$ENDIF}
  end;
end;

function TFileCustomList.GetItemClassName: string;
begin
  Result := GetItemClass.ClassName;
end;

function TFileCustomList.GetOwnsObjects: Boolean;
begin
  InitList;
  Result := FItens.OwnsObjects;
end;

procedure TFileCustomList.Initialize;
begin
  inherited;
end;

procedure TFileCustomList.InitList;
begin
  if not Assigned(FItens) then
    FItens := TObjectList{$IFDEF XE3UP}<TFileCustomObject>{$ENDIF}.Create;
end;

function TFileCustomList.Insert(const ACustomFileObject: TFileCustomObject;
  const AIndex: Integer) : Integer;
begin
  InitList;
  ACustomFileObject.Parent := Self;
  FItens.Insert(AIndex, ACustomFileObject);
  Result := FItens.IndexOf(ACustomFileObject);
end;

procedure TFileCustomList.LoadFromJSONString(const AJSONString: string);
var
  i: Integer;
  lObjeto: TFileCustomObject;
  lPair: TJSONPair;
  lJsonList: TJSONArray;
  lTempStr: string;
begin
  FFormatType := ffJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  lTempStr := AJSONString;
  // espera-se que AJSONString seja uma array: iniciando com '[', terminando com ']'
  if Pos('ClassName', lTempStr) = 3 then
    System.Delete(lTempStr, 1, Pos(',', lTempStr));

  if Pos('Itens', lTempStr) = 2 then
    lTempStr := '{' + lTempStr
  else if Pos('{{', lTempStr) = 1 then
  begin
    System.Delete(lTempStr, 1, 1);
    if Copy(lTempStr, Length(lTempStr) - 1, 2) = '}}' then
      System.Delete(lTempStr, Length(lTempStr), 1);
    lTempStr := '{"Itens" : [' + lTempStr + ']}'
  end
  else if Pos('Itens', lTempStr) <> 3 then
    lTempStr := '{"Itens":' + lTempStr + '}';

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse(BytesOf(lTempStr), 0);

  // se o parse deu certo
  if (FJSonObject <> nil) then
  begin
    // lista
    lPair := FJSonObject.Get('Itens');
    if lPair <> nil then
    begin
      lJsonList := lPair.JsonValue as TJSONArray;
      for i := 0 to lJsonList.Size - 1 do
      begin
        lObjeto := TFileCustomObject.CreateFromJson(lJsonList.Get(i) as TJSONObject, Self);
        Add(lObjeto);
      end;
    end;
  end;
end;

procedure TFileCustomList.LoadFromNode(const ANode: IXMLNode);
var
  I: Integer;
  lObjeto: TFileCustomObject;
begin
  Clear;

  FNode := ANode;
  if FNode <> nil then
  begin
    for I := 0 to FNode.ChildNodes.Count - 1 do
    begin
      lObjeto := TFileCustomObject.CreateFromNode(FNode.ChildNodes[I], Self);
      Add(lObjeto);
    end;
  end;
end;

procedure TFileCustomList.LoadFromXmlString(const AXmlValue: string);
var
  lXmlDoc : IXMLDocument;
  lXmlNode : IXMLNode;
  lStream: TStringStream;
begin
  FFormatType := ffXML;
  lStream := TStringStream.Create(AXmlValue);
  lXmlDoc := TXMLDocument.Create(nil);
  try
    lXmlDoc.LoadFromStream( lStream );
    lXmlNode := lXmlDoc.ChildNodes.FindNode('root');
    if Assigned(lXmlNode) then
    begin
      lXmlNode := lXmlNode.ChildNodes.FindNode( Self.ClassName );
      LoadFromNode(lXmlNode);
    end;
  finally
    FreeAndNil(lStream);
  end;
end;

function TFileCustomList.Remove(const ACustomFileObject: TFileCustomObject): Integer;
begin
  InitList;
  Result := FItens.Remove(ACustomFileObject);
end;

procedure TFileCustomList.Reset;
begin
  Self.Clear;
end;

procedure TFileCustomList.SetOwnsObjects(const Value: Boolean);
begin
  InitList;
  FItens.OwnsObjects := Value;
end;

{ TXMLClassRegister }

function TFileClassRegister.GetClass(const AClassName: string): TFileCustomObjectClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].FileClassName = AClassName then
    begin
      Result := TFileCustomObjectClass(Items[I].FileClass);
      Exit;
    end;
end;

function TFileClassRegister.GetClass(const AIndex: Integer): TFileCustomObjectClass;
begin
  Result := TFileCustomObjectClass(Items[AIndex].FileClass);
end;

function TFileClassRegister.GetItem(const Index: Integer): TFileClassRegisterItem;
begin
  Result := TFileClassRegisterItem(Self.Get(Index));
end;

procedure TFileClassRegister.Registrar(const AClass: TFileCustomObjectClass);
var
  lItem : TFileClassRegisterItem;
begin
  // so registra se ainda não existir
  if GetClass(AClass.ClassName) = nil then
  begin
    lItem := TFileClassRegisterItem.Create;
    lItem.FileClass := AClass;
    lItem.FileClassName := AClass.ClassName;
    Add(lItem);
  end;
end;

{ FileClassRegistrer }

class function FileClassRegistrer.Count: Integer;
begin
  if TFileClassRegister.InternalClassRegister = nil then
    Result := -1
  else
    Result := TFileClassRegister.InternalClassRegister.Count;
end;

class procedure FileClassRegistrer.Destruir;
begin
  if TFileClassRegister.InternalClassRegister <> nil then
    FreeAndNil(TFileClassRegister.InternalClassRegister);
end;

class function FileClassRegistrer.GetClass(const AClassName: string): TFileCustomObjectClass;
begin
  if TFileClassRegister.InternalClassRegister = nil then
    TFileClassRegister.InternalClassRegister := TFileClassRegister.Create;

  Result := TFileClassRegister.InternalClassRegister.GetClass(AClassName);
end;

class function FileClassRegistrer.GetClass(
  const AIndex: Integer): TFileCustomObjectClass;
begin
  if TFileClassRegister.InternalClassRegister = nil then
    TFileClassRegister.InternalClassRegister := TFileClassRegister.Create;

  Result := TFileClassRegister.InternalClassRegister.GetClass(AIndex);
end;

class procedure FileClassRegistrer.Registrar(const AClass: TFileCustomObjectClass);
begin
  if TFileClassRegister.InternalClassRegister = nil then
    TFileClassRegister.InternalClassRegister := TFileClassRegister.Create;
  TFileClassRegister.InternalClassRegister.Registrar(AClass);
end;

{ TRpListXMLController }

function TRpListFileController.ClearList: Boolean;
begin
  if Parent.InheritsFrom(TCollection) then
  begin
    TCollection(Parent).Clear;
    Result := True;
  end
  else
    if Parent.InheritsFrom(TList) then
    begin
      TList(Parent).Clear;
      Result := True;
    end
    else
      Result := False;
end;

procedure TRpListFileController.DoLoadFromNode(const ANode: IXMLNode);
var
  i: Integer;
begin
  if Assigned(FParent) and ClearList then
  begin
    if FormatType = ffJSON then
      LoadListFromJsonObject(FJSonObject.ToString)
    else
    begin
      for i := 0 to ANode.ChildNodes.Count - 1 do
        DoLoadItem(ANode.ChildNodes[i]);
    end;
  end;
end;

procedure TRpListFileController.DoLoadItem(const ANode: IXMLNode);
var
  lObj : TObject;
  lIntf: IRpFileSupport;
begin
  if GetNewItem(lObj) then
  begin
    if Supports(lObj, IRpFileSupport, lIntf) then
      lIntf.GetFileController.LoadFromNode(ANode)
    else
    begin
      if Parent.InheritsFrom(TCollection) then
        TCollection(Parent).Delete( TCollection(Parent).Count - 1 )
      else
        if Parent.InheritsFrom(TList) then
          TList(Parent).Delete( TList(Parent).Count - 1 );
    end;
  end;
end;

procedure TRpListFileController.DoSaveToNode;
var
  i: Integer;
  lIntf: IRpFileSupport;
  lJsonList: TJSONArray;
  lStr: String;
begin
  if Assigned(FParent) then
  begin
    if FFormatType = ffJSON then
      lJsonList := TJSONArray.Create;

    // TCollection
    if FParent.InheritsFrom(TCollection) then
      for i := 0 to TCollection(FParent).Count - 1 do
      begin
        if Supports(TCollection(FParent).Items[i], IRpFileSupport, lIntf) then
        begin
          lIntf.GetFileController.SaveToNode(lIntf.GetFileNodeName, True);
          if FFormatType = ffJSON then
          begin
            lStr := lIntf.GetFileController.JSonObject.ToString;
            lJsonList.AddElement( lIntf.GetFileController.JSonObject.Clone as TJSONObject );
          end;
        end;
      end
    // TList, TObjectList
    else if FParent.InheritsFrom(TList) then
      for i := 0 to TList(FParent).Count - 1 do
      begin
        if Supports(TList(FParent).Items[i], IRpFileSupport, lIntf) then
        begin
          lIntf.GetFileController.SaveToNode(lIntf.GetFileNodeName, True);
          if FFormatType = ffJSON then
          begin
            lStr := lIntf.GetFileController.JSonObject.ToString;
            lJsonList.AddElement( lIntf.GetFileController.JSonObject.Clone as TJSONObject );
          end;
        end;
      end;

    if FFormatType = ffJSON then
    begin
      FJSonObject.AddPair('Itens', TJSONArray(lJsonList.Clone));
      FreeAndNil( lJsonList );
    end;
  end;
end;

procedure TRpListFileController.LoadListFromJsonObject(const AJsonString : string);
var
  i: Integer;
  lPair: TJSONPair;
  lJsonList: TJSONArray;
  lTempStr: string;

  procedure InternalLoad;
  var
    lObj : TObject;
    lIntf: IRpFileSupport;
  begin
    if GetNewItem(lObj) then
    begin
      if Supports(lObj, IRpFileSupport, lIntf) then
        lIntf.GetFileController.LoadFromJSONString(lTempStr)
      else
      begin
        if Parent.InheritsFrom(TCollection) then
          TCollection(Parent).Delete( TCollection(Parent).Count - 1 )
        else
          if Parent.InheritsFrom(TList) then
            TList(Parent).Delete( TList(Parent).Count - 1 );
      end;
    end;
  end;

begin
  FFormatType := ffJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  lTempStr := AJsonString;
  // espera-se que AJSONString seja uma array: iniciando com '[', terminando com ']'
  if Pos('ClassName', lTempStr) = 3 then
    System.Delete(lTempStr, 1, Pos(',', lTempStr));

  if Pos('Itens', lTempStr) = 2 then
    lTempStr := '{' + lTempStr
  else if Pos('Itens', lTempStr) <> 3 then
    lTempStr := '{"Itens":' + lTempStr + '}';

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse(BytesOf(lTempStr), 0);

  // se o parse deu certo
  if (FJSonObject <> nil) then
  begin
    // lista
    lPair := FJSonObject.Get('Itens');
    if lPair <> nil then
    begin
      lJsonList := lPair.JsonValue as TJSONArray;
      for i := 0 to lJsonList.Size - 1 do
      begin
        lTempStr := (lJsonList.Get(i) as TJSONObject).ToString;
        InternalLoad;
//        lObjeto := TFileCustomObject.CreateFromJson(lJsonList.Get(i) as TJSONObject, Self);
//        Add(lObjeto);
      end;
    end;
  end;
end;

{ TRpBaseFileController }

procedure TRpBaseFileController.DoLoadFromNode(const ANode: IXMLNode);
begin
end;

procedure TRpBaseFileController.DoSaveToNode;
begin
end;

procedure TRpBaseFileController.FromOther(const AOther: IFileBase);
begin
end;

procedure TRpBaseFileController.Reset;
begin
end;

procedure TFileCustomObject.ToNode(const AFieldName: string; const AItem: IFileBase; const AGuid: TGuid);
begin
  if AItem <> nil then
  begin
    AItem.SaveToNode(AFieldName);
    if FFormatType = ffXML then
      AItem.Node.Attributes['GUID'] := GUIDToString(AGuid)
    else
    begin
      AItem.JSonObject.AddPair('GUID', GUIDToString(AGuid));
      FJSonObject.AddPair(AFieldName, AItem.JSonObject.Clone as TJSONObject);
    end;
  end;
end;

procedure TFileCustomObject.ToNode(const AFieldName: string; const AList: TInterfaceList; const AGuid: TGuid);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(AList[I], IFileBase) then
      ToNode(AFieldName, AList[I] as IFileBase, AGuid);
  end;
end;

procedure TFileCustomObject.VerificarDependenciaPropriedades(
  const AClassParent: TFileCustomObjectClass);
var
  lParent : TFileCustomObject;
  lNode, lNodeParent: IXMLNode;
  i: Integer;
begin
  if (AClassParent <> TFileCustomObject) and (AClassParent <> TFileCustomList) then
  begin
    lParent := AClassParent.Create(nil);
    try
      lParent.ProxyMode := True;
      VerificarDependenciaPropriedades(TFileCustomObjectClass(lParent.ClassParent));

      lParent.SaveToNode(lParent.ClassName, False);
      for i := 0 to lParent.Node.ChildNodes.Count - 1 do
      begin
        lNodeParent := lParent.Node.ChildNodes.Get(i);
        lNode := FNode.ChildNodes.FindNode(lNodeParent.NodeName);
        if (lNode <> nil) then
        begin
          if lNodeParent.Attributes['ClassName'] = Null then
          begin
            if Pos('.', lNode.NodeValue) <= 0 then
              lNode.NodeValue := lParent.ClassName + '.' + lNode.NodeValue;
          end
          else
            if lNode.Attributes['ClassParent'] = Null then
              lNode.Attributes['ClassParent'] := lParent.ClassName;
        end;
      end;
    finally
      FreeAndNil(lParent);
    end;
  end;
end;

{ TFileCustomObjectEx }

function TFileCustomObjectEx.DoDelete(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TFileCustomObjectEx.DoExecute(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TFileCustomObjectEx.DoLoad(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TFileCustomObjectEx.DoSave(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

initialization

finalization
  FileClassRegistrer.Destruir;

end.
