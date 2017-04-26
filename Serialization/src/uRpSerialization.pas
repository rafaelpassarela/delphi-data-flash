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
  TypInfo, uRpFields, uRpJSONBase, uRpAlgorithms, DB, StrUtils, uRpResourceString;

type
  TSerializationFormat = (sfXML, sfJSON, sfUnknown);

  EBaseSerializationTypeUnknown = Exception;

  TCustomSerializableObject = class;
  TCustomSerializableObjectClass = class of TCustomSerializableObject;
  TCustomSerializableList = class;
  TCustomSerializableListClass = class of TCustomSerializableList;

  ISerializableBase = interface
    ['{FC07098F-E2E1-47F7-A2FD-D72028DB71C3}']
    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True); overload;
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure LoadFromFile(const AFile : string; const AFormat : TSerializationFormat = sfUnknown);
    procedure LoadFromString(const AString : string; const AFormat : TSerializationFormat = sfUnknown);
    procedure LoadFromJSONString(const AJSONString : string);
    procedure LoadFromXMLString(const AXMLString : string);

    procedure SetFormatType(const Value : TSerializationFormat);
    function GetFormatType : TSerializationFormat;

    function GetXMLFile: IXMLDocument;
    function GetXMLNode: IXMLNode;
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
    procedure SaveToFile(const AFile : string; const AFormat : TSerializationFormat = sfUnknown);
    function SaveToXmlString : string;
    function SaveToJSONString : string;
    function SaveFileToXmlString : string;
    function GetXMLDoc(const AIncludeParent : Boolean = True) : IXMLDocument;

    property XMLFile : IXMLDocument read GetXMLFile;
    property XMLNode : IXMLNode read GetXMLNode;
    property Parent : TObject read GetParent write SetParent;
    property FileName : string read GetFileName write SetFileName;
    property FormatType : TSerializationFormat read GetFormatType write SetFormatType;
    property JSonObject : IJSONObject read GetJSonObject;
    property ProxyMode : Boolean read GetProxyMode write SetProxyMode;
    property NodeName : string read GetNodeName write SetNodeName;
    property IncludeClassName : Boolean read GetIncludeClassName write SetIncludeClassName;
  end;

  ISerializationSupport = interface
    ['{16325BDB-62C8-433B-925D-445803DF0ECF}']
    function GetSerializationController : ISerializableBase;
    function GetSerializationNodeName: string;
  end;

  ISerializableBaseHelper = interface
    ['{539FE1C3-E805-491A-8AF2-E4411DD2B7D9}']
    function DoSave(const ADataComponent : TComponent) : Boolean;
    function DoLoad(const ADataComponent : TComponent) : Boolean;
    function DoDelete(const ADataComponent : TComponent) : Boolean;
    function DoExecute(const ADataComponent : TComponent) : Boolean;
  end;

  TCustomSerializableObject = class(TInterfacedPersistent, ISerializableBase)
  private
    // XML
    FXMLFile : IXMLDocument;
    FXMLNode : IXMLNode;
    // JSON
    FJSonObject : IJSONObject;

    FNodeName : string;
    FParent: TObject;
    FFileName: string;
    FFormatType: TSerializationFormat;
    FProxyMode : Boolean;
    FLastError: string;
    FIncludeClassName: Boolean;
    function GetParentFileName : string;
    function InternalGetField(const AFieldName : string) : TRpFieldsBase;
    function GetNodeName: string;
    procedure SetNodeName(const Value: string);
    procedure InternalSaveJSON(const AFieldName : string; const AValue : Variant);
    procedure InternalSaveNodeProxy(const AFieldName : string; const AValue : Variant;
      const AAttributeValue : string = '');
    procedure CheckPropertyDependency(const AClassParent : TCustomSerializableObjectClass);
    procedure InitializeJson;
  protected
    function GetFormatType: TSerializationFormat;
    procedure SetFormatType(const Value: TSerializationFormat);
    function GetParent: TObject;
    procedure SetParent(const Value: TObject);
    function GetProxyMode : Boolean;
    procedure SetProxyMode(const AValue : Boolean);
    function GetIncludeClassName : Boolean;
    procedure SetIncludeClassName(const AValue : Boolean);

    function GetXMLFile: IXMLDocument;
    function GetXMLNode: IXMLNode;
    function GetJSonObject: IJSONObject;
    // native
    procedure FromNode(const AFieldName : string; out AValue : Word); overload;
    procedure FromNode(const AFieldName : string; out AValue : Char); overload;
    procedure FromNode(const AFieldName : string; out AValue : Boolean); overload;
    procedure FromNode(const AFieldName : string; out AValue : Integer); overload;
    procedure FromNode(const AFieldName : string; out AValue : String; const ABase64 : Boolean = False); overload;
    procedure FromNode(const AFieldName : string; out AValue : TDateTime); overload;
    procedure FromNode(const AFieldName : string; out AValue : TTime); overload;
    procedure FromNode(const AFieldName : string; out AValue : Double); overload;
    procedure FromNode(const AFieldName : string; const AFieldClass : TStrings); overload;
    // sets and enums
    procedure FromNode(const AFieldName : string; ASetTypeInfo : PTypeInfo; out ASetVar); overload;
    procedure FromNode(const AFieldName : string; out AEnumOrdValue : Cardinal; ATypeInfo : PTypeInfo); overload;
    // other objects and types with serialization support
    procedure FromNode(const AFieldName : string; const AItem : TCustomSerializableObject); overload;
    procedure FromNode(const AFieldName : string; const AList : TCustomSerializableList); overload;

    procedure FromNode(const AFieldName : string; out AItem : ISerializableBase); overload;
    procedure FromNode(const ANode: IXMLNode;     out AItem : ISerializableBase); overload;
    procedure FromNode(const AJSon: IJSONObject;  out AItem : ISerializableBase); overload;

    function FromNodeAsInterface(const ANode : IXMLNode) : ISerializableBase; overload;
    function FromNodeAsInterface(const AFieldName : string) : ISerializableBase; overload;

    procedure ToNode(const AFieldName : string; const AValue : Variant); overload;
    procedure ToNode(const AFieldName : string; const AFieldClass : TStrings); overload;
    procedure ToNode(const AFieldName : string; const AValue : string; const ABase64 : Boolean); overload;
    procedure ToNode(const AFieldName : string; const AItem : ISerializableBase); overload;
    procedure ToNode(const AFieldName : string; ASetTypeInfo : PTypeInfo; const ASetValue); overload;
    procedure ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo); overload;
    procedure ToNode(const AFieldName : string; const AList : TCustomSerializableList); overload;
    procedure ToNode(const AFieldName : string; const AItem : ISerializableBase; const AGuid : TGuid); overload;
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
    procedure FromOther(const AOther : ISerializableBase); virtual; abstract;
    procedure Assign(const AOther : ISerializableBase); reintroduce;
    procedure SetLastError(const AError : string);

    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True);
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure SaveToFile(const AFile : string; const AFormat : TSerializationFormat = sfUnknown); virtual;

    // XML
    function SaveToXmlString : string; virtual;
    function SaveFileToXmlString : string; virtual;
    // JSON
    function SaveToJSONString : string; virtual;

    procedure LoadFromFile(const AFile : string; const AFormat : TSerializationFormat = sfUnknown); virtual;
    procedure LoadFromString(const AString : string; const AFormat : TSerializationFormat = sfUnknown); virtual;

    function GetXMLDoc(const AIncludeParent : Boolean = True) : IXMLDocument;
    function GetClassName: string;

    property XMLNode : IXMLNode read GetXMLNode;
    property XMLFile : IXMLDocument read GetXMLFile;
    property JSonObject : IJSONObject read GetJSonObject;

    property Parent : TObject read GetParent write SetParent;
    property FileName : string read GetFileName write SetFileName;
    property NodeName : string read GetNodeName write SetNodeName;
    property FormatType : TSerializationFormat read GetFormatType write SetFormatType;
    property IncludeClassName : Boolean read GetIncludeClassName write SetIncludeClassName default True;
    property ProxyMode : Boolean read GetProxyMode write SetProxyMode;
    property LastError : string read FLastError;

    class function CreateFromNode(const ANode : IXMLNode; const AOwner : TCustomSerializableObject) : TCustomSerializableObject;
    class function CreateFromJson(const AJSonObject : IJSONObject; const AOwner : TCustomSerializableObject) : TCustomSerializableObject;
    class function CreateFromXML(const AXMLString : string; const AOwner : TCustomSerializableObject) : TCustomSerializableObject;
  end;

  TCustomSerializableList = class(TCustomSerializableObject)
  private
    FItens : TObjectList{$IFDEF XE3UP}<TCustomSerializableObject>{$ENDIF};
    procedure InitList;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    function GetItemClassName: string;
  protected
    function GetItem(AIndex: Integer): TCustomSerializableObject;
    function GetItemClass : TCustomSerializableObjectClass; virtual; abstract;

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

    function Add(const ACustomFileObject : TCustomSerializableObject) : Integer;
    function Insert(const ACustomFileObject : TCustomSerializableObject; const AIndex : Integer = -1) : Integer;
    function Delete(const AIndex : Integer) : Boolean;
    function Remove(const ACustomFileObject : TCustomSerializableObject) : Integer;
    function Count : Integer;

    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure Clear;
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;

    property Items[AIndex : Integer] : TCustomSerializableObject read GetItem; default;
    property OwnsObjects : Boolean read GetOwnsObjects write SetOwnsObjects;
    property ItemClassName : string read GetItemClassName;
  end;

  TFileCustomObjectEx = class(TCustomSerializableObject, ISerializableBaseHelper)
  protected
    function DoDelete(const ADataComponent: TComponent): Boolean; virtual;
    function DoSave(const ADataComponent: TComponent): Boolean; virtual;
    function DoExecute(const ADataComponent: TComponent): Boolean; virtual;
    function DoLoad(const ADataComponent: TComponent): Boolean; virtual;
  end;

  TFileClassRegisterItem = class
  private
    FFileClass: TCustomSerializableObjectClass;
    FFileClassName: string;
  public
    property FileClass : TCustomSerializableObjectClass read FFileClass write FFileClass;
    property FileClassName : string read FFileClassName write FFileClassName;
  end;

  TFileClassRegister = class({$IFDEF XE3UP} System.Contnrs.TObjectList {$ELSE} Contnrs.TObjectList {$ENDIF})
  protected
    function GetItem(const Index : Integer) : TFileClassRegisterItem;
  public
    class var InternalClassRegister: TFileClassRegister;
    procedure Registrar(const AClass : TCustomSerializableObjectClass);
    function GetClass(const AClassName : string) : TCustomSerializableObjectClass; overload;
    function GetClass(const AIndex : Integer) : TCustomSerializableObjectClass; overload;
    property Items[const Index: Integer]: TFileClassRegisterItem read GetItem; default;
  end;

  FileClassRegistrer = class
  public
    class procedure Destruir;
    class procedure Registrar(const AClass : TCustomSerializableObjectClass);
    class function GetClass(const AClassName : string) : TCustomSerializableObjectClass; overload;
    class function GetClass(const AIndex : Integer) : TCustomSerializableObjectClass; overload;
    class function Count : Integer;
//    class procedure Instanciar(const AClass : TXMLCustomObjectClass; const ANomeInstancia : string);
  end;

  TRpBaseFileController = class(TCustomSerializableObject)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: ISerializableBase); override;
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
  C_DEFAULT_FORMAT = sfJSON;

implementation

{ TXMLCustomObject }

procedure TCustomSerializableObject.Assign(const AOther: ISerializableBase);
var
  lNode: IXMLNode;
begin
  Reset;

  Self.FFormatType := C_DEFAULT_FORMAT;
  AOther.FormatType := C_DEFAULT_FORMAT;

  AOther.SaveToNode;
  lNode := AOther.XMLNode.CloneNode(True);
  LoadFromNode(lNode);
end;

constructor TCustomSerializableObject.Create(AOwner: TObject; const ANodeName: string);
begin
  FFormatType := C_DEFAULT_FORMAT;
  FIncludeClassName := True;
  FXMLNode := nil;
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

class function TCustomSerializableObject.CreateFromJson(const AJSonObject: IJSONObject;
  const AOwner: TCustomSerializableObject): TCustomSerializableObject;
var
  lObjClass: TCustomSerializableObjectClass;
  lClassName: string;
  lPair: TJSONPair;
begin
  lPair := AJSonObject.Get('ClassName');
  if Assigned(lPair) then
    lClassName := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll])
  else
  begin
    if Assigned(AOwner) and (AOwner is TCustomSerializableList) then
      lClassName := TCustomSerializableList(AOwner).GetItemClass.ClassName;
  end;

  if lClassName = '' then
    raise Exception.Create(R_CLASS_ID_NOT_FOUND);

  lObjClass := FileClassRegistrer.GetClass(lClassName);
  if lObjClass = nil then
    raise Exception.CreateFmt(R_CLASS_NAME_NOT_REGISTERED, [lClassName]);

  if not lObjClass.InheritsFrom(TCustomSerializableObject) then
    raise Exception.CreateFmt(R_CLASS_NAME_INVALID, [lClassName]);

  Result := lObjClass.Create(AOwner);
  Result.LoadFromJSONString( AJSonObject.ToString );
end;

class function TCustomSerializableObject.CreateFromNode(const ANode: IXMLNode;
  const AOwner: TCustomSerializableObject): TCustomSerializableObject;
var
  lObjClass: TCustomSerializableObjectClass;
  lClassName: string;
begin
  if (ANode.Attributes['ClassName'] <> Null) and (ANode.Attributes['ClassName'] <> Unassigned) then
    lClassName := ANode.Attributes['ClassName'];

  if (lClassName = EmptyStr) and (Assigned(AOwner)) and (AOwner is TCustomSerializableList) then
    lClassName := TCustomSerializableList(AOwner).GetItemClass.ClassName;

  lObjClass := FileClassRegistrer.GetClass(lClassName);
  if lObjClass = nil then
    raise Exception.CreateFmt(R_CLASS_NAME_NOT_REGISTERED, [lClassName]);

  if not lObjClass.InheritsFrom(TCustomSerializableObject) then
    raise Exception.CreateFmt(R_CLASS_NAME_INVALID, [lClassName]);

  Result := lObjClass.Create(AOwner, ANode.NodeName);
  Result.LoadFromNode(ANode);
end;

class function TCustomSerializableObject.CreateFromXML(const AXMLString : string;
  const AOwner: TCustomSerializableObject): TCustomSerializableObject;
var
  lClassName: string;
  lObjClass: TCustomSerializableObjectClass;
begin
  if Assigned(AOwner) and (AOwner is TCustomSerializableList) then
    lClassName := TCustomSerializableList(AOwner).GetItemClassName
  else
    lClassName := Self.ClassName;

  if lClassName = '' then
    raise Exception.Create(R_CLASS_ID_NOT_FOUND);

  lObjClass := Self;

  Result := lObjClass.Create(nil);
  Result.LoadFromXmlString(AXmlString);
end;

destructor TCustomSerializableObject.Destroy;
begin
  Finalize;
  if FJSonObject <> nil then
    FJSonObject := nil;
  inherited;
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: Boolean);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsBoolean;
  FreeAndNil(lField);
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: Integer);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil(lField);
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: Word);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil( lField );
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: Char);
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

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: Double);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsFloat;
  FreeAndNil( lField );
end;

procedure TCustomSerializableObject.Finalize;
begin
// dummy
end;

procedure TCustomSerializableObject.FromNode(const AFieldName : string; out AItem: ISerializableBase);
var
  lNode: IXMLNode;
  lPair: TJSONPair;
  lTmpObject: TJSONObject;
begin
  if FFormatType = sfUnknown then
    raise Exception.Create(R_SERIALIZE_UNKNOWN_FILE_TYPE);

  if FFormatType = sfXML then
  begin
    lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
    if lNode <> nil then
      FromNode(lNode, AItem);
  end
  else
    if FFormatType = sfJSON then
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

procedure TCustomSerializableObject.FromNode(const AFieldName: string; const AItem: TCustomSerializableObject);
var
  lNode: IXMLNode;
  lNodeName: string;
  lPair: TJSONPair;
  lTmpObject: TJSONObject;
  lJsonStr: string;
begin
  AItem.FNodeName := AFieldName;

  if FFormatType = sfXML then
  begin
    lNode := FXMLNode.ChildNodes.FindNode(AItem.FNodeName);
    AItem.FormatType := sfXML;
    if lNode <> nil then
      AItem.LoadFromNode(lNode);
  end
  else // object inside object
    if FFormatType = sfJSON then
    begin
      lNodeName := AItem.FNodeName;
      AItem.FormatType := sfJSON;

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
      raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_UNKNOWN_OBJECT_TYPE, [AItem.ClassName]);
end;

procedure TCustomSerializableObject.FromNode(const ANode: IXMLNode; out AItem: ISerializableBase);
var
  lObjClass: TCustomSerializableObjectClass;
  lNodeClass: IXMLNode;
begin
  lNodeClass := ANode.AttributeNodes.FindNode('ClassName');
  if lNodeClass <> nil then
  begin
    lObjClass := FileClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lObjClass <> nil then
    begin
      AItem := lObjClass.Create(Self);
      AItem.LoadFromNode(ANode);
    end;
  end;
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string;
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

procedure TCustomSerializableObject.FromNode(const AFieldName : string; const AList: TCustomSerializableList);
var
  lNode: IXMLNode;
  lPair: TJSONPair;
  lJsonStr: string;
  lTmpObject: TJSONObject;
begin
  AList.NodeName := AFieldName;
  if FFormatType = sfXML then
  begin
    lNode := FXMLNode.ChildNodes.FindNode(AList.FNodeName);
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

procedure TCustomSerializableObject.FromNode(const AJSon: IJSONObject; out AItem: ISerializableBase);
var
  lObjClass: TCustomSerializableObjectClass;
  lPair: TJSONPair;
  lNameClass: string;
begin
  lPair := AJSon.Get('ClassName');
  if Assigned(lPair) then
    lNameClass := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll])
  else
    lNameClass := EmptyStr;

  if lNameClass <> EmptyStr then
  begin
    lObjClass := FileClassRegistrer.GetClass(lNameClass);
    if lObjClass <> nil then
    begin
      AItem := lObjClass.Create(Self);
      AItem.LoadFromString( AJSon.ToString );
    end;
  end;
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; const AFieldClass: TStrings);
var
  lAux : string;
  lTmp: TStringList;
  i: Integer;
  lField : TRpFieldsBase;
begin
  if FFormatType = sfXML then
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

function TCustomSerializableObject.FromNodeAsInterface(const ANode: IXMLNode): ISerializableBase;
var
  lObjClass: TCustomSerializableObjectClass;
  lNodeClass: IXMLNode;
begin
  lNodeClass := ANode.AttributeNodes.FindNode('ClassName');
  if lNodeClass <> nil then
  begin
    lObjClass := FileClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lObjClass <> nil then
    begin
      Result := lObjClass.Create(Self);
      Result.LoadFromNode(ANode);
    end;
  end;
end;

function TCustomSerializableObject.FromNodeAsInterface(const AFieldName: string): ISerializableBase;
var
  lNode: IXMLNode;
begin
  lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
  if lNode <> nil then
    Result := FromNodeAsInterface(lNode);
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: TTime);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := Frac(lField.AsDateTime);
  FreeAndNil( lField );
end;

procedure TCustomSerializableObject.FromNode(const AFieldName : string; out AEnumOrdValue : Cardinal; ATypeInfo : PTypeInfo);
var
  lStr : string;
begin
  FromNode(AFieldName, lStr);
  if lStr = '' then
    AEnumOrdValue := 0
  else
    AEnumOrdValue := TRpStrings.StrToEnumOrd(ATypeInfo, lStr);
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; out AValue: TDateTime);
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

function TCustomSerializableObject.GetXMLFile: IXMLDocument;
begin
  Result := FXMLFile;
end;

function TCustomSerializableObject.GetClassName: string;
begin
  Result := Self.ClassName;
end;

function TCustomSerializableObject.GetFileName: string;
begin
  Result := FFileName;
end;

function TCustomSerializableObject.GetFormatType: TSerializationFormat;
begin
  Result := FFormatType;
end;

function TCustomSerializableObject.GetIncludeClassName: Boolean;
begin
  Result := FIncludeClassName;
end;

function TCustomSerializableObject.GetJSonObject: IJSONObject;
begin
  Result := FJSonObject;
end;

function TCustomSerializableObject.GetXMLNode: IXMLNode;
begin
  Result := FXMLNode;
end;

function TCustomSerializableObject.GetNodeName: string;
begin
  Result := FNodeName;
end;

function TCustomSerializableObject.GetParent: TObject;
begin
  Result := FParent;
end;

function TCustomSerializableObject.GetParentFileName: string;
var
  lParent: TObject;
  lIntf: ISerializableBase;
  lIntf2: ISerializationSupport;
begin
  lParent := Self;
  Result := '';
  while (Result = '') and (lParent <> nil) do
  begin
    if lParent.InheritsFrom(TCustomSerializableObject) then
    begin
      Result := TCustomSerializableObject(lParent).FFileName;
      lParent := TCustomSerializableObject(lParent).Parent;
    end
    else
      if Supports(lParent, ISerializableBase, lIntf) then
      begin
        Result := lIntf.FileName;
        lParent := lIntf.Parent;
      end
      else
        if Supports(lParent, ISerializationSupport, lIntf2) then
        begin
          Result := lIntf2.GetSerializationController.FileName;
          lParent := lIntf2.GetSerializationController.Parent;
        end
        else
          lParent := nil;

    if FFileName = Result then
      Exit;
  end;
end;

function TCustomSerializableObject.GetProxyMode: Boolean;
begin
  Result := FProxyMode;
end;

function TCustomSerializableObject.GetRaiseExceptions: Boolean;
begin
  Result := False;
end;

function TCustomSerializableObject.GetXMLDoc(const AIncludeParent: Boolean): IXMLDocument;
begin
  SaveToNode('', AIncludeParent);
  Result := FXMLFile;
end;

procedure TCustomSerializableObject.Initialize;
begin
  try
    Reset;
  except
    on E:EAbstractError do
      raise Exception.CreateFmt(R_SERIALIZE_RESET_ABSTRACT, [Self.ClassName]);
    on E:Exception do
      raise Exception.Create(E.Message);
  end;
end;

procedure TCustomSerializableObject.InitializeJson;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;

  if FIncludeClassName or FProxyMode then
    FJSonObject.AddPair('ClassName', Self.ClassName);
end;

function TCustomSerializableObject.InternalGetField(const AFieldName: string): TRpFieldsBase;
begin
  if FFormatType = sfXML then
    Result := TXMLFieldNode.Create(FXMLNode, AFieldName)
  else
    Result := TJSONFieldNode.Create(FJSonObject, AFieldName);
end;

procedure TCustomSerializableObject.InternalSaveJSON(const AFieldName: string; const AValue: Variant);
var
  lValType: TFieldType;

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
  lValType := VarTypeToDataType(VarType(AValue));
  case lValType of
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
    raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_FIELD_VALUE_ERROR_EX, [AFieldName, Ord(lValType) ]);
  end;
end;

procedure TCustomSerializableObject.InternalSaveNodeProxy(const AFieldName: string;
  const AValue: Variant;const AAttributeValue : string);
var
  lNode: IXMLNode;
begin
  case VarTypeToDataType(VarType(AValue)) of
    DB.ftSmallInt, DB.ftInteger :
      FXMLNode[AFieldName] := 'Integer';

    DB.ftBCD, DB.ftFloat, DB.ftLargeInt, DB.ftCurrency :
      FXMLNode[AFieldName] := 'Double';

    DB.ftDateTime, DB.ftTimeStamp, DB.ftTime :
      FXMLNode[AFieldName] := 'TDateTime';

    DB.ftBoolean :
      FXMLNode[AFieldName] := 'Boolean';

    DB.ftString, DB.ftWideString, DB.ftFixedChar, DB.ftFixedWideChar :
      FXMLNode[AFieldName] := 'String';
  else
    raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_FIELD_VALUE_ERROR, [AFieldName]);
  end;

  if AAttributeValue <> EmptyStr then
  begin
    lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
    if lNode <> nil then
      lNode.Attributes['info'] := AAttributeValue;
  end;
end;

procedure TCustomSerializableObject.LoadFromFile(const AFile: string; const AFormat : TSerializationFormat = sfUnknown);
var
  lRootNode: IXMLNode;
  lFormat : TSerializationFormat;
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
    if AFormat = sfUnknown then
    begin
      InternalLoadFile;
      lChar := Copy(lFileStr.Text, 1, 1);
      if lChar = '<' then
        lFormat := sfXML
      else
        if lChar = '{' then
          lFormat := sfJSON
        else
          raise EBaseSerializationTypeUnknown.Create(R_SERIALIZE_UNKNOWN_STRING_FOTMAT);
    end
    else
      lFormat := AFormat;

    if lFormat = sfXML then
    begin
      FFormatType := sfXML;

      FXMLFile := TXMLDocument.Create(nil);
      FXMLFile.LoadFromFile(FFileName);
      lRootNode := FXMLFile.ChildNodes.FindNode('root');
      LoadFromNode(lRootNode.ChildNodes.FindNode(FNodeName));
      lRootNode := nil;
    end
    else
      if lFormat = sfJSON then
      begin
        if lFileStr = nil then
          InternalLoadFile;
        LoadFromJSONString( lFileStr.Text );
      end
      else
        raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_UNKNOWN_FILE_FORMAT, [sLineBreak, AFile]);
  finally
    if lFileStr <> nil then
      FreeAndNil( lFileStr );
  end;
end;

procedure TCustomSerializableObject.LoadFromJSONString(const AJSONString: string);
begin
  FFormatType := sfJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse( BytesOf(AJSONString), 0);
  DoLoadFromNode(nil);
end;

procedure TCustomSerializableObject.LoadFromNode(const ANode: IXMLNode);
var
  lIsNil : Boolean;
begin
  lIsNil := ANode = nil;

  if lIsNil and GetRaiseExceptions then
    raise Exception.Create(R_SERIALIZE_NODE_NOT_FOUND);

  if (not lIsNil) and GetRaiseExceptions and (ANode.Attributes['ClassName'] <> Self.ClassName) then
  begin
    raise Exception.CreateFmt(
      R_SERIALIZE_CLASS_NODE_ERROR,
      [ANode.Attributes['ClassName'], Self.ClassName]);
  end;

  if not lIsNil then
  begin
    FFormatType := sfXML;
    FXMLNode := ANode;
    DoLoadFromNode(FXMLNode);
  end;
end;

procedure TCustomSerializableObject.LoadFromString(const AString: string; const AFormat: TSerializationFormat);
var
  lFormato : TSerializationFormat;
  lChar: string;
  lStr: string;
begin
  lStr := Trim( AString );

  if AFormat = sfUnknown then
  begin
    lChar := Copy(lStr, 1, 1);
    if lChar = '<' then
      lFormato := sfXML
    else
      if lChar = '{' then
        lFormato := sfJSON
      else
        raise EBaseSerializationTypeUnknown.Create(R_SERIALIZE_UNKNOWN_STRING_FOTMAT);
  end
  else
    lFormato := AFormat;

  if lFormato = sfXML then
    LoadFromXmlString(lStr)
  else
    LoadFromJSONString(lStr);
end;

procedure TCustomSerializableObject.LoadFromXmlString(const AXmlString: string);
var
  lNodeRoot: IXMLNode;
  lStream: TStringStream;
begin
  FFormatType := sfXML;
  lStream := TStringStream.Create(AXmlString);
  try
    FXMLFile := TXMLDocument.Create(nil);
    FXMLFile.LoadFromStream(lStream);
    lNodeRoot := FXMLFile.ChildNodes.FindNode('root');
    LoadFromNode(lNodeRoot.ChildNodes[0]);
  finally
    lStream.Free;
  end;
end;

procedure TCustomSerializableObject.Save;
var
  lFile: TStringList;
begin
  FFileName := GetParentFileName;

  if FFileName = '' then
    raise Exception.Create(R_SERIALIZE_NO_FILE_NAME);

  SaveToNode;

  if FFormatType = sfXML then
    FXMLFile.SaveToFile(FFileName)
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

function TCustomSerializableObject.SaveFileToXmlString: string;
begin
  FFormatType := sfXML;
  DoSaveToNode;
  Result := FXMLFile.XML.Text;
end;

procedure TCustomSerializableObject.SaveToFile(const AFile: string; const AFormat : TSerializationFormat = sfUnknown);
var
  lExt : string;
begin
  if AFormat <> sfUnknown then
    FFormatType := AFormat;

  if FFormatType = sfUnknown then
  begin
    lExt := AnsiUpperCase(ExtractFileExt(AFile));
    if (lExt = '.JSON') or (lExt = '.JSN') or (lExt = '.JSO') or (lExt = '.J') then
      FFormatType := sfJSON
    else
      FFormatType := sfXML
  end;

  FFileName := AFile;
  Save;
end;

function TCustomSerializableObject.SaveToJSONString: string;
begin
  FFormatType := sfJSON;

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

procedure TCustomSerializableObject.SaveToNode(const ANodeName: string; const ASaveParent: Boolean);
var
  lNode: IXMLNode;
  lCreateNewFile: Boolean;
  lIntf: ISerializableBase;
  lIntf2: ISerializationSupport;
begin
  lNode := nil;
  lCreateNewFile := True;
  if (FParent <> nil) and ASaveParent then
  begin
    // Inheritance test
    if FParent.InheritsFrom(TCustomSerializableObject) then
    begin
      FXMLFile := TCustomSerializableObject(FParent).GetXMLFile;
      FFormatType := TCustomSerializableObject(FParent).FormatType;
      FIncludeClassName := TCustomSerializableObject(FParent).IncludeClassName;
      lNode := TCustomSerializableObject(FParent).GetXMLNode;
    end
    else
      if FParent.InheritsFrom(TCustomSerializableList) then
      begin
        FXMLFile := TCustomSerializableList(FParent).GetXMLFile;
        FFormatType := TCustomSerializableList(FParent).FormatType;
        FIncludeClassName := TCustomSerializableList(FParent).IncludeClassName;
        lNode := TCustomSerializableList(FParent).GetXMLNode;
      end
      else
        // Interface test
        if Supports(FParent, ISerializableBase, lIntf) then
        begin
          FXMLFile := lIntf.XMLFile;
          FFormatType := lIntf.FormatType;
          FIncludeClassName := lIntf.IncludeClassName;
          lNode := lIntf.XMLNode;
        end
        else
          if Supports(FParent, ISerializationSupport, lIntf2) then
          begin
            FXMLFile := lIntf2.GetSerializationController.XMLFile;
            FFormatType := lIntf2.GetSerializationController.FormatType;
            FIncludeClassName := lIntf2.GetSerializationController.IncludeClassName;
            lNode := lIntf2.GetSerializationController.XMLNode;
          end;

    lCreateNewFile := not Assigned(FXMLFile);
  end;

  if ANodeName <> '' then
    FNodeName := ANodeName;

  if FFormatType = sfUnknown then
    FFormatType := C_DEFAULT_FORMAT;

  case FFormatType of
  sfXML:
    begin
      if lCreateNewFile then
      begin
        FXMLFile := TXMLDocument.Create(nil);
        FXMLFile.Active := True;
        FXMLFile.Encoding := C_XML_ENCODING;
        FXMLFile.Version := '1.0';
        FXMLFile.StandAlone := 'yes';
      end;

      if (lNode = nil) then
        lNode := FXMLFile.AddChild('root');

      FXMLNode := lNode.AddChild(FNodeName);
      if FIncludeClassName or FProxyMode then
        FXMLNode.Attributes['ClassName'] := Self.ClassName;
    end;

  sfJSON:
    InitializeJson;

  sfUnknown:
    raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_UNKNOWN_FILE_TYPE_EX, [Self.ClassName]);
  end;

  DoSaveToNode;
end;

function TCustomSerializableObject.SaveToXmlString: string;
var
  lFile: IXMLDocument;
  lNode: IXMLNode;
begin
  FFormatType := sfXML;

  lFile := TXMLDocument.Create(nil);
  lFile.Active := True;
  lFile.Encoding := C_XML_ENCODING;
  lFile.Version := '1.0';
  lFile.StandAlone := 'yes';
  lNode := lFile.AddChild('root');

  FXMLNode := lNode.AddChild(FNodeName);
  if FIncludeClassName or FProxyMode then
    FXMLNode.Attributes['ClassName'] := Self.ClassName;

  DoSaveToNode;

  if FProxyMode then
  begin
    CheckPropertyDependency(TCustomSerializableObjectClass(Self.ClassType));

    FXMLNode.Attributes['ParentClass'] := Self.ClassParent.ClassName;

    if Self is TCustomSerializableList then
    begin
      FXMLNode.Attributes['isList'] := 'true';
      FXMLNode.Attributes['itemClass'] := TCustomSerializableList(Self).ItemClassName;
    end;
  end;

  Result := lFile.XML.Text;
  // check for XML Encoding on file text
  if Pos(C_XML_ENCODING, Result) <= 0 then
    Insert(' encoding="' + C_XML_ENCODING + '"', Result, Pos('"1.0"', Result) + 5);
end;

procedure TCustomSerializableObject.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TCustomSerializableObject.SetFormatType(const Value: TSerializationFormat);
begin
  FFormatType := Value;
end;

procedure TCustomSerializableObject.SetIncludeClassName(const AValue: Boolean);
begin
  FIncludeClassName := AValue;
end;

procedure TCustomSerializableObject.SetLastError(const AError: string);
begin
  FLastError := AError;
end;

procedure TCustomSerializableObject.SetNodeName(const Value: string);
begin
  FNodeName := TRpStrings.RemoveAccent( Value );
end;

procedure TCustomSerializableObject.SetParent(const Value: TObject);
begin
  FParent := Value;
end;

procedure TCustomSerializableObject.SetProxyMode(const AValue: Boolean);
begin
  FProxyMode := AValue;
end;

procedure TCustomSerializableObject.ToNode(const AFieldName: string; const AFieldClass: TStrings);
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

  if FormatType = sfXML then
  begin
    ToNode(AFieldName, lAux);
  end else
    FJSonObject.AddPair(AFieldName, TJSONStrinNoQuote.Create( lAux ));
end;

procedure TCustomSerializableObject.ToNode(const AFieldName, AValue: string;
  const ABase64: Boolean);
begin
  if FProxyMode then
  begin
    InternalSaveNodeProxy(AFieldName, AValue, IfThen(ABase64, 'AsBase64', ''));
  end else
    if ABase64 and (AValue <> '') then
      ToNode(AFieldName, Algorithms.Base64CompressedString(AValue))
    else
      ToNode(AFieldName, AValue);
end;

procedure TCustomSerializableObject.ToNode(const AFieldName : string; const AList: TCustomSerializableList);
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

      if FFormatType = sfJSON then
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

procedure TCustomSerializableObject.ToNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; const ASetValue);
var
  lStr : string;
  lItensInfo: PTypeInfo;
  lNode: IXMLNode;
begin
  if FProxyMode then
  begin
    // save the enum value of the set type
    lItensInfo := GetTypeData(ASetTypeInfo)^.CompType^;
    ToNode(AFieldName, 0, lItensInfo);

    FXMLNode[AFieldName] := ASetTypeInfo.Name;
    lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
    lNode.Attributes['ex'] := 'set';
    FXMLNode.Attributes[String(ASetTypeInfo.Name)] := 'set of ' + lItensInfo.Name;
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

procedure TCustomSerializableObject.ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo);
var
  lStr : string;
  lValues: string;
  I: Integer;
  T: PTypeData;
  lNode: IXMLNode;
begin
  if FProxyMode then
  begin
    FXMLNode[AFieldName] := ASetTypeInfo.Name;
    lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
    lNode.Attributes['ex'] := 'enum';

    T := GetTypeData(GetTypeData(ASetTypeInfo)^.BaseType^);

    lValues := '';
    for I := T^.MinValue to T^.MaxValue do
    begin
      if lValues <> '' then
        lValues := lValues + ',';
      lValues := lValues  + GetEnumName(ASetTypeInfo, I);
    end;

    FXMLNode.Attributes[string(ASetTypeInfo.Name)] := lValues;
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

procedure TCustomSerializableObject.ToNode(const AFieldName : string; const AItem: ISerializableBase);
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

    if AItem.FormatType = sfJSON then
      FJSonObject.AddPair(AItem.GetNodeName, TJSONObject(AItem.JSonObject.Clone));
  end;
end;

procedure TCustomSerializableObject.ToNode(const AFieldName: string; const AValue: Variant);
var
  lClear: Boolean;
begin
  if FFormatType = sfUnknown then
    FFormatType := C_DEFAULT_FORMAT;

  if FProxyMode then
    InternalSaveNodeProxy(AFieldName, AValue)
  else
  begin
    lClear := (VarIsStr(AValue) and (Trim(AValue) = ''))
           or (VarIsNumeric(AValue) and (AValue = 0))
           or ((FindVarData(AValue)^.VType = varDate) and (AValue <= 2));

    if not lClear then
    begin
      if FFormatType = sfXML then
      begin
        if FXMLNode <> nil then
        begin
          case VarType(AValue) of
            varDate: FXMLNode[AFieldName] := TXMLFieldNode.GetDateFormated(AValue);
            varDouble: FXMLNode[AFieldName] := TXMLFieldNode.GetDoubleFormated(AValue);
          else
            FXMLNode[AFieldName] := AValue;
          end;
        end;
      end
      else
        if FFormatType = sfJSON then
          InternalSaveJSON(AFieldName, AValue)
        else
          raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_WRITE_ERROR, [Self.ClassName]);
    end;
  end;
end;

procedure TCustomSerializableObject.FromNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; out ASetVar);
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
  lIntf: ISerializationSupport;
  lNode: IXMLNode;
begin
  if Assigned(FParent) and Parent.InheritsFrom(TComponent) then
    for i := 0 to TComponent(FParent).ComponentCount - 1 do
    begin
      if Supports(TComponent(FParent).Components[i], ISerializationSupport, lIntf) then
      begin
        lNode := ANode.ChildNodes.FindNode(lIntf.GetSerializationController.GetnodeName);
        lIntf.GetSerializationController.LoadFromNode(lNode);
      end;
    end;
end;

procedure TRpContainerFileController.DoSaveToNode;
var
  i: Integer;
  lIntf: ISerializationSupport;
begin
  if Assigned(FParent) and FParent.InheritsFrom(TComponent) then
    for i := 0 to TComponent(FParent).ComponentCount - 1 do
    begin
      if Supports(TComponent(FParent).Components[i], ISerializationSupport, lIntf) then
        lIntf.GetSerializationController.SaveToNode(lIntf.GetSerializationNodeName, True);
//        lIntf.GetFileController.SaveToNode(FArquivo, FNode, lIntf.GetXMLNodeName );
    end;
//  else
//    if Assigned(FParent) and FParent.InheritsFrom(TWinControl) then
end;

{ TXMLCustomList }

function TCustomSerializableList.Add(const ACustomFileObject: TCustomSerializableObject) : Integer;
begin
  InitList;

  Result := FItens.Add(ACustomFileObject);
  ACustomFileObject.Parent := Self;
end;

procedure TCustomSerializableList.Clear;
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
  FXMLNode := nil;
end;

function TCustomSerializableList.Count: Integer;
begin
  InitList;
  Result := FItens.Count;
end;

constructor TCustomSerializableList.Create(AOwner: TObject; const ANodeName: string;
  const AOwnsObjects: Boolean);
begin
  inherited Create(AOwner, ANodeName);
  InitList;
  FItens.OwnsObjects := AOwnsObjects;

  InitializeJson;
end;

function TCustomSerializableList.Delete(const AIndex: Integer): Boolean;
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

destructor TCustomSerializableList.Destroy;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  inherited;
end;

procedure TCustomSerializableList.DoLoadFromNode(const ANode: IXMLNode);
begin
// dummy
end;

procedure TCustomSerializableList.DoSaveToNode;
var
  i: Integer;
  lJsonList: TJSONArray;
  lStr: string;
begin
  inherited;

  if FFormatType = sfJSON then
    lJsonList := TJSONArray.Create;

  for i := 0 to FItens.Count - 1 do
  begin
    {$IFDEF XE3UP}
    FItens[i].SaveToNode;
    {$ELSE}
    TCustomSerializableObject(FItens[i]).SaveToNode;
    {$ENDIF}

    if FFormatType = sfJSON then
    begin
      lStr := TCustomSerializableObject(Items[i]).FJSonObject.ToString;
      {$IFDEF XE8UP}
      lJsonList.AddElement( TCustomSerializableObject(Items[i]).FJSonObject.Clone as TJSONValue);
      {$ELSE}
      lJsonList.AddElement( TCustomSerializableObject(Items[i]).FJSonObject.Clone as TJSONObject );
      {$ENDIF}
    end;
  end;

  if FFormatType = sfJSON then
  begin
    FJSonObject.AddPair('Itens', TJSONArray(lJsonList.Clone));
    FreeAndNil( lJsonList );
  end;
end;

procedure TCustomSerializableList.Finalize;
begin
  Clear;
  if Assigned(FItens) then
  begin
    FItens.Clear;
    FreeAndNil(FItens);
  end;

  inherited;
end;

procedure TCustomSerializableList.FromOther(const AOther: ISerializableBase);
var
  lItem: TCustomSerializableObject;
  i: Integer;
begin
  if (AOther <> nil) and (AOther is TCustomSerializableList) and (TCustomSerializableList(AOther).Count > 0) then
  begin
    for i := 0 to TCustomSerializableList(AOther).Count - 1 do
    begin
      lItem := TCustomSerializableList(AOther).GetItemClass.Create(Self);
      lItem.FromOther( TCustomSerializableList(AOther).Items[i] );
      Self.Add(lItem);
    end;
  end;
end;

function TCustomSerializableList.GetItem(AIndex: Integer): TCustomSerializableObject;
begin
  InitList;
  Result := nil;
  if (FItens.Count > 0) and (AIndex >= 0) then
  begin
    {$IFDEF XE3UP}
    Result := FItens[AIndex];
    {$ELSE}
    Result := TCustomSerializableObject(FItens[AIndex]);
    {$ENDIF}
  end;
end;

function TCustomSerializableList.GetItemClassName: string;
begin
  Result := GetItemClass.ClassName;
end;

function TCustomSerializableList.GetOwnsObjects: Boolean;
begin
  InitList;
  Result := FItens.OwnsObjects;
end;

procedure TCustomSerializableList.Initialize;
begin
  inherited;
end;

procedure TCustomSerializableList.InitList;
begin
  if not Assigned(FItens) then
    FItens := TObjectList{$IFDEF XE3UP}<TCustomSerializableObject>{$ENDIF}.Create;
end;

function TCustomSerializableList.Insert(const ACustomFileObject: TCustomSerializableObject;
  const AIndex: Integer) : Integer;
begin
  InitList;
  ACustomFileObject.Parent := Self;
  FItens.Insert(AIndex, ACustomFileObject);
  Result := FItens.IndexOf(ACustomFileObject);
end;

procedure TCustomSerializableList.LoadFromJSONString(const AJSONString: string);
var
  i: Integer;
  lObject: TCustomSerializableObject;
  lPair: TJSONPair;
  lJsonList: TJSONArray;
  lTempStr: string;
begin
  FFormatType := sfJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  lTempStr := AJSONString;
  // AJSONString must be an array: starting with '[', and ending with ']'
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

  // if parse goes ok
  if (FJSonObject <> nil) then
  begin
    // list
    lPair := FJSonObject.Get('Itens');
    if lPair <> nil then
    begin
      lJsonList := lPair.JsonValue as TJSONArray;
      for i := 0 to lJsonList.Size - 1 do
      begin
        lObject := TCustomSerializableObject.CreateFromJson(lJsonList.Get(i) as TJSONObject, Self);
        Add(lObject);
      end;
    end;
  end;
end;

procedure TCustomSerializableList.LoadFromNode(const ANode: IXMLNode);
var
  I: Integer;
  lObject: TCustomSerializableObject;
begin
  Clear;

  FXMLNode := ANode;
  if FXMLNode <> nil then
  begin
    for I := 0 to FXMLNode.ChildNodes.Count - 1 do
    begin
      lObject := TCustomSerializableObject.CreateFromNode(FXMLNode.ChildNodes[I], Self);
      Add(lObject);
    end;
  end;
end;

procedure TCustomSerializableList.LoadFromXmlString(const AXmlValue: string);
var
  lXmlDoc : IXMLDocument;
  lXmlNode : IXMLNode;
  lStream: TStringStream;
begin
  FFormatType := sfXML;
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

function TCustomSerializableList.Remove(const ACustomFileObject: TCustomSerializableObject): Integer;
begin
  InitList;
  Result := FItens.Remove(ACustomFileObject);
end;

procedure TCustomSerializableList.Reset;
begin
  Self.Clear;
end;

procedure TCustomSerializableList.SetOwnsObjects(const Value: Boolean);
begin
  InitList;
  FItens.OwnsObjects := Value;
end;

{ TXMLClassRegister }

function TFileClassRegister.GetClass(const AClassName: string): TCustomSerializableObjectClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].FileClassName = AClassName then
    begin
      Result := TCustomSerializableObjectClass(Items[I].FileClass);
      Exit;
    end;
end;

function TFileClassRegister.GetClass(const AIndex: Integer): TCustomSerializableObjectClass;
begin
  Result := TCustomSerializableObjectClass(Items[AIndex].FileClass);
end;

function TFileClassRegister.GetItem(const Index: Integer): TFileClassRegisterItem;
begin
  Result := TFileClassRegisterItem(Self.Get(Index));
end;

procedure TFileClassRegister.Registrar(const AClass: TCustomSerializableObjectClass);
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

class function FileClassRegistrer.GetClass(const AClassName: string): TCustomSerializableObjectClass;
begin
  if TFileClassRegister.InternalClassRegister = nil then
    TFileClassRegister.InternalClassRegister := TFileClassRegister.Create;

  Result := TFileClassRegister.InternalClassRegister.GetClass(AClassName);
end;

class function FileClassRegistrer.GetClass(
  const AIndex: Integer): TCustomSerializableObjectClass;
begin
  if TFileClassRegister.InternalClassRegister = nil then
    TFileClassRegister.InternalClassRegister := TFileClassRegister.Create;

  Result := TFileClassRegister.InternalClassRegister.GetClass(AIndex);
end;

class procedure FileClassRegistrer.Registrar(const AClass: TCustomSerializableObjectClass);
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
    if FormatType = sfJSON then
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
  lIntf: ISerializationSupport;
begin
  if GetNewItem(lObj) then
  begin
    if Supports(lObj, ISerializationSupport, lIntf) then
      lIntf.GetSerializationController.LoadFromNode(ANode)
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
  lIntf: ISerializationSupport;
  lJsonList: TJSONArray;
  lStr: String;
begin
  if Assigned(FParent) then
  begin
    if FFormatType = sfJSON then
      lJsonList := TJSONArray.Create;

    // TCollection
    if FParent.InheritsFrom(TCollection) then
      for i := 0 to TCollection(FParent).Count - 1 do
      begin
        if Supports(TCollection(FParent).Items[i], ISerializationSupport, lIntf) then
        begin
          lIntf.GetSerializationController.SaveToNode(lIntf.GetSerializationNodeName, True);
          if FFormatType = sfJSON then
          begin
            lStr := lIntf.GetSerializationController.JSonObject.ToString;
            lJsonList.AddElement( lIntf.GetSerializationController.JSonObject.Clone as TJSONObject );
          end;
        end;
      end
    // TList, TObjectList
    else if FParent.InheritsFrom(TList) then
      for i := 0 to TList(FParent).Count - 1 do
      begin
        if Supports(TList(FParent).Items[i], ISerializationSupport, lIntf) then
        begin
          lIntf.GetSerializationController.SaveToNode(lIntf.GetSerializationNodeName, True);
          if FFormatType = sfJSON then
          begin
            lStr := lIntf.GetSerializationController.JSonObject.ToString;
            lJsonList.AddElement( lIntf.GetSerializationController.JSonObject.Clone as TJSONObject );
          end;
        end;
      end;

    if FFormatType = sfJSON then
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
    lIntf: ISerializationSupport;
  begin
    if GetNewItem(lObj) then
    begin
      if Supports(lObj, ISerializationSupport, lIntf) then
        lIntf.GetSerializationController.LoadFromJSONString(lTempStr)
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
  FFormatType := sfJSON;

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

procedure TRpBaseFileController.FromOther(const AOther: ISerializableBase);
begin
end;

procedure TRpBaseFileController.Reset;
begin
end;

procedure TCustomSerializableObject.ToNode(const AFieldName: string; const AItem: ISerializableBase; const AGuid: TGuid);
begin
  if AItem <> nil then
  begin
    AItem.SaveToNode(AFieldName);
    if FFormatType = sfXML then
      AItem.XMLNode.Attributes['GUID'] := GUIDToString(AGuid)
    else
    begin
      AItem.JSonObject.AddPair('GUID', GUIDToString(AGuid));
      FJSonObject.AddPair(AFieldName, AItem.JSonObject.Clone as TJSONObject);
    end;
  end;
end;

procedure TCustomSerializableObject.ToNode(const AFieldName: string; const AList: TInterfaceList; const AGuid: TGuid);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(AList[I], ISerializableBase) then
      ToNode(AFieldName, AList[I] as ISerializableBase, AGuid);
  end;
end;

procedure TCustomSerializableObject.CheckPropertyDependency(
  const AClassParent: TCustomSerializableObjectClass);
var
  lParent : TCustomSerializableObject;
  lNode, lNodeParent: IXMLNode;
  i: Integer;
begin
  if (AClassParent <> TCustomSerializableObject) and (AClassParent <> TCustomSerializableList) then
  begin
    lParent := AClassParent.Create(nil);
    try
      lParent.ProxyMode := True;
      CheckPropertyDependency(TCustomSerializableObjectClass(lParent.ClassParent));

      lParent.SaveToNode(lParent.ClassName, False);
      for i := 0 to lParent.XMLNode.ChildNodes.Count - 1 do
      begin
        lNodeParent := lParent.XMLNode.ChildNodes.Get(i);
        lNode := FXMLNode.ChildNodes.FindNode(lNodeParent.NodeName);
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
