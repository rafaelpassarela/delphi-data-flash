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
      System.Contnrs,
    {$ENDIF} // ANDROID
  {$ELSE}
  Contnrs, Dialogs,
  {$ENDIF} // XE3UP
  TypInfo, uRpFields, uRpJSONBase, uRpAlgorithms, DB, StrUtils, uRpResourceString;

type
  IXMLNode = XMLIntf.IXMLNode;

  TSerializationFormat = (sfXML, sfJSON, sfAuto);

  EBaseSerializationTypeUnknown = Exception;

  TBaseSerializableObject = class;
  TBaseSerializableObjectClass = class of TBaseSerializableObject;
  TBaseSerializableList = class;
  TBaseSerializableListClass = class of TBaseSerializableList;

  IBaseSerializable = interface
    ['{FC07098F-E2E1-47F7-A2FD-D72028DB71C3}']
    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True); overload;
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure LoadFromFile(const AFile : string; const AFormat : TSerializationFormat = sfAuto);
    procedure LoadFromString(const AString : string; const AFormat : TSerializationFormat = sfAuto);
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
    procedure SaveToFile(const AFile : string; const AFormat : TSerializationFormat = sfAuto);
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
    function GetSerializationController : IBaseSerializable;
    function GetSerializationNodeName: string;
  end;

  IBaseSerializableHelper = interface
    ['{539FE1C3-E805-491A-8AF2-E4411DD2B7D9}']
    function DoSave(const ADataComponent : TComponent) : Boolean;
    function DoLoad(const ADataComponent : TComponent) : Boolean;
    function DoDelete(const ADataComponent : TComponent) : Boolean;
    function DoExecute(const ADataComponent : TComponent) : Boolean;
  end;

  TBaseSerializableObject = class(TInterfacedPersistent, IBaseSerializable)
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
    FIncludeEmptyFields: Boolean;
    function GetParentFileName : string;
    function InternalGetField(const AFieldName : string) : TRpFieldsBase;
    function GetNodeName: string;
    procedure SetNodeName(const Value: string);
    procedure InternalSaveJSON(const AFieldName : string; const AValue : Variant);
    procedure InternalSaveNodeProxy(const AFieldName : string; const AValue : Variant;
      const AAttributeValue : string = '');
    procedure CheckPropertyDependency(const AClassParent : TBaseSerializableObjectClass);
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
    procedure FromNode(const AFieldName : string; const AItem : TBaseSerializableObject); overload;
    procedure FromNode(const AFieldName : string; const AList : TBaseSerializableList); overload;

    procedure FromNode(const AFieldName : string; out AItem : IBaseSerializable); overload;
    procedure FromNode(const ANode: IXMLNode;     out AItem : IBaseSerializable); overload;
    procedure FromNode(const AJSon: IJSONObject;  out AItem : IBaseSerializable); overload;

    function FromNodeAsInterface(const ANode : IXMLNode) : IBaseSerializable; overload;
    function FromNodeAsInterface(const AFieldName : string) : IBaseSerializable; overload;

    procedure ToNode(const AFieldName : string; const AValue : Variant); overload;
    procedure ToNode(const AFieldName : string; const AFieldClass : TStrings); overload;
    procedure ToNode(const AFieldName : string; const AValue : string; const ABase64 : Boolean); overload;
    procedure ToNode(const AFieldName : string; const AItem : IBaseSerializable); overload;
    procedure ToNode(const AFieldName : string; ASetTypeInfo : PTypeInfo; const ASetValue); overload;
    procedure ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo); overload;
    procedure ToNode(const AFieldName : string; const AList : TBaseSerializableList); overload;
    procedure ToNode(const AFieldName : string; const AItem : IBaseSerializable; const AGuid : TGuid); overload;
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
    procedure FromOther(const AOther : IBaseSerializable); virtual;
    procedure Assign(const AOther : IBaseSerializable); reintroduce;
    procedure SetLastError(const AError : string);

    procedure SaveToNode(const ANodeName : string = ''; const ASaveParent : Boolean = True);
    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure SaveToFile(const AFile : string; const AFormat : TSerializationFormat = sfAuto); virtual;

    // XML
    function SaveToXmlString : string; virtual;
    function SaveFileToXmlString : string; virtual;
    // JSON
    function SaveToJSONString : string; virtual;

    procedure LoadFromFile(const AFile : string; const AFormat : TSerializationFormat = sfAuto); virtual;
    procedure LoadFromString(const AString : string; const AFormat : TSerializationFormat = sfAuto); virtual;

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
    property IncludeEmptyFields : Boolean read FIncludeEmptyFields write FIncludeEmptyFields default False;
    property ProxyMode : Boolean read GetProxyMode write SetProxyMode;
    property LastError : string read FLastError;

    class function CreateFromNode(const ANode : IXMLNode; const AOwner : TBaseSerializableObject) : TBaseSerializableObject;
    class function CreateFromJson(const AJSonObject : IJSONObject; const AOwner : TBaseSerializableObject) : TBaseSerializableObject;
    class function CreateFromXML(const AXMLString : string; const AOwner : TBaseSerializableObject) : TBaseSerializableObject;
  end;

  TBaseSerializableList = class(TBaseSerializableObject)
  private
    FItens : TObjectList{$IFDEF XE3UP}<TBaseSerializableObject>{$ENDIF};
    procedure InitList;
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const Value: Boolean);
    function GetItemClassName: string;
  protected
    function GetItem(AIndex: Integer): TBaseSerializableObject;
    function GetItemClass : TBaseSerializableObjectClass; virtual; abstract;

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

    function Add(const ACustomFileObject : TBaseSerializableObject) : Integer;
    function Insert(const ACustomFileObject : TBaseSerializableObject; const AIndex : Integer = -1) : Integer;
    function Delete(const AIndex : Integer) : Boolean;
    function Remove(const ACustomFileObject : TBaseSerializableObject) : Integer;
    function Count : Integer;

    procedure LoadFromNode(const ANode : IXMLNode); overload;

    procedure Clear;
    procedure Reset; override;
    procedure FromOther(const AOther: IBaseSerializable); override;

    property Items[AIndex : Integer] : TBaseSerializableObject read GetItem; default;
    property OwnsObjects : Boolean read GetOwnsObjects write SetOwnsObjects;
    property ItemClassName : string read GetItemClassName;
  end;

  TCustomSerializableObjectEx = class(TBaseSerializableObject, IBaseSerializableHelper)
  protected
    function DoDelete(const ADataComponent: TComponent): Boolean; virtual;
    function DoSave(const ADataComponent: TComponent): Boolean; virtual;
    function DoExecute(const ADataComponent: TComponent): Boolean; virtual;
    function DoLoad(const ADataComponent: TComponent): Boolean; virtual;
  end;

  TSerializationClassRegisterItem = class
  private
    FSerializationClass: TBaseSerializableObjectClass;
    FSerializationClassName: string;
  public
    property SerializationClass : TBaseSerializableObjectClass read FSerializationClass write FSerializationClass;
    property SerializationClassName : string read FSerializationClassName write FSerializationClassName;
  end;

  TSerializationClassRegister = class(
    {$IFDEF XE3UP}
      {$IFDEF ANDROID}
        TList<TSerializationClassRegisterItem>
      {$ELSE}
        System.Contnrs.TObjectList
      {$ENDIF}
    {$ELSE}
      Contnrs.TObjectList
    {$ENDIF})
  protected
    {$IFNDEF ANDROID}
    function GetItem(const Index : Integer) : TSerializationClassRegisterItem;
    {$ENDIF}
  public
    class var InternalClassRegister: TSerializationClassRegister;
    procedure Registrate(const AClass : TBaseSerializableObjectClass);
    function GetClass(const AClassName : string) : TBaseSerializableObjectClass; overload;
    function GetClass(const AIndex : Integer) : TBaseSerializableObjectClass; overload;
    {$IFNDEF ANDROID}
    property Items[const Index: Integer]: TSerializationClassRegisterItem read GetItem; default;
    {$ENDIF}
  end;

  SerializationClassRegistrer = class
  public
    class procedure FreeRegister;
    class procedure Registrate(const AClass : TBaseSerializableObjectClass);
    class function GetClass(const AClassName : string) : TBaseSerializableObjectClass; overload;
    class function GetClass(const AIndex : Integer) : TBaseSerializableObjectClass; overload;
    class function Count : Integer;
  end;

  TSerializationBaseController = class(TBaseSerializableObject)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  public
    procedure Reset; override;
    procedure FromOther(const AOther: IBaseSerializable); override;
  end;

  // Used to control the save of forms, panels, groupBox, etc...
  // must be parent another component
  TContainerSerializationController = class(TSerializationBaseController)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode : IXMLNode); override;
  end;

  // Helper for TList, TObjectList, TCollection...
  TListSerializationController = class(TSerializationBaseController)
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

{ TCustomSerializableObject }

procedure TBaseSerializableObject.Assign(const AOther: IBaseSerializable);
var
  lNode: IXMLNode;
begin
  Reset;

  Self.FFormatType := AOther.FormatType;

  if AOther.FormatType = sfXML then
  begin
    AOther.SaveToNode;
    lNode := AOther.XMLNode.CloneNode(True);
    LoadFromNode(lNode);
  end else
  begin
    LoadFromJSONString( AOther.SaveToJSONString );
  end;
end;

constructor TBaseSerializableObject.Create(AOwner: TObject; const ANodeName: string);
begin
  FFormatType := C_DEFAULT_FORMAT;
  FIncludeClassName := True;
  FIncludeEmptyFields := False;
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

class function TBaseSerializableObject.CreateFromJson(const AJSonObject: IJSONObject;
  const AOwner: TBaseSerializableObject): TBaseSerializableObject;
var
  lObjClass: TBaseSerializableObjectClass;
  lClassName: string;
  lPair: TJSONPair;
begin
  lPair := AJSonObject.Get('ClassName');
  if Assigned(lPair) then
    lClassName := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll])
  else
  begin
    if Assigned(AOwner) and (AOwner is TBaseSerializableList) then
      lClassName := TBaseSerializableList(AOwner).GetItemClass.ClassName;
  end;

  if lClassName = '' then
    raise Exception.Create(R_CLASS_ID_NOT_FOUND);

  lObjClass := SerializationClassRegistrer.GetClass(lClassName);
  if lObjClass = nil then
    raise Exception.CreateFmt(R_CLASS_NAME_NOT_REGISTERED, [lClassName]);

  if not lObjClass.InheritsFrom(TBaseSerializableObject) then
    raise Exception.CreateFmt(R_CLASS_NAME_INVALID, [lClassName]);

  Result := lObjClass.Create(AOwner);
  Result.LoadFromJSONString( AJSonObject.ToString );
end;

class function TBaseSerializableObject.CreateFromNode(const ANode: IXMLNode;
  const AOwner: TBaseSerializableObject): TBaseSerializableObject;
var
  lObjClass: TBaseSerializableObjectClass;
  lClassName: string;
begin
  if (ANode.Attributes['ClassName'] <> Null) and (ANode.Attributes['ClassName'] <> Unassigned) then
    lClassName := ANode.Attributes['ClassName'];

  if (lClassName = EmptyStr) and (Assigned(AOwner)) and (AOwner is TBaseSerializableList) then
    lClassName := TBaseSerializableList(AOwner).GetItemClass.ClassName;

  lObjClass := SerializationClassRegistrer.GetClass(lClassName);
  if lObjClass = nil then
    raise Exception.CreateFmt(R_CLASS_NAME_NOT_REGISTERED, [lClassName]);

  if not lObjClass.InheritsFrom(TBaseSerializableObject) then
    raise Exception.CreateFmt(R_CLASS_NAME_INVALID, [lClassName]);

  Result := lObjClass.Create(AOwner, ANode.NodeName);
  Result.LoadFromNode(ANode);
end;

class function TBaseSerializableObject.CreateFromXML(const AXMLString : string;
  const AOwner: TBaseSerializableObject): TBaseSerializableObject;
var
  lClassName: string;
  lObjClass: TBaseSerializableObjectClass;
begin
  if Assigned(AOwner) and (AOwner is TBaseSerializableList) then
    lClassName := TBaseSerializableList(AOwner).GetItemClassName
  else
    lClassName := Self.ClassName;

  if lClassName = '' then
    raise Exception.Create(R_CLASS_ID_NOT_FOUND);

  lObjClass := Self;

  Result := lObjClass.Create(AOwner);
  Result.LoadFromXmlString(AXmlString);
end;

destructor TBaseSerializableObject.Destroy;
begin
  Finalize;
  if FJSonObject <> nil then
    FJSonObject := nil;
  inherited;
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: Boolean);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsBoolean;
  FreeAndNil(lField);
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: Integer);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil(lField);
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: Word);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsInteger;
  FreeAndNil( lField );
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: Char);
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

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: Double);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := lField.AsFloat;
  FreeAndNil( lField );
end;

procedure TBaseSerializableObject.Finalize;
begin
// dummy
end;

procedure TBaseSerializableObject.FromNode(const AFieldName : string; out AItem: IBaseSerializable);
var
  lNode: IXMLNode;
  lPair: TJSONPair;
  lTmpObject: TJSONObject;
begin
  if FFormatType = sfAuto then
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

procedure TBaseSerializableObject.FromNode(const AFieldName: string; const AItem: TBaseSerializableObject);
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

procedure TBaseSerializableObject.FromNode(const ANode: IXMLNode; out AItem: IBaseSerializable);
var
  lObjClass: TBaseSerializableObjectClass;
  lNodeClass: IXMLNode;
begin
  lNodeClass := ANode.AttributeNodes.FindNode('ClassName');
  if lNodeClass <> nil then
  begin
    lObjClass := SerializationClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lObjClass <> nil then
    begin
      AItem := lObjClass.Create(Self);
      AItem.LoadFromNode(ANode);
    end;
  end;
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string;
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

procedure TBaseSerializableObject.FromNode(const AFieldName : string; const AList: TBaseSerializableList);
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

procedure TBaseSerializableObject.FromNode(const AJSon: IJSONObject; out AItem: IBaseSerializable);
var
  lObjClass: TBaseSerializableObjectClass;
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
    lObjClass := SerializationClassRegistrer.GetClass(lNameClass);
    if lObjClass <> nil then
    begin
      AItem := lObjClass.Create(Self);
      AItem.LoadFromString( AJSon.ToString );
    end;
  end;
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; const AFieldClass: TStrings);
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

function TBaseSerializableObject.FromNodeAsInterface(const ANode: IXMLNode): IBaseSerializable;
var
  lObjClass: TBaseSerializableObjectClass;
  lNodeClass: IXMLNode;
begin
  lNodeClass := ANode.AttributeNodes.FindNode('ClassName');
  if lNodeClass <> nil then
  begin
    lObjClass := SerializationClassRegistrer.GetClass(String(ANode.Attributes['ClassName']));
    if lObjClass <> nil then
    begin
      Result := lObjClass.Create(Self);
      Result.LoadFromNode(ANode);
    end;
  end;
end;

function TBaseSerializableObject.FromNodeAsInterface(const AFieldName: string): IBaseSerializable;
var
  lNode: IXMLNode;
begin
  lNode := FXMLNode.ChildNodes.FindNode(AFieldName);
  if lNode <> nil then
    Result := FromNodeAsInterface(lNode);
end;

procedure TBaseSerializableObject.FromOther(const AOther: IBaseSerializable);
begin
  Assign(AOther);
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: TTime);
var
  lField : TRpFieldsBase;
begin
  lField := InternalGetField(AFieldName);
  AValue := Frac(lField.AsDateTime);
  FreeAndNil( lField );
end;

procedure TBaseSerializableObject.FromNode(const AFieldName : string; out AEnumOrdValue : Cardinal; ATypeInfo : PTypeInfo);
var
  lStr : string;
begin
  FromNode(AFieldName, lStr);
  if lStr = '' then
    AEnumOrdValue := 0
  else
    AEnumOrdValue := TRpStrings.StrToEnumOrd(ATypeInfo, lStr);
end;

procedure TBaseSerializableObject.FromNode(const AFieldName: string; out AValue: TDateTime);
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

function TBaseSerializableObject.GetXMLFile: IXMLDocument;
begin
  Result := FXMLFile;
end;

function TBaseSerializableObject.GetClassName: string;
begin
  Result := Self.ClassName;
end;

function TBaseSerializableObject.GetFileName: string;
begin
  Result := FFileName;
end;

function TBaseSerializableObject.GetFormatType: TSerializationFormat;
begin
  Result := FFormatType;
end;

function TBaseSerializableObject.GetIncludeClassName: Boolean;
begin
  Result := FIncludeClassName;
end;

function TBaseSerializableObject.GetJSonObject: IJSONObject;
begin
  Result := FJSonObject;
end;

function TBaseSerializableObject.GetXMLNode: IXMLNode;
begin
  Result := FXMLNode;
end;

function TBaseSerializableObject.GetNodeName: string;
begin
  Result := FNodeName;
end;

function TBaseSerializableObject.GetParent: TObject;
begin
  Result := FParent;
end;

function TBaseSerializableObject.GetParentFileName: string;
var
  lParent: TObject;
  lIntf: IBaseSerializable;
  lIntf2: ISerializationSupport;
begin
  lParent := Self;
  Result := '';
  while (Result = '') and (lParent <> nil) do
  begin
    if lParent.InheritsFrom(TBaseSerializableObject) then
    begin
      Result := TBaseSerializableObject(lParent).FFileName;
      lParent := TBaseSerializableObject(lParent).Parent;
    end
    else
      if Supports(lParent, IBaseSerializable, lIntf) then
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

function TBaseSerializableObject.GetProxyMode: Boolean;
begin
  Result := FProxyMode;
end;

function TBaseSerializableObject.GetRaiseExceptions: Boolean;
begin
  Result := False;
end;

function TBaseSerializableObject.GetXMLDoc(const AIncludeParent: Boolean): IXMLDocument;
begin
  SaveToNode('', AIncludeParent);
  Result := FXMLFile;
end;

procedure TBaseSerializableObject.Initialize;
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

procedure TBaseSerializableObject.InitializeJson;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;

  if FIncludeClassName or FProxyMode then
    FJSonObject.AddPair('ClassName', Self.ClassName);
end;

function TBaseSerializableObject.InternalGetField(const AFieldName: string): TRpFieldsBase;
begin
  if FFormatType = sfXML then
    Result := TXMLFieldNode.Create(FXMLNode, AFieldName)
  else
    Result := TJSONFieldNode.Create(FJSonObject, AFieldName);
end;

procedure TBaseSerializableObject.InternalSaveJSON(const AFieldName: string; const AValue: Variant);
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

procedure TBaseSerializableObject.InternalSaveNodeProxy(const AFieldName: string;
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

procedure TBaseSerializableObject.LoadFromFile(const AFile: string; const AFormat : TSerializationFormat = sfAuto);
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
    if AFormat = sfAuto then
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

procedure TBaseSerializableObject.LoadFromJSONString(const AJSONString: string);
begin
  FFormatType := sfJSON;

  if FJSonObject <> nil then
    FJSonObject := nil;

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse( BytesOf(AJSONString), 0);
  DoLoadFromNode(nil);
end;

procedure TBaseSerializableObject.LoadFromNode(const ANode: IXMLNode);
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

procedure TBaseSerializableObject.LoadFromString(const AString: string; const AFormat: TSerializationFormat);
var
  lFormato : TSerializationFormat;
  lChar: string;
  lStr: string;
begin
  lStr := Trim( AString );

  if AFormat = sfAuto then
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

procedure TBaseSerializableObject.LoadFromXmlString(const AXmlString: string);
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

procedure TBaseSerializableObject.Save;
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

function TBaseSerializableObject.SaveFileToXmlString: string;
begin
  FFormatType := sfXML;
  DoSaveToNode;
  Result := FXMLFile.XML.Text;
end;

procedure TBaseSerializableObject.SaveToFile(const AFile: string; const AFormat : TSerializationFormat = sfAuto);
var
  lExt : string;
begin
  if AFormat <> sfAuto then
    FFormatType := AFormat;

  if FFormatType = sfAuto then
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

function TBaseSerializableObject.SaveToJSONString: string;
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

procedure TBaseSerializableObject.SaveToNode(const ANodeName: string; const ASaveParent: Boolean);
var
  lNode: IXMLNode;
  lCreateNewFile: Boolean;
  lIntf: IBaseSerializable;
  lIntf2: ISerializationSupport;
begin
  lNode := nil;
  lCreateNewFile := True;
  if (FParent <> nil) and ASaveParent then
  begin
    // Inheritance test
    if FParent.InheritsFrom(TBaseSerializableObject) then
    begin
      FXMLFile := TBaseSerializableObject(FParent).GetXMLFile;
      FFormatType := TBaseSerializableObject(FParent).FormatType;
      FIncludeClassName := TBaseSerializableObject(FParent).IncludeClassName;
      lNode := TBaseSerializableObject(FParent).GetXMLNode;
    end
    else
      if FParent.InheritsFrom(TBaseSerializableList) then
      begin
        FXMLFile := TBaseSerializableList(FParent).GetXMLFile;
        FFormatType := TBaseSerializableList(FParent).FormatType;
        FIncludeClassName := TBaseSerializableList(FParent).IncludeClassName;
        lNode := TBaseSerializableList(FParent).GetXMLNode;
      end
      else
        // Interface test
        if Supports(FParent, IBaseSerializable, lIntf) then
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

  if FFormatType = sfAuto then
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

  sfAuto:
    raise EBaseSerializationTypeUnknown.CreateFmt(R_SERIALIZE_UNKNOWN_FILE_TYPE_EX, [Self.ClassName]);
  end;

  DoSaveToNode;
end;

function TBaseSerializableObject.SaveToXmlString: string;
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
    CheckPropertyDependency(TBaseSerializableObjectClass(Self.ClassType));

    FXMLNode.Attributes['ParentClass'] := Self.ClassParent.ClassName;

    if Self is TBaseSerializableList then
    begin
      FXMLNode.Attributes['isList'] := 'true';
      FXMLNode.Attributes['itemClass'] := TBaseSerializableList(Self).ItemClassName;
    end;
  end;

  Result := lFile.XML.Text;
  // check for XML Encoding on file text
  if Pos(C_XML_ENCODING, Result) <= 0 then
    Insert(' encoding="' + C_XML_ENCODING + '"', Result, Pos('"1.0"', Result) + 5);
end;

procedure TBaseSerializableObject.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TBaseSerializableObject.SetFormatType(const Value: TSerializationFormat);
begin
  FFormatType := Value;
end;

procedure TBaseSerializableObject.SetIncludeClassName(const AValue: Boolean);
begin
  FIncludeClassName := AValue;
end;

procedure TBaseSerializableObject.SetLastError(const AError: string);
begin
  FLastError := AError;
end;

procedure TBaseSerializableObject.SetNodeName(const Value: string);
begin
  FNodeName := TRpStrings.RemoveAccent( Value );
end;

procedure TBaseSerializableObject.SetParent(const Value: TObject);
begin
  FParent := Value;
end;

procedure TBaseSerializableObject.SetProxyMode(const AValue: Boolean);
begin
  FProxyMode := AValue;
end;

procedure TBaseSerializableObject.ToNode(const AFieldName: string; const AFieldClass: TStrings);
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

procedure TBaseSerializableObject.ToNode(const AFieldName, AValue: string;
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

procedure TBaseSerializableObject.ToNode(const AFieldName : string; const AList: TBaseSerializableList);
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
//        {$IFDEF XE8UP}
          FJSonObject.AddPair(TJSONPair.Create(AList.FNodeName, lItensPair.JsonValue.Clone as TJSONValue));
//        {$ELSE}
//          FJSonObject.AddPair(TJSONPair.Create(AList.FNodeName, lItensPair.JsonValue.Clone as TJSONObject));
//        {$ENDIF}
      end;
    end;
end;

procedure TBaseSerializableObject.ToNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; const ASetValue);
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
    {$IFDEF ANDROID}
      FXMLNode.Attributes[ASetTypeInfo.NameFld.ToString] := 'set of ' + lItensInfo.NameFld.ToString;
    {$ELSE}
      FXMLNode.Attributes[String(ASetTypeInfo.Name)] := 'set of ' + lItensInfo.Name;
    {$ENDIF}
  end
  else
  begin
    try
      lStr := TRpStrings.SetToString(ASetTypeInfo, ASetValue, True);
    except
      lStr := '';
    end;

    if ((lStr <> '') and (lStr <> '[]')) or (FIncludeEmptyFields) then
      ToNode(AFieldName, lStr);
  end;
end;

procedure TBaseSerializableObject.ToNode(const AFieldName : string; const AEnumValue : Cardinal; ASetTypeInfo : PTypeInfo);
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

    {$IFDEF ANDROID}
      FXMLNode.Attributes[ASetTypeInfo.NameFld.ToString] := lValues;
    {$ELSE}
      FXMLNode.Attributes[string(ASetTypeInfo.Name)] := lValues;
    {$ENDIF}
  end
  else
  begin
    try
      lStr := TRpStrings.EnumToStr(ASetTypeInfo, AEnumValue);
    except
      lStr := '';
    end;

    if (lStr <> '') or (FIncludeEmptyFields) then
      ToNode(AFieldName, lStr);
  end;
end;

procedure TBaseSerializableObject.ToNode(const AFieldName : string; const AItem: IBaseSerializable);
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

procedure TBaseSerializableObject.ToNode(const AFieldName: string; const AValue: Variant);
var
  lClear: Boolean;
begin
  if FFormatType = sfAuto then
    FFormatType := C_DEFAULT_FORMAT;

  if FProxyMode then
    InternalSaveNodeProxy(AFieldName, AValue)
  else
  begin
    lClear := (VarIsStr(AValue) and (Trim(AValue) = ''))
           or (VarIsNumeric(AValue) and (AValue = 0))
           or ((FindVarData(AValue)^.VType = varDate) and (AValue <= 2));

    if (not lClear) or (FIncludeEmptyFields) then
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

procedure TBaseSerializableObject.FromNode(const AFieldName: string; ASetTypeInfo: PTypeInfo; out ASetVar);
var
  lStr : string;
begin
  FromNode(AFieldName, lStr);
  TRpStrings.StringToSet(ASetTypeInfo, ASetVar, lStr);
end;

{ TContainerSerializationController }

procedure TContainerSerializationController.DoLoadFromNode(const ANode: IXMLNode);
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

procedure TContainerSerializationController.DoSaveToNode;
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

{ TCustomSerializableList }

function TBaseSerializableList.Add(const ACustomFileObject: TBaseSerializableObject) : Integer;
begin
  InitList;

  Result := FItens.Add(ACustomFileObject);
  ACustomFileObject.Parent := Self;
end;

procedure TBaseSerializableList.Clear;
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

function TBaseSerializableList.Count: Integer;
begin
  InitList;
  Result := FItens.Count;
end;

constructor TBaseSerializableList.Create(AOwner: TObject; const ANodeName: string;
  const AOwnsObjects: Boolean);
begin
  inherited Create(AOwner, ANodeName);
  InitList;
  FItens.OwnsObjects := AOwnsObjects;

  InitializeJson;
end;

function TBaseSerializableList.Delete(const AIndex: Integer): Boolean;
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

destructor TBaseSerializableList.Destroy;
begin
  if FJSonObject <> nil then
    FJSonObject := nil;

  inherited;
end;

procedure TBaseSerializableList.DoLoadFromNode(const ANode: IXMLNode);
begin
// dummy
end;

procedure TBaseSerializableList.DoSaveToNode;
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
    TBaseSerializableObject(FItens[i]).SaveToNode;
    {$ENDIF}

    if FFormatType = sfJSON then
    begin
      lStr := TBaseSerializableObject(Items[i]).FJSonObject.ToString;
      {$IFDEF XE8UP}
      lJsonList.AddElement( TBaseSerializableObject(Items[i]).FJSonObject.Clone as TJSONValue);
      {$ELSE}
      lJsonList.AddElement( TBaseSerializableObject(Items[i]).FJSonObject.Clone as TJSONObject );
      {$ENDIF}
    end;
  end;

  if FFormatType = sfJSON then
  begin
    FJSonObject.AddPair('Itens', TJSONArray(lJsonList.Clone));
    FreeAndNil( lJsonList );
  end;
end;

procedure TBaseSerializableList.Finalize;
begin
  Clear;
  if Assigned(FItens) then
  begin
    FItens.Clear;
    FreeAndNil(FItens);
  end;

  inherited;
end;

procedure TBaseSerializableList.FromOther(const AOther: IBaseSerializable);
var
  lItem: TBaseSerializableObject;
  i: Integer;
begin
  if (AOther <> nil) and (AOther is TBaseSerializableList) and (TBaseSerializableList(AOther).Count > 0) then
  begin
    for i := 0 to TBaseSerializableList(AOther).Count - 1 do
    begin
      lItem := TBaseSerializableList(AOther).GetItemClass.Create(Self);
      lItem.FromOther( TBaseSerializableList(AOther).Items[i] );
      Self.Add(lItem);
    end;
  end;
end;

function TBaseSerializableList.GetItem(AIndex: Integer): TBaseSerializableObject;
begin
  InitList;
  Result := nil;
  if (FItens.Count > 0) and (AIndex >= 0) then
  begin
    {$IFDEF XE3UP}
    Result := FItens[AIndex];
    {$ELSE}
    Result := TBaseSerializableObject(FItens[AIndex]);
    {$ENDIF}
  end;
end;

function TBaseSerializableList.GetItemClassName: string;
begin
  Result := GetItemClass.ClassName;
end;

function TBaseSerializableList.GetOwnsObjects: Boolean;
begin
  InitList;
  Result := FItens.OwnsObjects;
end;

procedure TBaseSerializableList.Initialize;
begin
  inherited;
end;

procedure TBaseSerializableList.InitList;
begin
  if not Assigned(FItens) then
    FItens := TObjectList{$IFDEF XE3UP}<TBaseSerializableObject>{$ENDIF}.Create;
end;

function TBaseSerializableList.Insert(const ACustomFileObject: TBaseSerializableObject;
  const AIndex: Integer) : Integer;
begin
  InitList;
  ACustomFileObject.Parent := Self;
  FItens.Insert(AIndex, ACustomFileObject);
  Result := FItens.IndexOf(ACustomFileObject);
end;

procedure TBaseSerializableList.LoadFromJSONString(const AJSONString: string);
var
  i: Integer;
  lObject: TBaseSerializableObject;
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
        lObject := TBaseSerializableObject.CreateFromJson(lJsonList.Get(i) as TJSONObject, Self);
        Add(lObject);
      end;
    end;
  end;
end;

procedure TBaseSerializableList.LoadFromNode(const ANode: IXMLNode);
var
  I: Integer;
  lObject: TBaseSerializableObject;
begin
  Clear;

  FXMLNode := ANode;
  if FXMLNode <> nil then
  begin
    for I := 0 to FXMLNode.ChildNodes.Count - 1 do
    begin
      lObject := TBaseSerializableObject.CreateFromNode(FXMLNode.ChildNodes[I], Self);
      Add(lObject);
    end;
  end;
end;

procedure TBaseSerializableList.LoadFromXmlString(const AXmlValue: string);
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

function TBaseSerializableList.Remove(const ACustomFileObject: TBaseSerializableObject): Integer;
begin
  InitList;
  Result := FItens.Remove(ACustomFileObject);
end;

procedure TBaseSerializableList.Reset;
begin
  Self.Clear;
end;

procedure TBaseSerializableList.SetOwnsObjects(const Value: Boolean);
begin
  InitList;
  FItens.OwnsObjects := Value;
end;

{ TSerializationClassRegister }

function TSerializationClassRegister.GetClass(const AClassName: string): TBaseSerializableObjectClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].SerializationClassName = AClassName then
    begin
      Result := TBaseSerializableObjectClass(Items[I].SerializationClass);
      Exit;
    end;
end;

function TSerializationClassRegister.GetClass(const AIndex: Integer): TBaseSerializableObjectClass;
begin
  Result := TBaseSerializableObjectClass(Items[AIndex].SerializationClass);
end;

{$IFNDEF ANDROID}
function TSerializationClassRegister.GetItem(const Index: Integer): TSerializationClassRegisterItem;
begin
  Result := TSerializationClassRegisterItem(Self.Get(Index));
end;
{$ENDIF}

procedure TSerializationClassRegister.Registrate(const AClass: TBaseSerializableObjectClass);
var
  lItem : TSerializationClassRegisterItem;
begin
  if GetClass(AClass.ClassName) = nil then
  begin
    lItem := TSerializationClassRegisterItem.Create;
    lItem.SerializationClass := AClass;
    lItem.SerializationClassName := AClass.ClassName;
    Add(lItem);
  end;
end;

{ SerializationClassRegistrer }

class function SerializationClassRegistrer.Count: Integer;
begin
  if TSerializationClassRegister.InternalClassRegister = nil then
    Result := -1
  else
    Result := TSerializationClassRegister.InternalClassRegister.Count;
end;

class procedure SerializationClassRegistrer.FreeRegister;
begin
  if TSerializationClassRegister.InternalClassRegister <> nil then
    FreeAndNil(TSerializationClassRegister.InternalClassRegister);
end;

class function SerializationClassRegistrer.GetClass(const AClassName: string): TBaseSerializableObjectClass;
begin
  if TSerializationClassRegister.InternalClassRegister = nil then
    TSerializationClassRegister.InternalClassRegister := TSerializationClassRegister.Create;

  Result := TSerializationClassRegister.InternalClassRegister.GetClass(AClassName);
end;

class function SerializationClassRegistrer.GetClass(
  const AIndex: Integer): TBaseSerializableObjectClass;
begin
  if TSerializationClassRegister.InternalClassRegister = nil then
    TSerializationClassRegister.InternalClassRegister := TSerializationClassRegister.Create;

  Result := TSerializationClassRegister.InternalClassRegister.GetClass(AIndex);
end;

class procedure SerializationClassRegistrer.Registrate(const AClass: TBaseSerializableObjectClass);
begin
  if TSerializationClassRegister.InternalClassRegister = nil then
    TSerializationClassRegister.InternalClassRegister := TSerializationClassRegister.Create;
  TSerializationClassRegister.InternalClassRegister.Registrate(AClass);
end;

{ TListSerializationController }

function TListSerializationController.ClearList: Boolean;
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

procedure TListSerializationController.DoLoadFromNode(const ANode: IXMLNode);
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

procedure TListSerializationController.DoLoadItem(const ANode: IXMLNode);
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

procedure TListSerializationController.DoSaveToNode;
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

procedure TListSerializationController.LoadListFromJsonObject(const AJsonString : string);
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

  if Pos('ClassName', lTempStr) = 3 then
    System.Delete(lTempStr, 1, Pos(',', lTempStr));

  if Pos('Itens', lTempStr) = 2 then
    lTempStr := '{' + lTempStr
  else if Pos('Itens', lTempStr) <> 3 then
    lTempStr := '{"Itens":' + lTempStr + '}';

  FJSonObject := TJSONObject.Create;
  FJSonObject.Parse(BytesOf(lTempStr), 0);

  if (FJSonObject <> nil) then
  begin
    // list
    lPair := FJSonObject.Get('Itens');
    if lPair <> nil then
    begin
      lJsonList := lPair.JsonValue as TJSONArray;
      for i := 0 to lJsonList.Size - 1 do
      begin
        lTempStr := (lJsonList.Get(i) as TJSONObject).ToString;
        InternalLoad;
      end;
    end;
  end;
end;

{ TSerializationBaseController }

procedure TSerializationBaseController.DoLoadFromNode(const ANode: IXMLNode);
begin
end;

procedure TSerializationBaseController.DoSaveToNode;
begin
end;

procedure TSerializationBaseController.FromOther(const AOther: IBaseSerializable);
begin
end;

procedure TSerializationBaseController.Reset;
begin
end;

procedure TBaseSerializableObject.ToNode(const AFieldName: string; const AItem: IBaseSerializable; const AGuid: TGuid);
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

procedure TBaseSerializableObject.ToNode(const AFieldName: string; const AList: TInterfaceList; const AGuid: TGuid);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
  begin
    if Supports(AList[I], IBaseSerializable) then
      ToNode(AFieldName, AList[I] as IBaseSerializable, AGuid);
  end;
end;

procedure TBaseSerializableObject.CheckPropertyDependency(
  const AClassParent: TBaseSerializableObjectClass);
var
  lParent : TBaseSerializableObject;
  lNode, lNodeParent: IXMLNode;
  i: Integer;
begin
  if (AClassParent <> TBaseSerializableObject) and (AClassParent <> TBaseSerializableList) then
  begin
    lParent := AClassParent.Create(nil);
    try
      lParent.ProxyMode := True;
      CheckPropertyDependency(TBaseSerializableObjectClass(lParent.ClassParent));

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

{ TCustomSerializableObjectEx }

function TCustomSerializableObjectEx.DoDelete(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TCustomSerializableObjectEx.DoExecute(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TCustomSerializableObjectEx.DoLoad(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

function TCustomSerializableObjectEx.DoSave(const ADataComponent: TComponent): Boolean;
begin
  Result := False;
end;

initialization

finalization
  SerializationClassRegistrer.FreeRegister;

end.
