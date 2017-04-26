unit uRpColumnsDefs;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  Xml.XMLIntf,
  {$ELSE}
  XMLIntf,
  {$ENDIF}
  uRpSerialization, Classes, uRpSystem, SysUtils, uRpStringFunctions;

type
  TRpColumnOrder = (coNormal, coAscending, coDescending);
  TRpFieldType = (ftString, ftInteger, ftFloat, ftDate, ftTime, ftDateTime, ftBoolean);
  TRpStringFilter = (sfNormal, sfUpperCase);

  TRpColumnDefsFileController = class(TRpBaseFileController)
  protected
    procedure DoSaveToNode; override;
    procedure DoLoadFromNode(const ANode: IXMLNode); override;
  end;

  TRpColumnDefsListFileController = class(TRpListFileController)
  protected
    function GetNewItem(out AObject: TObject): Boolean; override;
  end;

  TRpColumnDefs = class(TCollectionItem, IInterface, IRpFileSupport)
  private
    FDataField: string;
    FCaption: string;
    FWidth: Cardinal;
    FOrder: TRpColumnOrder;
    FFilterMask: string;
    FColumnType: TRpFieldType;
    FOwner : TPersistent;
//    FRefCount: Integer;
    FFileController: TRpColumnDefsFileController;
    FCanFilter: Boolean;
    FStringFilter: TRpStringFilter;
    procedure SetColumnType(const Value: TRpFieldType);
  protected
    function GetDisplayName : string; override;
  public
    constructor Create(Collection : TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    // IInterface
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; stdcall;
    // IRpXMLSupport
    function GetFileController: IFileBase;
    function GetFileNodeName: string;

    function GetFieldOnlyName: String;
  published
    property DataField : string read FDataField write FDataField;
    property Caption : string read FCaption write FCaption;
    property Width : Cardinal read FWidth write FWidth default 150;
    property Order : TRpColumnOrder read FOrder write FOrder default coNormal;
    property FilterMask : string read FFilterMask write FFilterMask;
    property ColumnType : TRpFieldType read FColumnType write SetColumnType default ftString;
    property CanFilter : Boolean read FCanFilter write FCanFilter default True;
    property StringFilter : TRpStringFilter read FStringFilter write FStringFilter default sfUpperCase;
  end;

  TRpColumnDefsList = class(TCollection) //TXMLCustomList
  private
    FOwner : TPersistent;
    function GetItem(AIndex: Integer): TRpColumnDefs;
    procedure SetItem(AIndex: Integer; const Value: TRpColumnDefs);
    procedure VerifyFileController;
  protected
    FFileController : TRpListFileController;
    function GetOwner : TPersistent; override;
  public
    property Items[AIndex : Integer] : TRpColumnDefs read GetItem write SetItem; default;

    constructor CreateOwner(ItemClass : TCollectionItemClass; Owner : TPersistent);
    destructor Destroy; override;
    function AddColumnDef : TRpColumnDefs;
    procedure Assign(Source : TPersistent); override;

    // xml
    function SaveToXmlString : string;
    function SaveToJSONString : string;
    procedure SaveToFile(const AFile : string; const AFormat : TFileFormat = ffUnknown);
    procedure LoadFromFile(const AFile : string);
    procedure LoadFromXmlString(const AXmlString : string);
    procedure LoadFromJSONString(const AJSONString : string);
  end;

implementation

{ TRpColumnsDefList }

function TRpColumnDefsList.AddColumnDef: TRpColumnDefs;
begin
  Result := TRpColumnDefs.Create(Self);
  Self.Add;
  Self[ Self.Count - 1 ].Assign( Result );

  FreeAndNil(Result);

  Result := Self[ Self.Count - 1];
end;

procedure TRpColumnDefsList.Assign(Source: TPersistent);
var
  i: Integer;
begin
  if Source = nil then
    Exit;

  if not (Source is TRpColumnDefsList) then
  begin
    inherited;
    Exit;
  end;

  Self.Clear;
  for i := 0 to TRpColumnDefsList(Source).Count - 1 do
  begin
    Self.Add;
    Self[i].Assign(TRpColumnDefsList(Source).Items[i]);
  end;
end;

constructor TRpColumnDefsList.CreateOwner(ItemClass: TCollectionItemClass;
  Owner: TPersistent);
begin
  FOwner := Owner;
  VerifyFileController;
  inherited Create(ItemClass);
end;

destructor TRpColumnDefsList.Destroy;
begin
  FreeAndNil( FFileController );
  inherited;
end;

function TRpColumnDefsList.GetItem(AIndex: Integer): TRpColumnDefs;
begin
  Result := TRpColumnDefs(inherited Items[AIndex]);
end;

function TRpColumnDefsList.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TRpColumnDefsList.LoadFromFile(const AFile: string);
begin
  VerifyFileController;
  FFileController.LoadFromFile( AFile );
end;

procedure TRpColumnDefsList.LoadFromJSONString(const AJSONString: string);
begin
  VerifyFileController;
  FFileController.LoadFromString( AJSONString, ffJSON);
end;

procedure TRpColumnDefsList.LoadFromXmlString(const AXmlString: string);
begin
  VerifyFileController;
  FFileController.LoadFromString( AXmlString, ffXML);
end;

procedure TRpColumnDefsList.SaveToFile(const AFile: string; const AFormat : TFileFormat = ffUnknown);
begin
  VerifyFileController;
  FFileController.SaveToFile(AFile, AFormat);
end;

function TRpColumnDefsList.SaveToJSONString: string;
begin
  VerifyFileController;

  Result := FFileController.SaveToJSONString
end;

function TRpColumnDefsList.SaveToXmlString: string;
begin
  VerifyFileController;

  Result := FFileController.SaveToXmlString
end;

procedure TRpColumnDefsList.SetItem(AIndex: Integer; const Value: TRpColumnDefs);
begin
  inherited Items[AIndex] := Value;
end;

procedure TRpColumnDefsList.VerifyFileController;
begin
  if not Assigned(FFileController) then
    FFileController := TRpColumnDefsListFileController.Create(Self, Self.ClassName);
end;

{ TRpColumnsDef }

procedure TRpColumnDefs.Assign(Source: TPersistent);
var
  lItem : TRpColumnDefs absolute Source;
begin
  if Source = nil then
    Exit;

  if Source is TRpColumnDefs then
  begin
    FDataField := lItem.DataField;
    FCaption := lItem.Caption;
    FWidth := lItem.Width;
    FOrder := lItem.Order;
    FFilterMask := lItem.FilterMask;
    FColumnType := lItem.ColumnType;
    FCanFilter := lItem.CanFilter;
    FStringFilter := lItem.StringFilter;
  end
  else
    inherited;
end;

constructor TRpColumnDefs.Create(Collection: TCollection);
begin
  if Assigned(Collection) then
    FOwner := Collection.Owner
  else
    FOwner := nil;

  inherited;
  FFileController := TRpColumnDefsFileController.Create(Self, Self.ClassName);
  FOrder := coNormal;
  FColumnType := ftString;
  FWidth := 150;
  FCanFilter := True;
  FStringFilter := sfUpperCase;
  FFilterMask := '%?%';
end;

destructor TRpColumnDefs.Destroy;
begin
  FreeAndNil( FFileController );
  inherited;
end;

function TRpColumnDefs.GetDisplayName: string;
begin
// ID (code) - ftInteger - coNormal
  Result := Format('%s (%s) - %s - %s', [
    DataField,
    Caption,
    TRpStrings.EnumToStr(TypeInfo(TRpFieldType), Ord(FColumnType)),
    TRpStrings.EnumToStr(TypeInfo(TRpColumnOrder), Ord(FOrder)) ]);

//
//  if Order = coAscending then
//    Result := Result + ' /\'
//  else
//    if Order = coDescending then
//      Result := Result + ' \/';
end;

function TRpColumnDefs.GetFieldOnlyName: String;
begin
  // remove the field alias
  Result := Copy(FDataField, Pos('.', FDataField) + 1, Length(FDataField));
  // keep just the first field "p.ID;b.BarCode" --> "ID"
  if Pos(';', Result) > 0 then
    Result := Copy(Result, 1, Pos(';', Result) - 1);
end;

function TRpColumnDefs.GetFileController: IFileBase;
begin
  Result := FFileController;
end;

function TRpColumnDefs.GetFileNodeName: string;
begin
  Result := Self.ClassName;
end;

function TRpColumnDefs.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TRpColumnDefs.SetColumnType(const Value: TRpFieldType);
begin
  if FColumnType <> Value then
  begin
    FColumnType := Value;
    case FColumnType of
      ftString:   FFilterMask := '%?%';
      ftInteger:  FFilterMask := '?';
      ftFloat:    FFilterMask := '?';
      ftDate:     FFilterMask := '?';
      ftTime:     FFilterMask := '?';
      ftDateTime: FFilterMask := '?';
      ftBoolean:  FFilterMask := '?';
    end;
  end;
end;

function TRpColumnDefs._AddRef: Integer;
begin
//  Result := InterlockedIncrement(FRefCount);
  Result := -1;
end;

function TRpColumnDefs._Release: Integer;
begin
//  Result := InterlockedDecrement(FRefCount);
//  if FRefCount = 0 then
//    Destroy;
  Result := -1;
end;

//initialization
//  XMLClassRegistrer.Registrar(TRpColumnDefs);

{ TRpColumnDefsXMLController }

procedure TRpColumnDefsFileController.DoLoadFromNode(const ANode: IXMLNode);
var
  lAux: Integer;
begin
  inherited;
  with TRpColumnDefs(Parent) do
  begin
    FromNode('DataField', FDataField);
    FromNode('Caption', FCaption);
    FromNode('FilterMask', FFilterMask);
    FromNode('CanFilter', FCanFilter);

    FromNode('Width', lAux);
    FWidth := lAux;

    FromNode('Order', lAux);
    FOrder := TRpColumnOrder(lAux);

    FromNode('ColumnType', lAux);
    FColumnType := TRpFieldType(lAux);

    FromNode('StringFilter', lAux);
    FStringFilter := TRpStringFilter(lAux);
  end;
end;

procedure TRpColumnDefsFileController.DoSaveToNode;
begin
  inherited;
  with TRpColumnDefs(Parent) do
  begin
    ToNode('DataField', FDataField);
    ToNode('Caption', FCaption);
    ToNode('Width', FWidth);
    ToNode('Order', Ord(FOrder));
    ToNode('FilterMask', FFilterMask);
    ToNode('ColumnType', Ord(FColumnType));
    ToNode('CanFilter', FCanFilter);
    ToNode('StringFilter', Ord(FStringFilter));
  end;
end;

{ TRpColumnDefsListXMLController }

function TRpColumnDefsListFileController.GetNewItem(out AObject: TObject): Boolean;
begin
  AObject := TRpColumnDefsList(Parent).AddColumnDef;
  Result := True;
end;

end.
