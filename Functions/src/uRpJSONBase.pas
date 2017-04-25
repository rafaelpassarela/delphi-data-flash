unit uRpJSONBase;

{$IFDEF ANDROID}
  {$I C:\Componentes\RafaPack\Common\src\RpInc.inc}
{$ELSE}
  {$I ..\Common\src\RpInc.inc}
{$ENDIF}

interface

uses
  {$IFDEF XE3UP}
    {$IFNDEF ANDROID}
      Winapi.Windows,
    {$ENDIF}
  {$ELSE}
  Windows,
  {$ENDIF}
  uRpJSONPlatform, SysUtils;

type
  UnicodeString = string;

  TJSONValue = class;
  TJSONString = class;
  TJSONObject = class;

  TRpJSONCallback = class abstract
  public
    function Execute(const Arg: TJSONValue): TJSONValue; overload; virtual; abstract;
    function Execute(Arg: TObject): TObject; overload; virtual; abstract;
    function AddRef: Integer; virtual;
    function Release: Integer; virtual;
  protected
    procedure SetConnectionHandler(const ConnectionHandler: TObject); virtual;
    procedure SetDsServer(const DsServer: TObject); virtual;
    procedure SetOrdinal(const Ordinal: Integer); virtual;

    function IsConnectionLost: Boolean; virtual;
  private
    FFRefCount: Integer;
  public
    property ConnectionHandler: TObject write SetConnectionHandler;
    property DsServer: TObject write SetDsServer;
    property Ordinal: Integer write SetOrdinal;
    property ConnectionLost: Boolean read IsConnectionLost;
  public
    const ArgJson = 1;
    const ArgObject = 2;
  end;

  TRpJSONCallbackDelegate = class(TRpJSONCallback)
  public
    destructor Destroy; override;
    function Execute(const Arg: TJSONValue): TJSONValue; overload; override;
    function Execute(Arg: TObject): TObject; overload; override;
  protected
    procedure SetDelegate(const Callback: TRpJSONCallback); virtual;
    function GetDelegate: TRpJSONCallback; virtual;
    procedure SetConnectionHandler(const ConnectionHandler: TObject); override;
    procedure SetOrdinal(const Ordinal: Integer); override;
    procedure SetDsServer(const DsServer: TObject); override;
    function IsConnectionLost: Boolean; override;
  private
    FDelegate: TRpJSONCallback;
    FConnectionHandler: TObject;
    FDsServer: TObject;
    FOrdinal: Integer;
  public
    property Delegate: TRpJSONCallback read GetDelegate write SetDelegate;
  end;

  TRpJSONNamedCallback = class abstract(TRpJSONCallback)
  public
    constructor Create(const Name: UnicodeString);
  protected
    function GetName: UnicodeString; virtual;
  protected
    FName: UnicodeString;
  public
    property Name: UnicodeString read GetName;
  end;

  TJSONAncestor = class abstract (TInterfacedObject)
  public
    constructor Create;
    function Value: UnicodeString; virtual;
    function EstimatedByteSize: Integer; virtual; abstract;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; virtual; abstract;
    function Clone: TJSONAncestor; virtual; abstract;
    function GetOwned: Boolean; virtual;
  protected
    function IsNull: Boolean; virtual;
    procedure AddDescendant(const Descendent: TJSONAncestor); virtual; abstract;
    procedure SetOwned(const Own: Boolean); virtual;
  private
    FOwned: Boolean;
  public
    property Null: Boolean read IsNull;
    property Owned: Boolean write SetOwned;
    {$IFDEF UNICODE}
    function ToString: String; override;
    {$ELSE}
    function ToString : UnicodeString; virtual;
    {$ENDIF}
  end;

  TJSONByteReader = class
  public
    constructor Create(const Data: TBytes; const Offset: Integer; const Range: Integer); overload;
    constructor Create(const Data: TBytes; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean); overload;
    function ConsumeByte: Byte; virtual;
    function PeekByte: Byte; virtual;
    function Empty: Boolean; virtual;
    function HasMore(const Size: Integer): Boolean; virtual;
  protected
    function GetOffset: Integer; virtual;
  private
    procedure ConsumeBOM;
    procedure MoveOffset;
  private
    FData: TBytes;
    FOffset: Integer;
    FRange: Integer;
    FIsUTF8: Boolean;
    FUtf8data: TBytes;
    FUtf8offset: Integer;
    FUtf8length: Integer;
  public
    property Offset: Integer read GetOffset;
  end;

  TJSONException = class(Exception)
  public
    constructor Create(const ErrorMessage: UnicodeString);
  private
    const FSerialVersionUID = 1964987864664789863;
  end;

  TJSONPair = class sealed(TJSONAncestor)
  public
    constructor Create; overload;
    constructor Create(const Str: TJSONString; const Value: TJSONValue); overload;
    constructor Create(const Str: UnicodeString; const Value: TJSONValue); overload;
    constructor Create(const Str: UnicodeString; const Value: UnicodeString); overload;
    destructor Destroy; override;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    procedure SetJsonString(const Descendant: TJSONString);
    procedure SetJsonValue(const Val: TJSONValue);
    function GetJsonString: TJSONString;

    function GetJsonValue: TJSONValue;
  private
    FJsonString: TJSONString;
    FJsonValue: TJSONValue;
    function GetFieldName: string;
    function GetFieldValue: string;
    procedure SetFieldValue(const Value: string);
  public
    property JsonString: TJSONString read GetJsonString write SetJsonString;
    property JsonValue: TJSONValue read GetJsonValue write SetJsonValue;

    property FieldName : string read GetFieldName;
    property FieldValue : string read GetFieldValue write SetFieldValue;
  end;

  TJSONValue = class abstract(TJSONAncestor)
  end;

  TJSONTrue = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  end;

  TJSONString = class(TJSONValue)
  public
    class function Hex(const Digit: Integer): Byte; static;
    constructor Create; overload;
    constructor Create(const Value: UnicodeString); overload;
    destructor Destroy; override;
    procedure AddChar(const Ch: WideChar); virtual;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function IsNull: Boolean; override;
  protected
    FStrBuffer: TRpJSONStringBuffer;
  end;

  TJSONStrinNoQuote = class(TJSONString)
  public
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  end;

  TJSONNumber = class sealed(TJSONString)
  public
    constructor Create; overload;
    constructor Create(const Value: Double); overload;
    constructor Create(const Value: Integer); overload;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Value: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    constructor Create(const Value: UnicodeString); overload;
    function GetAsDouble: Double;
    function GetAsInt: Integer;
  public
    property AsDouble: Double read GetAsDouble;
    property AsInt: Integer read GetAsInt;
  end;

  IJSONObject = interface
  ['{1A93C680-53A3-4495-9EF4-0C30B62A4127}']
    function AddPair(const Pair: TJSONPair): TJSONObject; overload;
    function AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: UnicodeString): TJSONObject; overload;
    function Parse(const Data: TBytes; const Pos: Integer): Integer; overload;
    function Parse(const Data: TBytes; const Pos: Integer; const Count: Integer): Integer; overload;
    function Get(const I: Integer): TJSONPair; overload;
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    function ToString: UnicodeString;
    function Clone: TJSONAncestor;
    function GetObject : TJSONObject;
  end;

  TJSONObject = class sealed(TJSONValue, IJSONObject)
  public
    class function HexToDecimal(const Value: Byte): Integer; static;
    class function ParseJSONValue(const Data: TBytes; const Offset: Integer; IsUTF8: Boolean = True): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: TBytes; const Offset: Integer; const Count: Integer; IsUTF8: Boolean = True): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: String): TJSONValue; overload; static;
    class function ParseJSONValue(const Data: UTF8String): TJSONValue; overload; static;
    class function ParseJSONValueUTF8(const Data: TBytes; const Offset: Integer; const Count: Integer): TJSONValue; overload; static;
    class function ParseJSONValueUTF8(const Data: TBytes; const Offset: Integer): TJSONValue; overload; static;
    constructor Create; overload;
    constructor Create(const Pair: TJSONPair); overload;
    function Size: Integer;
    function Get(const I: Integer): TJSONPair; overload;
    function Get(const PairName: UnicodeString): TJSONPair; overload;
    destructor Destroy; override;
    function AddPair(const Pair: TJSONPair): TJSONObject; overload;
    function AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: TJSONValue): TJSONObject; overload;
    function AddPair(const Str: UnicodeString; const Val: UnicodeString): TJSONObject; overload;
    function RemovePair(const PairName: String): TJSONPair;
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Idx: Integer): Integer; override;
    function Clone: TJSONAncestor; override;
    function Parse(const Data: TBytes; const Pos: Integer): Integer; overload;
    function Parse(const Data: TBytes; const Pos: Integer; const Count: Integer): Integer; overload;
    function GetObject : TJSONObject;

    procedure SetMemberList(AList: TRpJSONArrayList);

    function ToString: UnicodeString; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  private
    function Parse(const Br: TJSONByteReader): Integer; overload;
    class procedure ConsumeWhitespaces(const Br: TJSONByteReader); static;
    class function ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject): Integer; static;
    class function ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
    class function ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer; static;
  private
    FMembers: TRpJSONArrayList;
  end;

  TJSONNull = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function IsNull: Boolean; override;
  end;

  TJSONFalse = class sealed(TJSONValue)
  public
    function EstimatedByteSize: Integer; override;
    function ToBytes(const Data: TBytes; const Offset: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
  end;

  TJSONArray = class sealed(TJSONValue)
  public
    constructor Create; overload;
    constructor Create(const FirstElem: TJSONValue); overload;
    constructor Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue); overload;
    constructor Create(const FirstElem: String; const SecondElem: String); overload;
    destructor Destroy; override;
    function Size: Integer;
    function Get(const Index: Integer): TJSONValue;
    procedure AddElement(const Element: TJSONValue);
    function Add(const Element: UnicodeString): TJSONArray; overload;
    function Add(const Element: Integer): TJSONArray; overload;
    function Add(const Element: Double): TJSONArray; overload;
    function Add(const Element: Boolean): TJSONArray; overload;
    function Add(const Element: TJSONObject): TJSONArray; overload;
    function Add(const Element: TJSONArray): TJSONArray; overload;
    function EstimatedByteSize: Integer; override;
    procedure SetElements(AList: TRpJSONArrayList);
    function ToBytes(const Data: TBytes; const Pos: Integer): Integer; override;
    function ToString: UnicodeString; override;
    function Clone: TJSONAncestor; override;
  protected
    procedure AddDescendant(const Descendant: TJSONAncestor); override;
    function Pop: TJSONValue;
  private
    FElements: TRpJSONArrayList;
  end;

//function BytesOf(const Val: RawByteString): TBytes; overload;
function StringToBytes(const Value : string) : TBytes;
function BytesToString(const Value : TBytes) : string;
{$IFNDEF UNICODE}
function BytesOf(const Val: UnicodeString): TBytes; overload;
function BytesOf(const Val: WideChar): TBytes; overload;
function BytesOf(const Val: AnsiChar): TBytes; overload;
{$ENDIF}
function StringOf(const Bytes: TBytes): UnicodeString;

implementation

uses
  uRpJSONCommonResStrs;

//function BytesOf(const Val: RawByteString): TBytes;
//var
//  Len: Integer;
//begin
//  Len := Length(Val);
//  SetLength(Result, Len);
//  Move(Val[1], Result[0], Len);
//end;

function StringToBytes(const Value : string) : TBytes;
var
  I: integer;
  lAux: string;
begin
  lAux := StringReplace(Value, #13, '', [rfReplaceAll]);
  lAux := StringReplace(lAux, #10, '', [rfReplaceAll]);
  lAux := StringReplace(lAux, #9, '', [rfReplaceAll]);

  SetLength(Result, Length(lAux));
  for I := 0 to Length(lAux) - 1 do
  begin
    Result[I] := Ord(lAux[I + 1]);
//    Result[I] := Result[I] - 48;
  end;
end;

function BytesToString(const Value : TBytes) : string;
var
  I: integer;
  S : String;
  Letra: char;
begin
  S := '';
  for I := Length(Value)-1 Downto 0 do
  begin
    letra := Chr(Value[I] {+ 48});
    S := letra + S;
  end;
  Result := S;
end;

{$IFNDEF UNICODE}
function BytesOf(const Val: UnicodeString): TBytes;
begin
  Result := StringToBytes(Val);
end;

function BytesOf(const Val: WideChar): TBytes;
begin
  Result := BytesOf(UnicodeString(Val));
end;

function BytesOf(const Val: AnsiChar): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := Byte(Val);
end;
{$ENDIF}

function StringOf(const Bytes: TBytes): UnicodeString;
begin
  if Assigned(Bytes) then
    Result := BytesToString(Bytes)
  else
    Result := '';
end;

procedure TRpJSONCallback.SetConnectionHandler(const ConnectionHandler: TObject);
begin
end;

procedure TRpJSONCallback.SetDsServer(const DsServer: TObject);
begin
end;

procedure TRpJSONCallback.SetOrdinal(const Ordinal: Integer);
begin
end;

function TRpJSONCallback.AddRef: Integer;
begin
  Result := Incr(FFRefCount);
end;

function TRpJSONCallback.IsConnectionLost: Boolean;
begin
  Result := False;
end;

function TRpJSONCallback.Release: Integer;
var
  Count: Integer;
begin
  Count := Decr(FFRefCount);
  if Count <= 0 then
    self.Free;
  Result := Count;
end;

destructor TRpJSONCallbackDelegate.Destroy;
begin
  FreeAndNil(FDelegate);
  inherited Destroy;
end;

function TRpJSONCallbackDelegate.Execute(const Arg: TJSONValue): TJSONValue;
begin
  Result := FDelegate.Execute(Arg);
end;

function TRpJSONCallbackDelegate.Execute(Arg: TObject): TObject;
begin
  Result := FDelegate.Execute(Arg);
end;

procedure TRpJSONCallbackDelegate.SetDelegate(const Callback: TRpJSONCallback);
begin
  FDelegate := Callback;
  if FDelegate <> nil then
  begin
    FDelegate.Ordinal := FOrdinal;
    FDelegate.ConnectionHandler := FConnectionHandler;
    FDelegate.DsServer := FDsServer;
  end;
end;

function TRpJSONCallbackDelegate.GetDelegate: TRpJSONCallback;
begin
  Result := FDelegate;
end;

function TRpJSONCallbackDelegate.IsConnectionLost: Boolean;
begin
  if Assigned(FDelegate) then
  begin
    Result := FDelegate.ConnectionLost;
    if Result then
      exit;
  end;

  Result := False;
end;

procedure TRpJSONCallbackDelegate.SetConnectionHandler(const ConnectionHandler: TObject);
begin
  self.FConnectionHandler := ConnectionHandler;
  if FDelegate <> nil then
    FDelegate.ConnectionHandler := ConnectionHandler;
end;

procedure TRpJSONCallbackDelegate.SetOrdinal(const Ordinal: Integer);
begin
  self.FOrdinal := Ordinal;
  if FDelegate <> nil then
    FDelegate.Ordinal := Ordinal;
end;

procedure TRpJSONCallbackDelegate.SetDsServer(const DsServer: TObject);
begin
  self.FDsServer := DsServer;
  if FDelegate <> nil then
    FDelegate.DsServer := DsServer;
end;

constructor TRpJSONNamedCallback.Create(const Name: UnicodeString);
begin
  inherited Create;
  self.FName := Name;
end;

function TRpJSONNamedCallback.GetName: UnicodeString;
begin
  Result := FName;
end;

constructor TJSONAncestor.Create;
begin
  inherited Create;
  FOwned := True;
end;

function TJSONAncestor.IsNull: Boolean;
begin
  Result := False;
end;

function TJSONAncestor.Value: UnicodeString;
begin
  Result := NullString;
end;

procedure TJSONAncestor.SetOwned(const Own: Boolean);
begin
  FOwned := Own;
end;

function TJSONAncestor.ToString: UnicodeString;
begin
  Result := Self.ClassName;
end;

function TJSONAncestor.GetOwned: Boolean;
begin
  Result := FOwned;
end;

constructor TJSONByteReader.Create(const Data: TBytes; const Offset: Integer; const Range: Integer);
begin
  inherited Create;
  self.FData := Data;
  self.FOffset := Offset;
  self.FRange := Range;
  ConsumeBOM;
end;

constructor TJSONByteReader.Create(const Data: TBytes; const Offset: Integer; const Range: Integer; const IsUTF8: Boolean);
begin
  inherited Create;
  self.FData := Data;
  self.FOffset := Offset;
  self.FRange := Range;
  self.FIsUTF8 := IsUTF8;
  if IsUTF8 then
    ConsumeBOM;
end;

procedure TJSONByteReader.ConsumeBOM;
begin
  if FOffset + 3 < FRange then
  begin
    if (FData[FOffset] = Byte(239)) and (FData[FOffset + 1] = Byte(187)) and (FData[FOffset + 2] = Byte(191)) then
    begin
      FIsUTF8 := True;
      FOffset := FOffset + 3;
    end;
  end;
end;

procedure TJSONByteReader.MoveOffset;
begin
  if FUtf8offset < FUtf8length then
    IncrAfter(FUtf8offset)
  else
    IncrAfter(FOffset);
end;

function TJSONByteReader.ConsumeByte: Byte;
var
  Data: Byte;
begin
  Data := PeekByte;
  MoveOffset;
  Result := Data;
end;

function TJSONByteReader.PeekByte: Byte;
var
  Bmp: Int64;
  W1: Integer;
  W2: Integer;

  procedure InternalExit(AValor : Byte);
  begin
    Result := AValor;
    Exit;
  end;

begin
//  if not FIsUTF8 then
//    InternalExit(FData[FOffset]);
  if FUtf8offset < FUtf8length then
    InternalExit(FUtf8data[FUtf8offset]);

//  OutputDebugString(PChar('JSON OFFSET: ' + IntToStr(FOffset)));

  if (FData[FOffset] in [176, 192..255]) then
  begin
    Result := FData[FOffset];
//    Inc(FOffset);
  end
  else if (FData[FOffset] and (Byte(128))) <> 0 then
  begin
    FUtf8offset := 0;
    if (FData[FOffset] and (Byte(224))) = Byte(192) then
    begin
      if FOffset + 1 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TRpJSONInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(2),TRpJSONInt32Object.Create(FOffset + 1)]));
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(0);
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset] and Byte(28)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 1] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 1] and Byte(15));
      FOffset := FOffset + 2;
    end
    else if (FData[FOffset] and (Byte(240))) = Byte(224) then
    begin
      if FOffset + 2 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TRpJSONInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(3),TRpJSONInt32Object.Create(FOffset + 1)]));
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(3),TRpJSONInt32Object.Create(FOffset + 2)]));
      SetLength(FUtf8data,6);
      FUtf8length := 6;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex(FData[FOffset] and Byte(15));
      FUtf8data[3] := TJSONString.Hex((Byte((FData[FOffset + 1] and Byte(60)))) shr 2);
      FUtf8data[4] := TJSONString.Hex((Byte((Byte(FData[FOffset + 1]) and Byte(3))) shl 2) or (Byte((Byte((FData[FOffset + 2] and Byte(48))) shr 4))));
      FUtf8data[5] := TJSONString.Hex(FData[FOffset + 2] and Byte(15));
      FOffset := FOffset + 3;
    end
    else if (FData[FOffset] and (Byte(248))) = Byte(240) then
    begin
      if FOffset + 3 >= FRange then
        raise TJSONException.Create(Format(SUTF8Start, [TRpJSONInt32Object.Create(FOffset)]));
      if (FData[FOffset + 1] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(4),TRpJSONInt32Object.Create(FOffset + 1)]));
      if (FData[FOffset + 2] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(4),TRpJSONInt32Object.Create(FOffset + 2)]));
      if (FData[FOffset + 3] and (Byte(192))) <> Byte(128) then
        raise TJSONException.Create(Format(SUTF8UnexpectedByte, [TRpJSONInt32Object.Create(4),TRpJSONInt32Object.Create(FOffset + 3)]));
      Bmp := FData[FOffset] and Byte(7);
      Bmp := (Bmp shl 6) or (FData[FOffset + 1] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 2] and Byte(63));
      Bmp := (Bmp shl 6) or (FData[FOffset + 3] and Byte(63));
      Bmp := Bmp - 65536;
      W1 := 55296;
      W1 := W1 or ((Integer((Bmp shr 10))) and 2047);
      W2 := 56320;
      W2 := W2 or Integer((Bmp and 2047));
      SetLength(FUtf8data,12);
      FUtf8length := 12;
      FUtf8data[0] := Ord('\');
      FUtf8data[1] := Ord('u');
      FUtf8data[2] := TJSONString.Hex((W1 and 61440) shr 12);
      FUtf8data[3] := TJSONString.Hex((W1 and 3840) shr 8);
      FUtf8data[4] := TJSONString.Hex((W1 and 240) shr 4);
      FUtf8data[5] := TJSONString.Hex(W1 and 15);
      FUtf8data[6] := Ord('\');
      FUtf8data[7] := Ord('u');
      FUtf8data[8] := TJSONString.Hex((W2 and 61440) shr 12);
      FUtf8data[9] := TJSONString.Hex((W2 and 3840) shr 8);
      FUtf8data[10] := TJSONString.Hex((W2 and 240) shr 4);
      FUtf8data[11] := TJSONString.Hex(W2 and 15);
      FOffset := FOffset + 4;
    end
    else
    begin
//      raise TJSONException.Create(Format(SUTF8InvalidHeaderByte, [IntToStr(FOffset) ]));
      if FOffset <= Length( FData ) then
        Result := FData[FOffset]
      else
        Result := FData[Length(FData)-1]; 
      Exit;
    end;

    Result := FUtf8data[FUtf8offset];
  end
  else
    Result := FData[FOffset];
end;

function TJSONByteReader.Empty: Boolean;
begin
  Result := (FOffset >= FRange) and (FUtf8offset >= FUtf8length);
end;

function TJSONByteReader.GetOffset: Integer;
begin
  Result := FOffset;
end;

function TJSONByteReader.HasMore(const Size: Integer): Boolean;
begin
  if FOffset + Size < FRange then
    Result := True
  else if FUtf8offset + Size < FUtf8length then
    Result := True
  else
    Result := False;
end;

constructor TJSONException.Create(const ErrorMessage: UnicodeString);
begin
  inherited Create(ErrorMessage);
end;

constructor TJSONPair.Create;
begin
  inherited Create;
end;

constructor TJSONPair.Create(const Str: TJSONString; const Value: TJSONValue);
begin
  inherited Create;
  FJsonString := Str;
  FJsonValue := Value;
end;

constructor TJSONPair.Create(const Str: UnicodeString; const Value: TJSONValue);
begin
  Create(TJSONString.Create(Str), Value);
end;

constructor TJSONPair.Create(const Str: UnicodeString; const Value: UnicodeString);
begin
  Create(TJSONString.Create(Str), TJSONString.Create(Value));
end;

destructor TJSONPair.Destroy;
begin
  if FJsonString <> nil then
    FreeAndNil(FJsonString);
  if (FJsonValue <> nil) and FJsonValue.GetOwned then
    FreeAndNil(FJsonValue);
  inherited Destroy;
end;

procedure TJSONPair.AddDescendant(const Descendant: TJSONAncestor);
begin
  if FJsonString = nil then
    FJsonString := TJSONString(Descendant)
  else
    FJsonValue := TJSONValue(Descendant);
end;

procedure TJSONPair.SetFieldValue(const Value: string);
begin
  if FJsonValue <> nil then
    FreeAndNil(FJsonValue);

  FJsonValue := TJSONString.Create(Value);
end;

procedure TJSONPair.SetJsonString(const Descendant: TJSONString);
begin
  if Descendant <> nil then
    FJsonString := Descendant;
end;

procedure TJSONPair.SetJsonValue(const Val: TJSONValue);
begin
  if Val <> nil then
    FJsonValue := Val;
end;

function TJSONPair.EstimatedByteSize: Integer;
begin
  Result := 1 + FJsonString.EstimatedByteSize + FJsonValue.EstimatedByteSize;
end;

function TJSONPair.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Idx := FJsonString.ToBytes(Data, Offset);
  Data[IncrAfter(Idx)] := Ord(':');
  Result := FJsonValue.ToBytes(Data, Idx);
end;

function TJSONPair.GetFieldName: string;
begin
  if FJsonString <> nil then
    Result := FJsonString.ToString
  else
    Result := NullString;
end;

function TJSONPair.GetFieldValue: string;
begin
  if FJsonValue <> nil then
    Result := FJsonValue.ToString
  else
    Result := NullString;
end;

function TJSONPair.GetJsonString: TJSONString;
begin
  Result := FJsonString;
end;

function TJSONPair.GetJsonValue: TJSONValue;
begin
  Result := FJsonValue;
end;

function TJSONPair.ToString: UnicodeString;
begin
  if (FJsonString <> nil) and (FJsonValue <> nil) then
    Result := FJsonString.ToString + ':' + FJsonValue.ToString
  else
    Result := NullString;
end;

function TJSONPair.Clone: TJSONAncestor;
begin
  Result := TJSONPair.Create(TJSONString(FJsonString.Clone), TJSONValue(FJsonValue.Clone));
end;

procedure TJSONTrue.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONTrue.EstimatedByteSize: Integer;
begin
  Result := 4;
end;

function TJSONTrue.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('t');
  Data[IncrAfter(Idx)] := Ord('r');
  Data[IncrAfter(Idx)] := Ord('u');
  Data[IncrAfter(Idx)] := Ord('e');
  Result := Idx;
end;

function TJSONTrue.ToString: UnicodeString;
begin
  Result := 'true';
end;

function TJSONTrue.Clone: TJSONAncestor;
begin
  Result := TJSONTrue.Create;
end;

class function TJSONString.Hex(const Digit: Integer): Byte;
var
  HexData: TBytes;
begin
  SetLength(HexData,16);
  HexData[0] := Ord('0');
  HexData[1] := Ord('1');
  HexData[2] := Ord('2');
  HexData[3] := Ord('3');
  HexData[4] := Ord('4');
  HexData[5] := Ord('5');
  HexData[6] := Ord('6');
  HexData[7] := Ord('7');
  HexData[8] := Ord('8');
  HexData[9] := Ord('9');
  HexData[10] := Ord('A');
  HexData[11] := Ord('B');
  HexData[12] := Ord('C');
  HexData[13] := Ord('D');
  HexData[14] := Ord('E');
  HexData[15] := Ord('F');
  Result := HexData[Digit];
end;

constructor TJSONString.Create;
begin
  inherited Create;
end;

constructor TJSONString.Create(const Value: UnicodeString);
begin
  inherited Create;
  FStrBuffer := TRpJSONStringBuffer.Create(Value);
end;

destructor TJSONString.Destroy;
begin
  FreeAndNil(FStrBuffer);
  inherited Destroy;
end;

procedure TJSONString.AddChar(const Ch: WideChar);
begin
  FStrBuffer.Append(Ch);
end;

procedure TJSONString.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONString.IsNull: Boolean;
begin
  Result := (FStrBuffer = nil);
end;

function TJSONString.EstimatedByteSize: Integer;
begin
  if Null then
    Result := 4
  else
    Result := 2 + 6 * FStrBuffer.Length;
end;

function TJSONString.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
  UnicodeValue: Integer;
begin
  Offset := Idx;
  if Null then
  begin
    Data[IncrAfter(Offset)] := Ord('n');
    Data[IncrAfter(Offset)] := Ord('u');
    Data[IncrAfter(Offset)] := Ord('l');
    Data[IncrAfter(Offset)] := Ord('l');
  end
  else
  begin
    Data[IncrAfter(Offset)] := Ord('"');
    Index := 0;
    Count := FStrBuffer.Length;
    while Index < Count do
    begin
      CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
      case CurrentChar of
        '"':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('"');
          end;
        '\':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('\');
          end;
        '/':
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('/');
          end;
        #$8:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('b');
          end;
        #$c:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('f');
          end;
        #$a:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('n');
          end;
        #$d:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('r');
          end;
        #$9:
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('t');
          end;
        else
          if (CurrentChar < WideChar(32)) or (CurrentChar > WideChar(127)) then
          begin
            Data[IncrAfter(Offset)] := Ord('\');
            Data[IncrAfter(Offset)] := Ord('u');
            UnicodeValue := Ord(CurrentChar);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 61440) shr 12);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 3840) shr 8);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 240) shr 4);
            Data[IncrAfter(Offset)] := Hex((UnicodeValue and 15));
          end
          else
            Data[IncrAfter(Offset)] := Ord(CurrentChar);
      end;
    end;
    Data[IncrAfter(Offset)] := Ord('"');
  end;
  Result := Offset;
end;

function TJSONString.ToString: UnicodeString;
begin
  if FStrBuffer <> nil then
    Result := '"' + FStrBuffer.ToString + '"'
  else
    Result := NullString;
end;

function TJSONString.Value: UnicodeString;
begin
  if FStrBuffer = nil then
    Result := NullString
  else
    Result := FStrBuffer.ToString;
end;

function TJSONString.Clone: TJSONAncestor;
begin
  if FStrBuffer = nil then
    Result := TJSONString.Create
  else
    Result := TJSONString.Create(Value);
end;

constructor TJSONNumber.Create;
begin
  inherited Create('');
end;

constructor TJSONNumber.Create(const Value: Double);
begin
  inherited Create(TDBXPlatform.JsonFloat(Value));
end;

constructor TJSONNumber.Create(const Value: UnicodeString);
begin
  inherited Create(Value);
end;

constructor TJSONNumber.Create(const Value: Integer);
begin
  inherited Create(IntToStr(Value));
end;

function TJSONNumber.EstimatedByteSize: Integer;
begin
  Result := FStrBuffer.Length;
end;

function TJSONNumber.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Index: Integer;
  Count: Integer;
  CurrentChar: WideChar;
begin
  Offset := Idx;
  Index := 0;
  Count := FStrBuffer.Length;
  while Index < Count do
  begin
    CurrentChar := FStrBuffer.Chars[IncrAfter(Index)];
    Data[IncrAfter(Offset)] := Ord(CurrentChar);
  end;
  Result := Offset;
end;

function TJSONNumber.ToString: UnicodeString;
begin
  Result := FStrBuffer.ToString;
end;

function TJSONNumber.Value: UnicodeString;
begin
  Result := FloatToStr(AsDouble);
end;

function TJSONNumber.Clone: TJSONAncestor;
begin
  Result := TJSONNumber.Create(ToString);
end;

function TJSONNumber.GetAsDouble: Double;
begin
  Result := TDBXPlatform.JsonToFloat(FStrBuffer.ToString);
end;

function TJSONNumber.GetAsInt: Integer;
begin
  Result := TDBXPlatform.JsonToInt(FStrBuffer.ToString);
end;

class function TJSONObject.HexToDecimal(const Value: Byte): Integer;
begin
  if Value > Ord('9') then
  begin
    if Value > Ord('F') then
      Result := (Value - Ord('a') + 10)
    else
      Result := (Value - Ord('A') + 10);
  end
  else
    Result := (Value - Ord('0'));
end;

class function TJSONObject.ParseJSONValue(const Data: TBytes; const Offset: Integer; IsUTF8: Boolean): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Length(Data), IsUTF8);
end;

class function TJSONObject.ParseJSONValue(const Data: TBytes; const Offset: Integer;
                                          const Count: Integer; IsUTF8: Boolean): TJSONValue;
var
  Parent: TJSONArray;
  Answer: TJSONValue;
  Br: TJSONByteReader;
begin
  Parent := TJSONArray.Create;
  Answer := nil;
  Br := TJSONByteReader.Create(Data, Offset, Count, IsUTF8);
  try
    ConsumeWhitespaces(Br);
    if (ParseValue(Br, Parent) = Count) and (Parent.Size = 1) then
      Answer := Parent.Pop;
    Result := Answer;
  finally
    Parent.Free;
    Br.Free;
  end;
end;

class function TJSONObject.ParseJSONValue(const Data: String): TJSONValue;
begin
  raise Exception.Create('Erro convertendo UTF8. Método não implementado.');
//  Result := ParseJSONValue(TRpEncoding.UTF8.GetBytes(Data), 0, True);
end;

class function TJSONObject.ParseJSONValue(const Data: UTF8String): TJSONValue;
begin
  Result := ParseJSONValue(BytesOf(Data), 0, True);
end;

class function TJSONObject.ParseJSONValueUTF8(const Data: TBytes; const Offset: Integer; const Count: Integer): TJSONValue;
begin
  Result := ParseJSONValue(Data, Offset, Count, True);
end;

class function TJSONObject.ParseJSONValueUTF8(const Data: TBytes; const Offset: Integer): TJSONValue;
begin
  Result := ParseJSONValueUTF8(Data, Offset, Length(Data));
end;

constructor TJSONObject.Create;
begin
  inherited Create;
  FMembers := TRpJSONArrayList.Create;
end;

constructor TJSONObject.Create(const Pair: TJSONPair);
begin
  Create;
  if Pair <> nil then
    FMembers.Add(Pair);
end;

procedure TJSONObject.SetMemberList(AList: TRpJSONArrayList);
begin
  FMembers.Free;
  FMembers := AList;
end;

function TJSONObject.Size: Integer;
begin
  Result := FMembers.Count;
end;

function TJSONObject.Get(const I: Integer): TJSONPair;
begin
  if (I >= 0) and (I < Size) then
    Result := TJSONPair(FMembers[I])
  else
    Result := nil;
end;

function TJSONObject.Get(const PairName: UnicodeString): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do
  begin
    Candidate := TJSONPair(FMembers[I]);
    if (Candidate.JsonString.Value = PairName) then
    begin
      Result := Candidate;
      Exit;
    end;
  end;
  Result := nil;
end;

function TJSONObject.GetObject: TJSONObject;
begin
  Result := Self;
end;

destructor TJSONObject.Destroy;
var
  Member: TJSONAncestor;
  I: Integer;
begin
  if FMembers <> nil then
  begin
    for i := 0 to FMembers.Count - 1 do
    begin
      Member := TJSONAncestor(FMembers[I]);
      if Member.GetOwned then
        Member.Free;
    end;
    FreeAndNil(FMembers);
  end;
  inherited Destroy;
end;

function TJSONObject.AddPair(const Pair: TJSONPair): TJSONObject;
begin
  if Pair <> nil then
    AddDescendant(Pair);
  Result := self;
end;

function TJSONObject.AddPair(const Str: TJSONString; const Val: TJSONValue): TJSONObject;
begin
  if (Str <> nil) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

function TJSONObject.AddPair(const Str: UnicodeString; const Val: TJSONValue): TJSONObject;
begin
  if (not StringIsNil(Str)) and (Val <> nil) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

function TJSONObject.AddPair(const Str: UnicodeString; const Val: UnicodeString): TJSONObject;
begin
  if (not StringIsNil(Str)) and (not StringIsNil(Val)) then
    AddPair(TJSONPair.Create(Str, Val));
  Result := self;
end;

procedure TJSONObject.AddDescendant(const Descendant: TJSONAncestor);
begin
  FMembers.Add(Descendant);
end;

function TJSONObject.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for i := 0 to FMembers.Count - 1 do
    Size := Size + (TJSONAncestor(FMembers[I])).EstimatedByteSize + 1;
  if Size = 1 then
    Result := 2
  else
    Result := Size;
end;

function TJSONObject.ToBytes(const Data: TBytes; const Idx: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Idx;
  Size := FMembers.Count;
  Data[IncrAfter(Offset)] := Ord('{');
  if Size > 0 then
    Offset := (TJSONAncestor(FMembers[0])).ToBytes(Data, Offset);
  for i := 1 to FMembers.Count - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FMembers[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord('}');
  Result := Offset;
end;

function TJSONObject.Clone: TJSONAncestor;
var
  Data: TJSONObject;
  I: Integer;
begin
  Data := TJSONObject.Create;
  for i := 0 to FMembers.Count - 1 do
    Data.AddPair(TJSONPair(Get(I).Clone));
  Result := Data;
end;

function TJSONObject.Parse(const Data: TBytes; const Pos: Integer): Integer;
var
  Offset: Integer;
  Count: Integer;
begin
  Count := Length(Data);
  Offset := Parse(Data, Pos, Count);
  if Offset = Count then
    Result := Count
  else if Offset < 0 then
    Result := Offset
  else
    Result := -Offset;
end;

function TJSONObject.Parse(const Data: TBytes; const Pos: Integer; const Count: Integer): Integer;
var
  Br: TJSONByteReader;
begin
  if (Data = nil) or (Pos < 0) or (Pos >= Count) then
  begin
    REsult := -1;
    Exit;
  end;
  Br := TJSONByteReader.Create(Data, Pos, Count);
  try
    Result := Parse(Br);
  finally
    Br.Free;
  end;
end;

function TJSONObject.Parse(const Br: TJSONByteReader): Integer;
var
  SepPos: Integer;
  PairExpected: Boolean;

  procedure InternalExit(AValor : Integer);
  begin
    Result := AValor;
    Exit;
  end;

begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    InternalExit(-Br.Offset);
  if Br.PeekByte <> Ord('{') then
    InternalExit(-Br.Offset);
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  if Br.Empty then
    InternalExit(-Br.Offset);
  PairExpected := False;
  while PairExpected or (Br.PeekByte <> Ord('}')) do
  begin
    SepPos := ParsePair(Br, self);
    if SepPos <= 0 then
      InternalExit(SepPos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      InternalExit(-Br.Offset);
    PairExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ConsumeWhitespaces(Br);
      PairExpected := True;
      if Br.PeekByte = Ord('}') then
        InternalExit(-Br.Offset);
    end;
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class procedure TJSONObject.ConsumeWhitespaces(const Br: TJSONByteReader);
var
  Current: Byte;
begin
  while not Br.Empty do
  begin
    Current := Br.PeekByte;
    case Current of
      32,
      9,
      10,
      13:
        Br.ConsumeByte;
      else
        Exit;
    end;
  end;
end;

class function TJSONObject.ParseObject(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  JsonObj: TJSONObject;
begin
  JsonObj := TJSONObject.Create;
  Parent.AddDescendant(JsonObj);
  Result := JsonObj.Parse(Br);
end;

class function TJSONObject.ParsePair(const Br: TJSONByteReader; const Parent: TJSONObject): Integer;
var
  Pair: TJSONPair;
  CommaPos: Integer;

  procedure InternalExit(AValor : Byte);
  begin
    Result := AValor;
    Exit;
  end;

begin
  Pair := TJSONPair.Create;
  Parent.AddDescendant(Pair);
  CommaPos := ParseString(Br, Pair);
  if CommaPos > 0 then
  begin
    ConsumeWhitespaces(Br);
    if Br.Empty then
      InternalExit(-Br.Offset);
    if Br.PeekByte <> Ord(':') then
      InternalExit(-Br.Offset);
    Br.ConsumeByte;
    ConsumeWhitespaces(Br);
    CommaPos := ParseValue(Br, Pair);
  end;
  Result := CommaPos;
end;

class function TJSONObject.ParseArray(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  ValueExpected: Boolean;
  JsonArray: TJSONArray;
  Pos: Integer;

  procedure InternalExit(AValor : Integer);
  begin
    Result := AValor;
    Exit;
  end;

begin
  ConsumeWhitespaces(Br);
  if Br.Empty then
    InternalExit(-Br.Offset);
  if Br.PeekByte <> Ord('[') then
    InternalExit(-Br.Offset);
  Br.ConsumeByte;
  JsonArray := TJSONArray.Create;
  Parent.AddDescendant(JsonArray);
  ValueExpected := False;
  while ValueExpected or (Br.PeekByte <> Ord(']')) do
  begin
    ConsumeWhitespaces(Br);
    Pos := ParseValue(Br, JsonArray);
    if Pos <= 0 then
      InternalExit(Pos);
    ConsumeWhitespaces(Br);
    if Br.Empty then
      InternalExit(-Br.Offset);
    ValueExpected := False;
    if Br.PeekByte = Ord(',') then
    begin
      Br.ConsumeByte;
      ValueExpected := True;
    end
    else if Br.PeekByte <> Ord(']') then
      InternalExit(-Br.Offset);

//    if Br.Offset > Br.FRange then
//      Break;
  end;
  Br.ConsumeByte;
  ConsumeWhitespaces(Br);
  Result := Br.Offset;
end;

class function TJSONObject.ParseValue(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  Pos: Integer;

  procedure InternalExit(AValor : Integer);
  begin
    Result := AValor;
    Exit;
  end;

begin
  Pos := Br.Offset;
  if Br.Empty then
    InternalExit(-Pos);
  case Br.PeekByte of
    Ord('"'):
      InternalExit(ParseString(Br, Parent));
    Ord('-'),
    Ord('0'),
    Ord('1'),
    Ord('2'),
    Ord('3'),
    Ord('4'),
    Ord('5'),
    Ord('6'),
    Ord('7'),
    Ord('8'),
    Ord('9'):
      InternalExit(ParseNumber(Br, Parent));
    Ord('{'):
      InternalExit(ParseObject(Br, Parent));
    Ord('['):
      InternalExit(ParseArray(Br, Parent));
    Ord('t'):
      begin
        if not Br.HasMore(3) then
          InternalExit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('r')) or (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('e')) then
          InternalExit(-Pos);
        Parent.AddDescendant(TJSONTrue.Create);
        InternalExit(Br.Offset);
      end;
    Ord('f'):
      begin
        if not Br.HasMore(4) then
          InternalExit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('a')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('s')) or (Br.ConsumeByte <> Ord('e')) then
          InternalExit(-Pos);
        Parent.AddDescendant(TJSONFalse.Create);
        InternalExit(Br.Offset);
      end;
    Ord('n'):
      begin
        if not Br.HasMore(3) then
          InternalExit(-Pos);
        Br.ConsumeByte;
        if (Br.ConsumeByte <> Ord('u')) or (Br.ConsumeByte <> Ord('l')) or (Br.ConsumeByte <> Ord('l')) then
          InternalExit(-Pos);
        Parent.AddDescendant(TJSONNull.Create);
        InternalExit(Br.Offset);
      end;
  end;
  Result := -Pos;
end;

function TJSONObject.RemovePair(const PairName: String): TJSONPair;
var
  Candidate: TJSONPair;
  I: Integer;
begin
  for i := 0 to Size - 1 do
  begin
    Candidate := TJSONPair(FMembers[I]);
    if (Candidate.JsonString.Value = PairName) then
    begin
      FMembers.RemoveAt(i);
      Result := Candidate;
      Exit;
    end;
  end;
  Result := nil;
end;

class function TJSONObject.ParseNumber(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  Nb: TJSONNumber;
  Consume: Boolean;
  Exponent: Boolean;
  OneAdded: Boolean;

  procedure InternalExit(AValor : Integer);
  begin
    Result := AValor;
    Exit;
  end;


begin
  Nb := TJSONNumber.Create;
  Parent.AddDescendant(Nb);
  if Br.PeekByte = Ord('-') then
  begin
    Nb.AddChar('-');
    Br.ConsumeByte;
    if Br.Empty then
      InternalExit(-1);
  end;
  if Br.PeekByte = Ord('0') then
  begin
    Nb.AddChar('0');
    Br.ConsumeByte;
    if Br.Empty then
      InternalExit(Br.Offset);
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        InternalExit(-Br.Offset);
    end;
  end;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          if Br.Empty then
            InternalExit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  Exponent := False;
  if Br.PeekByte = Ord('.') then
  begin
    Nb.AddChar('.');
    Br.ConsumeByte;
    if Br.Empty then
      InternalExit(-Br.Offset);
  end
  else if (Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E')) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    Exponent := True;
    if Br.Empty then
      InternalExit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        InternalExit(-Br.Offset);
    end;
  end
  else
    InternalExit(Br.Offset);
  OneAdded := False;
  Consume := True;
  while Consume do
    case Br.PeekByte of
      Ord('0'),
      Ord('1'),
      Ord('2'),
      Ord('3'),
      Ord('4'),
      Ord('5'),
      Ord('6'),
      Ord('7'),
      Ord('8'),
      Ord('9'):
        begin
          Nb.AddChar(WideChar(Br.ConsumeByte));
          OneAdded := True;
          if Br.Empty then
            InternalExit(Br.Offset);
        end;
      else
        Consume := False;
    end;
  if not OneAdded then
    InternalExit(-Br.Offset);
  if not Exponent and ((Br.PeekByte = Ord('e')) or (Br.PeekByte = Ord('E'))) then
  begin
    Nb.AddChar(WideChar(Br.ConsumeByte));
    if Br.Empty then
      InternalExit(-Br.Offset);
    if (Br.PeekByte = Ord('-')) or (Br.PeekByte = Ord('+')) then
    begin
      Nb.AddChar(WideChar(Br.ConsumeByte));
      if Br.Empty then
        InternalExit(-Br.Offset);
    end;
    OneAdded := False;
    Consume := True;
    while Consume do
      case Br.PeekByte of
        Ord('0'),
        Ord('1'),
        Ord('2'),
        Ord('3'),
        Ord('4'),
        Ord('5'),
        Ord('6'),
        Ord('7'),
        Ord('8'),
        Ord('9'):
          begin
            Nb.AddChar(WideChar(Br.ConsumeByte));
            OneAdded := True;
            if Br.Empty then
              InternalExit(Br.Offset);
          end;
        else
          Consume := False;
      end;
    if not OneAdded then
      InternalExit(-Br.Offset);
  end;
  Result := Br.Offset;
end;

class function TJSONObject.ParseString(const Br: TJSONByteReader; const Parent: TJSONAncestor): Integer;
var
  UnicodeCh: Integer;
  Ch: WideChar;
  Str: TJSONString;

  procedure InternalExit(AValor : Integer);
  begin
    Result := AValor;
    Exit;
  end;

begin
  Ch := ' ';
  if Br.PeekByte <> Ord('"') then
    InternalExit(-Br.Offset);
  Br.ConsumeByte;
  if Br.Empty then
    InternalExit(-Br.Offset);
  Str := TJSONString.Create('');
  Parent.AddDescendant(Str);
  while Br.PeekByte <> Ord('"') do
  begin
    case Br.PeekByte of
      Ord('\'):
        begin
          Br.ConsumeByte;
          if Br.Empty then
            InternalExit(-Br.Offset);
          case Br.PeekByte of
            Ord('"'):
              Ch := '"';
            Ord('\'):
              Ch := '\';
            Ord('/'):
              Ch := '/';
            Ord('b'):
              Ch := #$8;
            Ord('f'):
              Ch := #$c;
            Ord('n'):
              Ch := #$a;
            Ord('r'):
              Ch := #$d;
            Ord('t'):
              Ch := #$9;
            Ord('u'):
              begin
                Br.ConsumeByte;
                if not Br.HasMore(3) then
                  InternalExit(-Br.Offset);
                UnicodeCh := HexToDecimal(Br.ConsumeByte) shl 12;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 8;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.ConsumeByte) shl 4;
                UnicodeCh := UnicodeCh or HexToDecimal(Br.PeekByte);
                Ch := WideChar(UnicodeCh);
              end;
            else
              InternalExit(-Br.Offset);
          end;
        end;
      else
        Ch := WideChar(Br.PeekByte);
    end;
    Str.AddChar(Ch);
    Br.ConsumeByte;
    if Br.Empty then
      InternalExit(-Br.Offset);
  end;
  Br.ConsumeByte;
  Result := Br.Offset;
end;

function TJSONObject.ToString: UnicodeString;
var
  Buf: TRpJSONStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FMembers.Count;
  Buf := TRpJSONStringBuffer.Create;
  try
    Buf.Append('{');
    if Size > 0 then
      Buf.Append(TJSONAncestor(FMembers[0]).ToString);
    for i := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(TJSONAncestor(FMembers[I]).ToString);
    end;
    Buf.Append('}');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

procedure TJSONNull.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONNull.IsNull: Boolean;
begin
  Result := True;
end;

function TJSONNull.EstimatedByteSize: Integer;
begin
  Result := 4;
end;

function TJSONNull.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('n');
  Data[IncrAfter(Idx)] := Ord('u');
  Data[IncrAfter(Idx)] := Ord('l');
  Data[IncrAfter(Idx)] := Ord('l');
  Result := Idx;
end;

function TJSONNull.ToString: UnicodeString;
begin
  Result := 'null';
end;

function TJSONNull.Clone: TJSONAncestor;
begin
  Result := TJSONNull.Create;
end;

procedure TJSONFalse.AddDescendant(const Descendant: TJSONAncestor);
begin
end;

function TJSONFalse.EstimatedByteSize: Integer;
begin
  Result := 5;
end;

function TJSONFalse.ToBytes(const Data: TBytes; const Offset: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := Offset;
  Data[IncrAfter(Idx)] := Ord('f');
  Data[IncrAfter(Idx)] := Ord('a');
  Data[IncrAfter(Idx)] := Ord('l');
  Data[IncrAfter(Idx)] := Ord('s');
  Data[IncrAfter(Idx)] := Ord('e');
  Result := Idx;
end;

function TJSONFalse.ToString: UnicodeString;
begin
  Result := 'false';
end;

function TJSONFalse.Clone: TJSONAncestor;
begin
  Result := TJSONFalse.Create;
end;

constructor TJSONArray.Create;
begin
  inherited Create;
  FElements := TRpJSONArrayList.Create;
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
end;

constructor TJSONArray.Create(const FirstElem: TJSONValue; const SecondElem: TJSONValue);
begin
  Create;
  AddElement(FirstElem);
  AddElement(SecondElem);
end;

constructor TJSONArray.Create(const FirstElem: String; const SecondElem: String);
begin
  Create;
  AddElement(TJSONString.Create(FirstElem));
  AddElement(TJSONString.Create(SecondElem));
end;

destructor TJSONArray.Destroy;
var
  Element: TJSONAncestor;
  I: Integer;
begin
  if FElements <> nil then
  begin
    for i := 0 to FElements.Count - 1 do
    begin
      Element := TJSONAncestor(FElements[I]);
      if Element.GetOwned then
        Element.Free;
    end;
    FreeAndNil(FElements);
  end;
  inherited Destroy;
end;

procedure TJSONArray.SetElements(AList: TRpJSONArrayList);
begin
  FElements.Free;
  FElements := AList;
end;

function TJSONArray.Size: Integer;
begin
  if (FElements = nil) or (FElements.Count = 0) then
    REsult := 0
  else
    Result := FElements.Count;
end;

function TJSONArray.Get(const Index: Integer): TJSONValue;
begin
  if (Index < 0) or (Index >= Size) then
    Result := nil
  else
    Result := TJSONValue(FElements[Index]);
end;

procedure TJSONArray.AddDescendant(const Descendant: TJSONAncestor);
begin
  FElements.Add(Descendant);
end;

function TJSONArray.Pop: TJSONValue;
var
  Value: TJSONValue;
begin
  Value := TJSONValue(FElements[0]);
  FElements.RemoveAt(0);
  Result := Value;
end;

procedure TJSONArray.AddElement(const Element: TJSONValue);
begin
  if Element <> nil then
    AddDescendant(Element);
end;

function TJSONArray.Add(const Element: UnicodeString): TJSONArray;
begin
  AddElement(TJSONString.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Integer): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Double): TJSONArray;
begin
  AddElement(TJSONNumber.Create(Element));
  Result := self;
end;

function TJSONArray.Add(const Element: Boolean): TJSONArray;
begin
  if Element then
    AddElement(TJSONTrue.Create)
  else
    AddElement(TJSONFalse.Create);
  Result := self;
end;

function TJSONArray.Add(const Element: TJSONObject): TJSONArray;
begin
  if Element <> nil then
    AddElement(Element)
  else
    AddElement(TJSONNull.Create);
  Result := self;
end;

function TJSONArray.Add(const Element: TJSONArray): TJSONArray;
begin
  AddElement(Element);
  Result := self;
end;

function TJSONArray.EstimatedByteSize: Integer;
var
  Size: Integer;
  I: Integer;
begin
  Size := 1;
  for i := 0 to FElements.Count - 1 do
    Size := Size + (TJSONAncestor(FElements[I])).EstimatedByteSize + 1;
  if Size = 1 then
    REsult := 2
  else
    Result := Size;
end;

function TJSONArray.ToBytes(const Data: TBytes; const Pos: Integer): Integer;
var
  Offset: Integer;
  Size: Integer;
  I: Integer;
begin
  Offset := Pos;
  Size := FElements.Count;
  Data[IncrAfter(Offset)] := Ord('[');
  if Size > 0 then
    Offset := (TJSONAncestor(FElements[0])).ToBytes(Data, Offset);
  for i := 1 to Size - 1 do
  begin
    Data[IncrAfter(Offset)] := Ord(',');
    Offset := (TJSONAncestor(FElements[I])).ToBytes(Data, Offset);
  end;
  Data[IncrAfter(Offset)] := Ord(']');
  Result := Offset;
end;

function TJSONArray.ToString: UnicodeString;
var
  Buf: TRpJSONStringBuffer;
  Size: Integer;
  I: Integer;
begin
  Size := FElements.Count;
  Buf := TRpJSONStringBuffer.Create;
  try
    Buf.Append('[');
    if Size > 0 then
      Buf.Append(TJSONAncestor(FElements[0]).ToString);
    for i := 1 to Size - 1 do
    begin
      Buf.Append(',');
      Buf.Append(TJSONAncestor(FElements[I]).ToString);
    end;
    Buf.Append(']');
    Result := Buf.ToString;
  finally
    Buf.Free;
  end;
end;

function TJSONArray.Clone: TJSONAncestor;
var
  Data: TJSONArray;
  I: Integer;
begin
  Data := TJSONArray.Create;
  for i := 0 to Size - 1 do
    Data.AddDescendant(Get(I).Clone);
  Result := Data;
end;

{ TJSONStrinNoQuote }

function TJSONStrinNoQuote.Clone: TJSONAncestor;
begin
  if FStrBuffer = nil then
    Result := TJSONStrinNoQuote.Create
  else
    Result := TJSONStrinNoQuote.Create(Value);
end;

function TJSONStrinNoQuote.ToString: UnicodeString;
begin
  if FStrBuffer <> nil then
    Result := FStrBuffer.ToString
  else
    Result := NullString;
end;

end.
