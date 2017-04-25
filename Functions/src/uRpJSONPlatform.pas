unit uRpJSONPlatform;

{$IFDEF ANDROID}
  {$I C:\Componentes\RafaPack\Common\src\RpInc.inc}
{$ELSE}
  {$I ..\Common\src\RpInc.inc}
{$ENDIF}

interface

uses
  Math, SysUtils, StrUtils
  {$IFDEF ANDROID}
    , System.Generics.Collections
  {$ELSE}
    {$IFDEF XE3UP}
    , System.Contnrs
    {$ELSE}
    , Contnrs
    {$ENDIF}
  {$ENDIF}
  ;

type
  UnicodeString = string;
  {$IFDEF ANDROID}
  AnsiString = string;
  {$ENDIF}

  TRpJSONStringBuffer = class
  private
    FBuffer: String;
    FCount: Integer;
  private
    function CharAt( const Idx: Integer): WideChar;
  public
    constructor Create; overload;
    constructor Create(InitialSize: Integer); overload;
    constructor Create(const Value: String); overload;
    procedure Append(const Value: String); overload;
    procedure Append(const Value: Integer); overload;
    procedure Append(const Value: TRpJSONStringBuffer); overload;
    property Length: Integer read FCount write FCount;
    procedure Replace(const Original, Replacement: String; const StartIndex: Integer; const Count: Integer);
    {$IFDEF UNICODE}
    function ToString: String; override;
    {$ELSE}
    function ToString: String;
    {$ENDIF}
    function Substring(const Ordinal: Integer): String;
    property Chars[ const Index: Integer]: WideChar read CharAt;
  end;

  TRpJSONArrayList = class
  private
    FList: {$IFDEF ANDROID} TList<TObject> {$ELSE} TObjectList {$ENDIF};
//    FCount: Integer;
  private
    function GetValue(Index: Integer): TObject;
    procedure SetValue(Index: Integer; Value: TObject);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Element: TObject);
    procedure RemoveAt(Index: Integer);
    procedure Remove(Index: Integer); overload;
    procedure Remove(Element: TObject); overload;
  public
    property Count: Integer read GetCount;
    property Values[Index: Integer]: TObject read GetValue write SetValue; default;
  end;

  TRpJSONInt32Object = class
  public
    constructor Create(const Value: Integer);
    destructor Destroy; override;
    function IntValue: Integer;
  private
    FValue: Integer;
  end;

  TDBXPlatform = class
//    class function AnsiStrToBytes(const Value: AnsiString): TBytes; static;
//    class function WideStrToBytes(const Value: UnicodeString): TBytes; static; inline;
//    class function BytesToWideStr(const Value: TBytes): UnicodeString; static;
//    class function BytesToAnsiStr(const Value: TBytes): AnsiString; static; inline;

//    class function GetStringBuilderLength(const Value: TDBXAnsiStringBuilder): TInt32; static;
//    class function CreateStringBuilder(Length:  TInt32): TDBXAnsiStringBuilder; static;
//    class function ToAnsiString(const Value: TDBXAnsiStringBuilder): AnsiString; static;
//    class procedure CopyStringBuilder(const Value: TDBXAnsiStringBuilder; var Dest: AnsiString); static; inline;
//    class procedure FreeAndNilStringBuilder(var Value: TDBXAnsiStringBuilder); static;
//    class procedure ResizeStringBuilder(var Value: TDBXAnsiStringBuilder; Size: Integer); static;
//    class procedure CopyStringToBuilder(const Source: AnsiString; AnsiStringBuilderSize: Integer; var Value: TDBXAnsiStringBuilder); static;
//
//    class function GetWideStringBuilderLength(const Value: TDBXWideStringBuilder): TInt32; static; inline;
//    class function CreateWideStringBuilder(Length:  TInt32): TDBXWideStringBuilder; static; inline;
//    class function ToWideString(const Value: TDBXWideStringBuilder): UnicodeString; static; inline;
//    class procedure CopyWideStringBuilder(const Value: TDBXWideStringBuilder; var Dest: UnicodeString); static; inline;
//    class procedure ResizeWideStringBuilder(var Value: TDBXWideStringBuilder; Size: Integer); static; inline;
//    class procedure FreeAndNilWideStringBuilder(var Value: TDBXWideStringBuilder); static;
//    class procedure CopyWideStringToBuilder(const Source: UnicodeString; WideStringBuilderSize: Integer; var Value: TDBXWideStringBuilder); static;
//
//    class procedure CopyInt32Array(const Source: TDBXInt32s; SourceOffset: Integer; const Dest: TDBXInt32s; DestOffset: Integer; Count: Integer); static; inline;
//    class procedure CopyCharArray(const Source: TDBXWideChars; SourceOffset: Integer; const Dest: TDBXWideChars; DestOffset: Integer; Count: Integer); static; inline;
//    class procedure CopyByteArray(const Source: TBytes; SourceOffset: Integer; const Dest: TBytes; DestOffset: Integer; Count: Integer); static; inline;
//    class function  CreateWideString(const Source: TDBXWideChars; Count: Integer): UnicodeString; static; inline;
//    class function  CreateAnsiString(const Source: TDBXAnsiChars; Count: Integer): AnsiString; static; inline;
//    class procedure WriteAsciiBytes(const Message: String; ByteBuffer: TBytes; Offset: Integer; Count: Integer);
//    class function  Int64BitsToDouble(const value: Int64): Double;
//    class function  DoubleToInt64Bits(const value: Double): Int64;
//    class function Int32BitsToSingle(const value: TInt32): Single; static;
//    class function SingleToInt32Bits(const Value: Single): TInt32; static;
//    class procedure CopyBytesToAnsiChars(const Source: TBytes; SourceOffset: Integer; const Dest: TDBXAnsiChars; DestOffset: Integer; Count: Integer); static; inline;
//
//    class procedure CopyInt32(const Value: Integer; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyInt16(const Value: SmallInt; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyUInt16(const Value: Word; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyInt64(const Value: Int64; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyInt8(const Value: ShortInt; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyUInt8(const Value: Byte; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopySqlTimeStamp(const Value: TSQLTimeStamp; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopySqlTimeStampOffset(const Value: TSQLTimeStampOffset; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure CopyBcd(const Value: TBcd; const Dest: TBytes; const DestOffset: Integer); static; inline;
//    class procedure Sleep(const Duration: Int64);
//    class function GetClassName(ObjectValue: TObject): UnicodeString; static;
//    class function GetNestedException(Ex: Exception): Exception; static;
//    class function GetPublicKeyToken: string;

    ///  <summary>  Converts a double into a string using dot character
    ///  </summary>
    class function JsonFloat(Value: Double): string; static;
    class function JsonToFloat(DotValue: String): double; static;
    class function JsonToInt(Value: String): integer; static;
    ///  <summary>  Checks to see if the argument represents a valid boolean string representation
    ///  </summary>
    ///  <param name="Value">The string to check for if it is a boolean</param>
    ///  <result>true if the string value is a boolean, false otherwise</result>
//    class function IsBoolean(const value: String): Boolean; static;
//
//    class function StringOf(data: TBytes; const size: integer): String;

//  private
//    class procedure UnexpectedStringOverflow(const Source: UnicodeString; WideStringBuilderSize: Integer); overload;static;
//    class procedure UnexpectedStringOverflow(const Source: AnsiString; AnsiStringBuilderSize: Integer); overload;static;
  end;

function Incr(var Arg: Integer): Integer; inline;
function Decr(var Arg: Integer): Integer; inline;
function IncrAfter(var Arg: Integer): Integer; inline;
function DecrAfter(var Arg: Integer): Integer; inline;
function C_Conditional(const Condition: Boolean; const TruePart, FalsePart: UnicodeString): UnicodeString;
function BytesOf(const Val: AnsiString): TBytes;
function StringIsNil(const Str: UnicodeString): Boolean;
//function CompareTimeStamp(const ATimeStamp: TSQLTimeStamp; const BTimeStamp: TSQLTimeStamp): Integer;
//function CompareTimeStampOffset(const ATimeStamp: TSQLTimeStampOffset; const BTimeStamp: TSQLTimeStampOffset): Integer;

const
  NullString = '';

implementation

function StringIsNil(const Str: UnicodeString): Boolean;
begin
  Result := Str = NullString;
end;

function BytesOf(const Val: AnsiString): TBytes;
var
  Len: Integer;
begin
  Len := Length(Val);
  SetLength(Result, Len);
  Move(Val[1], Result[0], Len);
end;

procedure Zap(Buffer: TRpJSONStringBuffer);
var
  I: Integer;
begin
  for I := Buffer.FCount+1 to Length(Buffer.FBuffer) do
    Buffer.FBuffer[I] := ' ';
end;

function Incr(var Arg: Integer): Integer; inline;
begin
  Inc(Arg);
  Result := Arg;
end;

function Decr(var Arg: Integer): Integer; inline;
begin
  Dec(Arg);
  Result := Arg;
end;

function IncrAfter(var Arg: Integer): Integer; inline;
begin
  Result := Arg;
  Inc(Arg);
end;

function DecrAfter(var Arg: Integer): Integer; inline;
begin
  Result := Arg;
  Dec(Arg);
end;

function C_Conditional(const Condition: Boolean; const TruePart, FalsePart: UnicodeString): UnicodeString;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

{ TDBXStringBuffer }

constructor TRpJSONStringBuffer.Create;
begin
  inherited Create;
end;

constructor TRpJSONStringBuffer.Create(InitialSize: Integer);
begin
  inherited Create;
  SetLength(FBuffer,InitialSize);
end;

constructor TRpJSONStringBuffer.Create(const Value: String);
begin
  inherited Create;
  Append( Value );
end;

function TRpJSONStringBuffer.ToString: String;
begin
  if FCount > System.Length(FBuffer) then
    SetLength(FBuffer,FCount);
  Result := Copy(FBuffer,0,FCount);
end;

function TRpJSONStringBuffer.Substring(const Ordinal: Integer): String;
begin
  if Ordinal >= FCount then
    Result := ''
  else
    Result := Copy(FBuffer, Ordinal, FCount - Ordinal);
end;

procedure TRpJSONStringBuffer.Append(const Value: String);
var
  Pos: Integer;
begin
  if FCount+System.Length(Value) > System.Length(FBuffer) then
    SetLength(FBuffer,Math.Max(2*System.Length(FBuffer),System.Length(FBuffer)+System.Length(Value)));
  for Pos := 1 to System.Length(Value) do
    FBuffer[FCount+Pos] := Value[Pos];
  FCount := FCount + System.Length(Value);
end;

procedure TRpJSONStringBuffer.Append(const Value: Integer);
begin
  Append(IntToStr(Value));
end;

procedure TRpJSONStringBuffer.Append(const Value: TRpJSONStringBuffer);
begin
  Append(Value.ToString);
end;

procedure TRpJSONStringBuffer.Replace(const Original, Replacement: String; const StartIndex: Integer; const Count: Integer);
var
  Part: UnicodeString;
begin
  Part := Copy(FBuffer, StartIndex+1, Count);
  Part := ReplaceStr(Part, Original, Replacement);
  Self.FCount := StartIndex;
  Zap(Self);
  Append(Part);
  Zap(Self);
end;

function TRpJSONStringBuffer.CharAt( const Idx: Integer ): WideChar;
begin
  Result := WideChar(FBuffer[ Idx + 1 ]);
end;

{ TDBXArrayList }

constructor TRpJSONArrayList.Create;
begin
  inherited;
  {$IFDEF ANDROID}
  FList := TList<TObject>.Create;
//  FList.OwnsObjects := False;
  {$ELSE}
  FList := TObjectList.Create;
  FList.OwnsObjects := False;
  {$ENDIF}
//  SetLength(FList,30);
end;

destructor TRpJSONArrayList.Destroy;
begin
  FreeAndNil(FList);
//  SetLength(FList,0);
  inherited;
end;

function TRpJSONArrayList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TRpJSONArrayList.GetValue(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

procedure TRpJSONArrayList.SetValue(Index: Integer; Value: TObject);
begin
  FList[Index] := Value;
end;

procedure TRpJSONArrayList.Remove(Element: TObject);
begin
  FList.Remove(Element);
end;

procedure TRpJSONArrayList.Remove(Index: Integer);
begin
  FList.Remove(FList[Index]);
end;

procedure TRpJSONArrayList.RemoveAt(Index: Integer);
begin
  Remove(Index);
end;

procedure TRpJSONArrayList.Add(Element: TObject);
begin
  FList.Add(Element);
//  if FCount >= Length(FList) then
//    SetLength(FList,FCount*2);
//  FList[FCount] := Element;
//  Inc(FCount);
end;

procedure TRpJSONArrayList.Clear;
//var
//  Index: Integer;
begin
  FList.Clear;
//  for Index := 0 to FCount - 1 do
//    FreeAndNil(FList[Index]);
//  FCount := 0;
end;

{ TDBXInt32Object }

constructor TRpJSONInt32Object.Create(const Value: Integer);
begin
  inherited Create;
  FValue := Value;
end;

destructor TRpJSONInt32Object.Destroy;
begin
  inherited Destroy;
end;


function TRpJSONInt32Object.IntValue: Integer;
begin
  Result := FValue;
end;

{ TDBXPlatform }

class function TDBXPlatform.JsonFloat(Value: Double): string;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';
{$IFDEF CLR}
  // The following two fields are set to avoid null reference exception in Borland.Vcl.SysUtils.AdjustFormatProvider
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.CurrencyString := System.string.Empty;
{$ENDIF}
  Result := FloatToStr(Value, FormatSettings);
end;

class function TDBXPlatform.JsonToFloat(DotValue: String): double;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';
{$IFDEF CLR}
  // The following two fields are set to avoid null reference exception in Borland.Vcl.SysUtils.AdjustFormatProvider
  FormatSettings.ThousandSeparator := ',';
  FormatSettings.CurrencyString := System.string.Empty;
{$ENDIF}
  Result := StrToFloat(DotValue, FormatSettings);
end;

class function TDBXPlatform.JsonToInt(Value: String): integer;
begin
  Result := StrToInt(Value);
end;

end.
