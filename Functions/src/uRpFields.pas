unit uRpFields;

interface

uses
  XMLIntf, Variants, SysUtils, DateUtils, uRpJSONBase, StrUtils,
  uRpDateTimeFunctions;

type
  IRpField = interface
    ['{BC384902-ED93-4B4D-8E1D-9887E0348152}']
    function IsNull : Boolean;
    function GetValue : Variant;
    function GetAsDateTime: TDateTime;
    function GetAsFloat: Double;
    function GetAsString: string;
    function GetAsInteger: Integer;
    function GetAsBoolean: Boolean;
    procedure SetValue(const Value: Variant);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsString(const Value: string);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsBoolean(const Value: Boolean);

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property Value : Variant read GetValue write SetValue;
  end;

  TRpFieldsBase = class(TInterfacedObject, IRpField)
  private
    FFieldName: string;
    FValue: Variant;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsString: string; virtual;
    function GetAsInteger: Integer; virtual;
    function GetAsBoolean: Boolean; virtual;
    procedure SetAsDateTime(const Value: TDateTime); virtual;
    procedure SetAsFloat(const Value: Double); virtual;
    procedure SetAsString(const Value: string); virtual;
    procedure SetAsInteger(const Value: Integer); virtual;
    procedure SetAsBoolean(const Value: Boolean); virtual;
  protected
    function GetValue : Variant; virtual;
    procedure SetValue(const Value: Variant); virtual;
  public
    class function GetDateFormated(const ADateTime : TDateTime) : string; virtual;
    class function GetDoubleFormated(const ADouble : Double) : string;

    function IsNull : Boolean;

    constructor Create(const AFieldName : string); reintroduce;
    property FieldName : string read FFieldName;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: string read GetAsString write SetAsString;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property Value : Variant read GetValue write SetValue;
  end;

  TComponentField = class(TRpFieldsBase)
  protected
    FOwner : TObject;
  public
    constructor Create(const AOwner : TObject); reintroduce;
  end;

  TXMLFieldNode = class(TRpFieldsBase)
  private
    FSource : IXMLNode;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsBoolean(const Value: Boolean); override;
  protected
    function GetValue : Variant; override;
  public
    constructor Create(const ASource : IXMLNode; const AFieldName : string); reintroduce;
  end;

  TJSONFieldNode = class(TRpFieldsBase)
  private
    FSource : IJSONObject;
    function GetAsBoolean: Boolean; override;
    procedure SetAsDateTime(const Value: TDateTime); override;
    procedure SetAsFloat(const Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsInteger(const Value: Integer); override;
    procedure SetAsBoolean(const Value: Boolean); override;
  protected
    function GetPair(out lPair : TJSONPair) : Boolean;
    function GetValue : Variant; override;
  public
    constructor Create(const ASource : IJSONObject; const AFieldName : string); reintroduce;
  end;

implementation

{ TFieldsNode }

constructor TXMLFieldNode.Create(const ASource: IXMLNode;
  const AFieldName: string);
begin
  inherited Create(AFieldName);
  FSource := ASource;
end;

function TXMLFieldNode.GetValue: Variant;
var
  lChild: IXMLNode;
begin
  Result := Null;
  lChild := FSource.ChildNodes.FindNode(FFieldName);
  if lChild <> nil then
  begin
    Result := FSource[FFieldName];
    if Result = Unassigned then
      Result := Null;
  end;
end;

procedure TXMLFieldNode.SetAsBoolean(const Value: Boolean);
begin
  inherited;
  FSource[FFieldName] := Value;
end;

procedure TXMLFieldNode.SetAsDateTime(const Value: TDateTime);
begin
  inherited;
  FSource[FFieldName] := TXMLFieldNode.GetDateFormated(Value);
end;

procedure TXMLFieldNode.SetAsFloat(const Value: Double);
begin
  inherited;
  FSource[FFieldName] := TXMLFieldNode.GetDoubleFormated(Value);
end;

procedure TXMLFieldNode.SetAsInteger(const Value: Integer);
begin
  inherited;
  FSource[FFieldName] := Value;
end;

procedure TXMLFieldNode.SetAsString(const Value: string);
begin
  inherited;
  FSource[FFieldName] := Value;
end;

{ TRpFieldsBase }

constructor TRpFieldsBase.Create(const AFieldName: string);
begin
  FValue := Null;
  FFieldName := AFieldName;
end;

function TRpFieldsBase.GetAsBoolean: Boolean;
var
  lResultado: Variant;
begin
  lResultado := GetValue;
  if VarIsNull(lResultado) then
    Result := False
  else
    try
      Result := lResultado;
    except
      Result := False;
    end;
end;

function TRpFieldsBase.GetAsDateTime: TDateTime;
var
  lDataResultado: string;
  lResultado: Variant;
begin
  Result := 0;

  lResultado := GetValue;
  if VarIsNull(lResultado) then
    Exit;

  lDataResultado := lResultado;

  //2014-10-23
  if (Pos('-', lResultado) > 0) then
  begin
    if (Pos('T', lResultado) > 0) then
      Result := TRpDateTime.StrToDateFmt('yyyy-mm-ddThh:nn:ss:zzzZ', lDataResultado, 0)
    else
      Result := TRpDateTime.StrToDateFmt('yyyy-mm-dd', lDataResultado, 0)
  end;

  // 2014-10-23T15:55:23.123Z
  if (Pos('T', lResultado) > 0) and (Pos('-', lResultado) > 0) then
    Result := TRpDateTime.StrToDateFmt('yyyy-mm-ddThh:nn:ss:zzzZ', lDataResultado, 0);

  // 23.10.2014 15:55:23:789
  if Length(lDataResultado) = 23 then
    Result := TRpDateTime.StrToDateFmt('dd.mm.yyyy hh:nn:ss:zzz', lDataResultado, 0);

  // 03:07:31.2200124
  if Length(lDataResultado) = 16 then
    Result := TRpDateTime.StrToDateFmt('hh:nn:ss:zzz', lDataResultado, 0);

  //Sun, 15 Mar 2015 19:40:15 GMT
  if Length(lDataResultado) = 29 then
    Result := TRpDateTime.StrToDateFmt('DDD, dd MMM yyyy hh:nn:ss', lDataResultado, 0);
end;

function TRpFieldsBase.GetAsFloat: Double;
var
  lFrac, lInt, lDiv: string;
  p: Integer;
  lResultado: Variant;
begin
  lResultado := GetValue;
  if VarIsNull(lResultado) then
    Result := 0
  else
  begin
    try
      lFrac := StringReplace(lResultado, '.,', '.', [rfReplaceAll]);

      // modo antigo trazia com virgula e o novo com ponto
      p := Pos(',', lFrac); // O valor é salvo com "," (antigo)
      if p = 0 then
        p := Pos('.', lFrac);

      if p = 0 then
      begin
        lInt := lFrac;
        lFrac := '0';
      end
      else
      begin
        lInt := Copy(lFrac, 1, p - 1);
        Delete(lFrac, 1, p);
      end;

      lDiv := '1' + StringOfChar('0', Length(lFrac)); // 10   100   10000 ....

      if StrToInt64Def(lInt, 0) < 0 then
        Result := StrToInt64Def(lInt, 0) - (StrToInt64Def(lFrac, 0) / StrToInt64Def(lDiv, 0))
      else
        Result := StrToInt64Def(lInt, 0) + (StrToInt64Def(lFrac, 0) / StrToInt64Def(lDiv, 0));
    except
      Result := 0;
    end;
  end;
end;

function TRpFieldsBase.GetAsInteger: Integer;
var
  lResultado: Variant;
begin
  lResultado := GetValue;
  if VarIsNull(lResultado) then
    Result := 0
  else
    try
      Result := lResultado;
    except
      Result := 0;
    end;
end;

function TRpFieldsBase.GetAsString: string;
var
  lResultado: Variant;
begin
  lResultado := GetValue;
  if VarIsNull(lResultado) then
    Result := ''
  else
    Result := VarToStr(lResultado);
end;

class function TRpFieldsBase.GetDateFormated(const ADateTime: TDateTime): string;
const
  C_DATE_FORMAT = '%4.4d-%2.2d-%2.2d';
  C_TIME_FORMAT = '%2.2d:%2.2d:%2.2d.%3.3d';
var
  D, M, Y, H, N, S, Z: Word;
begin
  try
    DecodeDate(ADateTime, Y, M, D);
    DecodeTime(ADateTime, H, N, S, Z);
  except
    Y := 1900;
    M := 1;
    D := 1;
    H := 0;
    N := 0;
    S := 0;
    Z := 0;
  end;
  Result := Format(C_DATE_FORMAT, [Y, M, D]);
  if (H <> 0) or (N <> 0) or (S <> 0) or (Z <> 0) then
    Result := Result + 'T' + Format(C_TIME_FORMAT, [H, N, S, Z]) + 'Z';
end;

class function TRpFieldsBase.GetDoubleFormated(const ADouble: Double): string;
var
  lInt, lFrac: Double;
  lValue: Double;  
  lIntStr, lFracStr: string;
begin
  try
//    lInt := Int(ADouble);
//    lFrac := Frac(ADouble);

    // Corrigir erro de arredondamento: ADouble = 2 -> Int(ADouble) = 1
    lValue := StrToFloat(FloatToStr(ADouble));
    lInt := Int(lValue);
    lFrac := Frac(lValue);

    lIntStr := FloatToStr(lInt);
    lFracStr := FloatToStr(lFrac);
    Delete(lFracStr, 1, 2); // 0.
  except
    lIntStr := '0';
    lFracStr := '0';
  end;

  Result := lIntStr;
  if lFracStr <> '' then
    Result := Result + '.' + lFracStr;
end;

function TRpFieldsBase.GetValue: Variant;
begin
  Result := FValue;
end;

function TRpFieldsBase.IsNull: Boolean;
begin
  Result := (FValue = Null)
         or (FValue = Unassigned);
end;

procedure TRpFieldsBase.SetAsBoolean(const Value: Boolean);
begin
  SetValue(Value);
end;

procedure TRpFieldsBase.SetAsDateTime(const Value: TDateTime);
begin
  SetValue(Value);
end;

procedure TRpFieldsBase.SetAsFloat(const Value: Double);
begin
  SetValue(Value);
end;

procedure TRpFieldsBase.SetAsInteger(const Value: Integer);
begin
  SetValue(Value);
end;

procedure TRpFieldsBase.SetAsString(const Value: string);
begin
  SetValue(Value);
end;

procedure TRpFieldsBase.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TJSONFieldNode }

constructor TJSONFieldNode.Create(const ASource: IJSONObject;
  const AFieldName: string);
begin
  inherited Create(AFieldName);
  FSource := ASource;
end;

function TJSONFieldNode.GetAsBoolean: Boolean;
var
  lValue : string;
begin
  lValue := VarToStrDef(GetValue, 'false');
  Result := (LowerCase(lValue) = 'true')
         or (lValue = '1');
end;

function TJSONFieldNode.GetPair(out lPair: TJSONPair): Boolean;
begin
  lPair := FSource.Get(FFieldName);
  Result := lPair <> nil;
end;

function TJSONFieldNode.GetValue: Variant;
var
  lPair: TJSONPair;
  lStr: string;
begin
  Result := Null;
  if GetPair(lPair) then
  begin
    Result := lPair.FieldValue;
    if Result = Unassigned then
      Result := Null
    else
    begin
      lStr := VarToStr(Result);
      if (lStr <> '') and (lStr[1] = '"') and (lStr[Length(lStr)] = '"') then
        Result := Copy(lStr, 2, Length(lStr) - 2);
    end;
  end;
end;

procedure TJSONFieldNode.SetAsBoolean(const Value: Boolean);
var
  lPair: TJSONPair;
begin
  inherited;
  if GetPair(lPair) then
    lPair.FieldValue := IfThen(Value, 'true', 'false')
  else
  begin
    if Value then
      FSource.AddPair(FFieldName, TJSONTrue.Create)
    else
      FSource.AddPair(FFieldName, TJSONFalse.Create);
  end;
end;

procedure TJSONFieldNode.SetAsDateTime(const Value: TDateTime);
var
  lPair: TJSONPair;
  lValue: string;
begin
  inherited;
  lValue := TJSONFieldNode.GetDateFormated(Value);

  if GetPair(lPair) then
    lPair.FieldValue := lValue
  else
    FSource.AddPair(FFieldName, TJSONString.Create(lValue) );
end;

procedure TJSONFieldNode.SetAsFloat(const Value: Double);
var
  lValue: string;
  lPair: TJSONPair;
begin
  inherited;
  lValue := TJSONFieldNode.GetDoubleFormated(Value);
  if GetPair(lPair) then
    lPair.FieldValue := lValue
  else
    FSource.AddPair(FFieldName, TJSONString.Create(lValue));
end;

procedure TJSONFieldNode.SetAsInteger(const Value: Integer);
var
  lPair: TJSONPair;
begin
  inherited;
  if GetPair(lPair) then
    lPair.FieldValue := IntToStr(Value)
  else
    FSource.AddPair(FieldName, TJSONNumber.Create(Value));
end;

procedure TJSONFieldNode.SetAsString(const Value: string);
var
  lPair: TJSONPair;
begin
  inherited;
  if GetPair(lPair) then
    lPair.FieldValue := Value
  else
    FSource.AddPair(FFieldName, TJSONString.Create(Value));
end;

{ TComponentFiel }

constructor TComponentField.Create(const AOwner: TObject);
begin
  FOwner := AOwner;
end;

end.
