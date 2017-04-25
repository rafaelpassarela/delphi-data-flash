unit uLRDF.FieldFormatter;

interface

uses
  SysUtils, DB;

type
  EFieldTypeNotSupported = class(Exception)
  public
    constructor CreateField(const pField : string; const pMessage: string);
  end;

  IFormaterMaskValues = interface
    ['{4C0B2FFB-EF75-4984-B658-EFC9593C72B2}']
    function BoolTrueValue : string;
    function BoolFalseValue : string;
    function DateFormat : string;
    function TimeFormat : string;
    function DateTimeFormat : string;
    function QuoteChar : Char;
  end;

  TFieldFormatter = class
  private
    FValue: Variant;
    FFormaterMask: IFormaterMaskValues;
    FDataType: TFieldType;
    FFieldName: string;
    function AsString : string;
    function AsInteger : string;
    function AsFloat : string;
    function AsBoolean : string;
    function AsDate : string;
    function AsTime : string;
    function AsDateTime : string;
    function GetSQLValue: string;
  public
    constructor Create(AFormatMask : IFormaterMaskValues);
    procedure Error(const pFieldName : string; const pMessage: string);
    property Value : Variant read FValue write FValue;
    property DataType : TFieldType read FDataType write FDataType;
    property FieldName : string read FFieldName write FFieldName;
    property SQLValue : string read GetSQLValue;
  end;

implementation

uses
  Variants;

{ TFieldFormatter }

function TFieldFormatter.AsBoolean: string;
var
  lStr : string;
begin
  lStr := VarToStr(Value) + ' ';
  {$IFDEF UNICODE}
  if CharInSet(lStr[1], ['T', 'S', '1']) then
  {$ELSE}
  if lStr[1] in ['T', 'S', '1'] then
  {$ENDIF}
    Result := FFormaterMask.BoolTrueValue
  else
    Result := FFormaterMask.BoolFalseValue;
end;

function TFieldFormatter.AsDate: string;
var
  lDate : TDateTime;
begin
  lDate := VarToDateTime(Value);
  Result := FFormaterMask.QuoteChar + FormatDateTime(FFormaterMask.DateFormat, lDate) + FFormaterMask.QuoteChar;
end;

function TFieldFormatter.AsDateTime: string;
var
  lDate : TDateTime;
begin
  lDate := VarToDateTime(Value);
  Result := FFormaterMask.QuoteChar + FormatDateTime(FFormaterMask.DateTimeFormat, lDate) + FFormaterMask.QuoteChar;
end;

function TFieldFormatter.AsFloat: string;
var
  f : Double;
begin
  try
    f := Value;
    if Frac(f) <> 0 then
    begin
      Result := FormatFloat('#0.0#', f);
      Result := StringReplace(Result, ',', '.', []);
    end
    else
      Result := FormatFloat('#0', f);
  except
    Result := '0';
  end;
end;

function TFieldFormatter.AsInteger: string;
var
  i: Integer;
begin
  try
    i := Value;
  except
    i := 0;
  end;
  Result := IntToStr(i);
end;

function TFieldFormatter.AsString: string;
begin
  Result := FFormaterMask.QuoteChar + VarToStrDef(Value, EmptyStr) + FFormaterMask.QuoteChar;
end;

function TFieldFormatter.AsTime: string;
var
  lTime : TDateTime;
begin
  lTime := VarToDateTime(Value);
  Result := FFormaterMask.QuoteChar + FormatDateTime(FFormaterMask.TimeFormat, lTime) + FFormaterMask.QuoteChar;
end;

constructor TFieldFormatter.Create(AFormatMask: IFormaterMaskValues);
begin
  FFormaterMask := AFormatMask;
end;

procedure TFieldFormatter.Error(const pFieldName : string; const pMessage: string);
begin
  raise EFieldTypeNotSupported.CreateField(pFieldName, pMessage);
end;

function TFieldFormatter.GetSQLValue: string;
begin
    if (Value = Null) or (Value = Unassigned) then
      Result := ' NULL '
    else
{$REGION 'TypeCases'}
      case DataType of
        // ERRO
        ftUnknown,
        ftBytes,
        ftVarBytes,
        ftBlob,
        ftMemo,
        ftGraphic,
        ftFmtMemo,
        ftParadoxOle,
        ftDBaseOle,
        ftTypedBinary,
        ftCursor,
        ftFixedChar,
        ftADT,
        ftArray,
        ftReference,
        ftDataSet,
        ftOraBlob,
        ftOraClob,
        ftInterface,
        ftIDispatch,
        ftGuid,
        ftFixedWideChar,
        ftWideMemo,
        ftOraTimeStamp,
        ftOraInterval : Error(FieldName, 'FieldType: ' + IntToStr(Ord(DataType)) );

        // string
        ftString,
        ftWideString,
        ftVariant : Result := AsString;

        // integer
        ftSmallint,
        ftInteger,
        ftWord : Result := AsInteger;

        // float
        ftAutoInc,
        ftFloat,
        ftCurrency,
        ftBCD,
        ftFMTBcd,
        ftLargeint : Result := AsFloat;

        // bool
        ftBoolean  : Result := AsBoolean;
        // date and time
        ftDate : Result := AsDate;
        ftTime : Result := AsTime;
        ftDateTime,
        ftTimeStamp : Result := AsDateTime;

{$IFDEF UNICODE}
        ftConnection,
        ftParams,
        ftStream,
        ftTimeStampOffset,
        ftObject : Error(FieldName, 'FieldType: ' + IntToStr(Ord(DataType)) );

        ftLongWord,
        ftExtended : Result := AsFloat;

        ftShortint,
        ftByte,
        ftSingle : Result := AsInteger
{$ENDIF}
      end;
{$ENDREGION}
end;

{ EFieldTypeNotSupported }

constructor EFieldTypeNotSupported.CreateField(const pField : string; const pMessage: string);
begin
  Message := 'Erro verificando parâmetros. ' + pMessage + sLineBreak + 'Campo: ' + pField;
end;

end.
