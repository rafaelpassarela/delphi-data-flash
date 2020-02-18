unit uRpDataFlash.DataSet.Params;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.Classes, System.SysUtils, Data.DB, System.StrUtils
  {$ELSE}
  Classes, SysUtils, DB, StrUtils
  {$ENDIF}
  ;

type
  TRpDataFlashDataSetParams = class(TParams)
  private
    FSQL : TStringList;
  public
    constructor Create(pSql : String); reintroduce;
    destructor Destroy; override;

    function ParseSQL : string; overload;
    procedure AssignSQL(const pSQL : TStringList);
    procedure ParamOrNull(const AParamName : string; const Value : Integer); overload;
    procedure ParamOrNull(const AParamName : string; const Value : Double); overload;
    procedure ParamOrNull(const AParamName : string; const Value : string); overload;
    function GetParamValues : String;

    property SQL : TStringList read FSQL;
  end;

implementation

{ TDBParams }

procedure TRpDataFlashDataSetParams.AssignSQL(const pSQL: TStringList);
begin
  if pSQL is TStringList then
  begin
    FSQL.Assign( pSQL );
    ParseSQL(FSQL.Text, True);
  end
  else
    raise Exception.CreateFmt('A origem do SQL não pode ser identificada. [%s]', [pSQL.ClassName]);
end;

constructor TRpDataFlashDataSetParams.Create(pSql: String);
begin
  inherited Create();
  FSQL := TStringList.Create;
  FSQL.Text := pSql;

  ParseSQL(pSql, True);
end;

destructor TRpDataFlashDataSetParams.Destroy;
begin
  FreeAndNil(FSQL);
  inherited;
end;

function TRpDataFlashDataSetParams.GetParamValues: String;
var
  i : Integer;
  lParam : TParam;
begin
  Result := EmptyStr;

  for i := 0 to Self.Count - 1 do
  begin
    lParam := Self[i];
    Result := Result + sLineBreak + lParam.Name + ' = ' + IfThen(lParam.IsNull, 'NULL', lParam.AsString);
  end;

  if Result <> EmptyStr then
    Result := '/* ' + Result + sLineBreak + ' */';
end;

procedure TRpDataFlashDataSetParams.ParamOrNull(const AParamName: string; const Value: Integer);
begin
  if Value = 0 then
  begin
    ParamByName(AParamName).Clear;
    ParamByName(AParamName).DataType := ftInteger;
  end
  else
    ParamByName(AParamName).AsInteger := Value;
end;

procedure TRpDataFlashDataSetParams.ParamOrNull(const AParamName: string; const Value: Double);
begin
  if Value = 0 then
  begin
    ParamByName(AParamName).Clear;
    ParamByName(AParamName).DataType := ftFloat;
  end
  else
    ParamByName(AParamName).AsFloat := Value;
end;

procedure TRpDataFlashDataSetParams.ParamOrNull(const AParamName, Value: string);
begin
  if string.IsNullOrWhiteSpace(Value) then
  begin
    ParamByName(AParamName).Clear;
    ParamByName(AParamName).DataType := ftString;
  end
  else
    ParamByName(AParamName).AsString := Value;
end;

function TRpDataFlashDataSetParams.ParseSQL : string;
begin
  Result := ParseSQL(FSQL.Text, True);
end;

end.
