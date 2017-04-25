unit uLRDF.ConvertUtils;

interface

uses DateUtils, SysUtils;

type
  TDateConverter = class
  public
    class function Decode(const ADateTime : TDateTime) : string;
    class function Encode(const ADateTime : string) : TDateTime;
  end;

  TFloatConverter = class
    class function Decode(const AValue : Double) : string;
    class function Encode(const AValue : string) : Double;
  end;

  TURLConverter = class
  public
    class function DecodeURL(const AUrlValue : string) : string;
  end;

implementation

{ TDateConverter }

class function TDateConverter.Decode(const ADateTime : TDateTime): string;
var
  Y, M, D: Word;
  H, N, S, Z: Word;
begin
  try
    DecodeDate(ADateTime, Y, M, D);
    DecodeTime(ADateTime, H, N, S, Z);
  except
    // 01/01/1900 00:00:00:000
    Y := 1900;
    M := 1;
    D := 1;

    H := 0;
    N := 0;
    S := 0;
    Z := 0;
  end;
//                   dd    mm    yyyy   hh   nn     ss    zzz
  Result := Format('%2.2d/%2.2d/%4.4d %2.2d:%2.2d:%2.2d:%3.3d', [D, M, Y, H, N, S, Z]);
end;

class function TDateConverter.Encode(const ADateTime : string): TDateTime;
var
  Y, M, D: Word;
  H, N, S, Z: Word;
begin
//  Result := StrToDateTimeDef(ADateTime, 0);
//  if Result = 0 then
//  begin
    //12|09|2010 13|46|17|666
    try
      D := StrToInt(Copy(ADateTime, 1, 2));
      M := StrToInt(Copy(ADateTime, 4, 2));
      Y := StrToInt(Copy(ADateTime, 7, 4));
    except
      Y := 1899;
      M := 12;
      D := 30;
    end;

    try
      H := StrToIntDef(Copy(ADateTime, 12, 2), 0);
      N := StrToIntDef(Copy(ADateTime, 15, 2), 0);
      S := StrToIntDef(Copy(ADateTime, 18, 2), 0);
      Z := StrToIntDef(Copy(ADateTime, 21, 3), 0);
    except
      H := 0;
      N := 0;
      S := 0;
      Z := 0;
    end;

    Result := EncodeDateTime(Y, M, D, H, N, S, Z);
//  end;
end;

{ TFloatConverter }

class function TFloatConverter.Decode(const AValue: Double): string;
var
  lInt, lFrac: Double;
  lIntStr, lFracStr: string;
begin
  try
    lInt := Int(AValue);
    lFrac := Frac(AValue);

    lIntStr := FloatToStr(lInt);
    lFracStr := FloatToStr(lFrac);
    Delete(lFracStr, 1, 2); // 0.
  except
    lIntStr := '0';
    lFracStr := '0';
  end;

  Result := lIntStr;
  if lFracStr <> '' then
    Result := Result + ',' + lFracStr;
end;

class function TFloatConverter.Encode(const AValue: string): Double;
var
  lFrac, lInt, lDiv: string;
  p: Integer;
begin
  try
    lFrac := AValue;
    p := Pos(',', lFrac); // O valor é salvo com "," nunca com "."

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

    Result := StrToInt64Def(lInt, 0) + (StrToInt64Def(lFrac, 0) / StrToInt64Def(lDiv, 0));
  except
    Result := 0;
  end;
end;

{ TURLConverter }

class function TURLConverter.DecodeURL(const AUrlValue: string): string;
const
  C_LIMITE = 28;
  C_OLD : array[0..C_LIMITE] of string = ('%C3', '%A0', '%A2', '%AA', '%B4', '%BB', '%A3',
                                   '%B5', '%A1', '%A9', '%AD', '%B3', '%BA', '%A7',
                                   '%BC', '%80', '%82', '%8A', '%94', '%9B', '%83',
                                   '%95', '%81', '%89', '%8D', '%93', '%9A', '%87',
                                   '%9C');


  C_NEW : array[0..C_LIMITE] of string = ('',  'à', 'â', 'ê', 'ô', 'û', 'ã', 'õ', 'á',
                               'é', 'í', 'ó', 'ú', 'ç', 'ü', 'À', 'Â', 'Ê', 'Ô',
                               'Û', 'Ã', 'Õ', 'Á', 'É', 'Í', 'Ó', 'Ú', 'Ç', 'Ü');
var
  i: Integer;
begin
  Result := AUrlValue;
  for i := 0 to C_LIMITE do
    Result := StringReplace(Result, C_OLD[i], C_NEW[i], [rfReplaceAll]);
end;

end.
