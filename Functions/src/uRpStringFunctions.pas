unit uRpStringFunctions;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  TypInfo, StrUtils, uRpResourceString,
  {$IFDEF XE3UP}
    {$IFDEF ANDROID}

    {$ELSE}
      Vcl.Graphics, Vcl.Controls, Winapi.Windows, Vcl.Forms,
    {$ENDIF}
  System.Types, System.SysUtils, System.Classes;
  {$ELSE}
  Graphics, Types, Controls, SysUtils, Windows, Classes, Forms;
  {$ENDIF}

type
  TRpStrings = class
  private
    class function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
    class procedure SetOrdValue(Info: PTypeInfo; var SetParam; Value: Integer);
  public
    class function FisrtUpper(const AValue : string) : string;
    class function RemoveAccent(const AValue : string) : string;
    {$IFNDEF ANDROID}
    class function CharSize(Canvas: TCanvas): TPoint;
    class function TextWidth(const pCanvas: TCanvas; const pFont : TFont;
      const pMens : string) : TRect;
    {$ENDIF}

    class procedure ResetLength(var S : string);

    class function EnumToStr(const ATypInfo : PTypeInfo; const AEnumOrdValue : Cardinal) : string;
    class function StrToEnumOrd(const ATypInfo : PTypeInfo; const AEnumName : string) : Integer;
    class function SetToString(Info: PTypeInfo; const SetParam; Brackets: Boolean = True): string;
    {$IFNDEF ANDROID}
    class procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: string);
    {$ENDIF}

    class function InsertWhereCondition(const ASQL, AWhereCondition : string) : string;
    class function StringReplaceWholeWord(const AText, ASearchText, AReplaceText: string;
      const AReplaceFlags: TReplaceFlags): String;
  end;

implementation

{ TRpStrings }

{$IFNDEF ANDROID}
class function TRpStrings.CharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do
    Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do
    Buffer[I + 26] := Chr(I + Ord('a'));

  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;
{$ENDIF}

class function TRpStrings.EnumToStr(const ATypInfo: PTypeInfo; const AEnumOrdValue: Cardinal): string;
begin
  Result := GetEnumName(ATypInfo, AEnumOrdValue);
end;

class function TRpStrings.FisrtUpper(const AValue: string): string;
var
  I: Integer;
  lStr: string;
begin
  lStr := AnsiLowerCase(AValue);
  for I := 1 to Length(lStr) do
  begin
    if I = 1 then
      lStr[I] := AnsiUpperCase(lStr[1])[1]
    else
      if CharInSet(lStr[I-1], [' ', #10, #13]) then
        if Pos( Copy(lStr, I, Pos(' ', Copy(lStr, I + 1, Length(lStr)))), R_FIRST_UPPER_EXCPTION) = 0 then
          lStr[I] := AnsiUpperCase(lStr[I])[1];
  end;

  lStr := StringReplace(lStr,' Iii',' III', [rfReplaceAll]);
  lStr := StringReplace(lStr,' Ii',' II', [rfReplaceAll]);
  Result := lStr;
end;

class function TRpStrings.GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Result := Byte(SetParam);
    otSWord, otUWord:
      Result := Word(SetParam);
    otSLong, otULong:
      Result := Integer(SetParam);
  end;
end;

class function TRpStrings.InsertWhereCondition(const ASQL, AWhereCondition: string): string;
var
  lSqlTratado: string;
  lWhere: string;

  function LastPos(const SubStr: String; const S: String): Integer;
  begin
     Result := Pos(ReverseString(SubStr), ReverseString(S)) ;

     if (Result <> 0) then
       Result := ((Length(S) - Length(SubStr)) + 1) - Result + 1;
  end;

  function InsertCondition(const pTag : string) : Boolean;
  var
    lPos: Integer;
    lLenTag: Integer;
  begin
    lLenTag := Length(pTag) + 1;
    lPos := LastPos(pTag, UpperCase(lSqlTratado));
    Result := lPos > 0;
    if Result then
    begin
      if pTag = 'WHERE' then
        System.Insert(lWhere + ' and ', lSqlTratado, lPos + lLenTag)
      else
        System.Insert(' where ' + lWhere, lSqlTratado, lPos - 1)
    end;
  end;

begin
  lSqlTratado := ASQL;
  lWhere := '(' + AWhereCondition + ')';

  // se nao tem nehuma das tags do sql, adiciona no final
  if  (not InsertCondition('WHERE')) and (not InsertCondition('ORDER BY'))
  and (not InsertCondition('GROUP BY')) then
    lSqlTratado := lSqlTratado + ' WHERE ' + lWhere;

  Result := lSqlTratado;
end;

class function TRpStrings.RemoveAccent(const AValue: string): string;
const
  ComAcento = '‡‚ÍÙ˚„ı·ÈÌÛ˙Á¸¿¬ ‘€√’¡…Õ”⁄«‹<>!?';
  SemAcento = 'aaeouaoaeioucuAAEOUAOAEIOUCU    ';
var
  x: Integer;
  lStr: string;
begin
  lStr := AValue;
  for x := 1 to Length(lStr) do
    if Pos(lStr[x], ComAcento) <> 0 then
      lStr[x] := SemAcento[Pos(lStr[x], ComAcento)];
  Result := lStr;
end;

class procedure TRpStrings.ResetLength(var S: string);
var
  i : Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = #0 then
    begin
      SetLength(S, i);
      Break;
    end;
end;

class procedure TRpStrings.SetOrdValue(Info: PTypeInfo; var SetParam;
  Value: Integer);
begin
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Byte(SetParam) := Value;
    otSWord, otUWord:
      Word(SetParam) := Value;
    otSLong, otULong:
      Integer(SetParam) := Value;
  end
end;

class function TRpStrings.SetToString(Info: PTypeInfo; const SetParam;
  Brackets: Boolean): string;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';

  Integer(S) := TRpStrings.GetOrdValue(Info, SetParam);
  TypeInfo := GetTypeData(Info)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;

  if Brackets then
    Result := '[' + Result + ']';
end;

{$IFNDEF ANDROID}
class function TRpStrings.StringReplaceWholeWord(const AText, ASearchText, AReplaceText: string;
  const AReplaceFlags: TReplaceFlags): String;
const
  C_SEPARATORS : set of AnsiChar = [' ', '.', ',', '?', '!',#13, #10, #09, '(', ')'];

var
  lStartPos, lEndPos : Integer;
  lLeft, lRight : WideString;

  function isWordThere(const AText, AWord: string; AReplaceFlags: TReplaceFlags;
    var AStartPos, AEndPos: Integer) : Boolean;
  var
     Before, After: boolean;
  begin
    Result:= false;
    AStartPos := 0;
    while (not Result) do
    begin
      Inc(AStartPos);
      if (rfIgnoreCase in AReplaceFlags) then
        AStartPos := PosEx(lowercase(AWord), Lowercase(AText), AStartPos)
      else
        AStartPos := PosEx(AWord, AText, AStartPos);

      if AStartPos = 0 then
        Exit;

      AEndPos := AStartPos + Length(AWord) -1;
      {$IFDEF UNICODE}
      if (AStartPos = 1) or (CharInSet(AText[AStartPos-1], C_SEPARATORS)) then
      {$ELSE}
      if (AStartPos = 1) or ((Text[AStartPos-1] in C_SEPARATORS)) then
      {$ENDIF}
        Before := true
      else
        Before := false;

      {$IFDEF UNICODE}
      if (AEndPos = Length(AText)) or CharInSet(AText[AEndPos+1], C_SEPARATORS) then
      {$ELSE}
      if (AEndPos = Length(AText)) or (AText[AEndPos+1] in C_SEPARATORS) then
      {$ENDIF}
        After := True
      else
        After := False;

      Result := Before and After;
    end;
  end;

begin
  Result := AText;
  if not isWordThere(AText, ASearchText, AReplaceFlags, lStartPos, lEndPos) then
    Exit;

  lLeft := LeftStr(AText, lStartPos-1);
  lRight := RightStr(AText, Length(AText)-lEndPos);

  if rfReplaceAll in AReplaceFlags then
    lRight := StringReplaceWholeWord(lRight, ASearchText, AReplaceText, AReplaceFlags);

  Result := lLeft + AReplaceText + lRight;
end;

class procedure TRpStrings.StringToSet(Info: PTypeInfo; var SetParam;
  const Value: string);
var
  P: PAnsiChar;
  EnumInfo: PTypeInfo;
  EnumName: string;
  EnumValue, SetValue: Longint;

  function NextWord(var P: PAnsiChar) : AnsiString;
  var
    I: Integer;
  begin
    I := 0;
    // scan til whitespace
    while not (P[I] in [',', ' ', #0,']']) do
      Inc(I);
    SetString(Result, P, I);
    // skip whitespace
    while P[I] in [',', ' ',']'] do
      Inc(I);
    Inc(P, I);
  end;

begin
  SetOrdValue(Info, SetParam, 0);
  if Value = '' then
    Exit;

  SetValue := 0;
  P := PAnsiChar( AnsiString(Value) );
  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);
  EnumInfo := GetTypeData(Info)^.CompType^;
  EnumName := string(NextWord(P));
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue >= 0 then
      Include(TIntegerSet(SetValue), EnumValue);
    EnumName := string( NextWord(P) );
  end;
  SetOrdValue(Info, SetParam, SetValue);
end;
{$ENDIF}

class function TRpStrings.StrToEnumOrd(const ATypInfo: PTypeInfo;
  const AEnumName: string): Integer;
begin
  Result := GetEnumValue(ATypInfo, AEnumName);
end;

{$IFNDEF ANDROID}
class function TRpStrings.TextWidth(const pCanvas: TCanvas; const pFont: TFont;
  const pMens: string): TRect;
begin
  pCanvas.Font.Assign( pFont );

  SetRect(Result, 0, 0, Screen.Width div 2, 0);

  DrawText(
    pCanvas.Handle,
    PChar( pMens ),
    Length( pMens ) + 1,
    Result,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or {DrawTextBiDiModeFlagsReadingOnly} 0);
end;
{$ENDIF}

end.
