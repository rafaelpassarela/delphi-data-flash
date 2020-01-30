unit uRpDateTimeFunctions;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
    System.SysUtils, System.DateUtils,
    {$IFDEF ANDROID}

    {$ELSE}
      Winapi.Windows,
    {$ENDIF}
  {$ELSE}
    TypInfo, Graphics, Types, Controls, SysUtils, Windows, Classes, Forms,
    DateUtils, StrUtils,
  {$ENDIF}
  uRpResourceString;

type
  TRpDateTime = class
  public
    class function StartDateOfMonth(const ADate : TDateTime) : TDate;
    class function StartDateOfMonthStr(const ADate : TDateTime) : string;

    class function EndDateOfMonth(const ADate : TDateTime) : TDate;
    class function EndDateOfMonthStr(const ADate : TDateTime) : string;

    class function StartDateOfWeek(const ADate : TDateTime) : TDate;
    class function StartDateOfWeekStr(const ADate: TDateTime) : string;

    class function EndDateOfWeek(const ADate : TDateTime) : TDate;
    class function EndDateOfWeekStr(const ADate : TDateTime) : string;

    class function StrToDateFmt(const AFormat, AValue : string; const ADefault : TDateTime) : TDateTime;
  end;

implementation

{ TRpDateTime }

class function TRpDateTime.StartDateOfMonthStr(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(R_DATE_FORMAT, TRpDateTime.StartDateOfMonth(ADate));
end;

class function TRpDateTime.StartDateOfWeek(const ADate: TDateTime): TDate;
var
  i : Word;
begin
  i := DayOfWeek(ADate);
  Result := IncDay(ADate, (-i + 1) );
end;

class function TRpDateTime.StartDateOfWeekStr(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(R_DATE_FORMAT, TRpDateTime.StartDateOfWeek(ADate));
end;

class function TRpDateTime.StrToDateFmt(const AFormat, AValue: string; const ADefault: TDateTime): TDateTime;
var
  i,ii,iii : integer;
  Tmp, Fmt,Data,Mask,Spec : string;
  Year,Month,Day,Hour,
  Minute,Second,MSec : word;
  AmPm : integer;
var
  lFormatSettings : TFormatSettings;
begin
  {$IFDEF XE3UP}
    lFormatSettings := FormatSettings;
  {$ELSE}
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, lFormatSettings);
  {$ENDIF}

  if (Trim(AValue) = '') or (Trim(AFormat) = '') then
  begin
    Result := 0;
    Exit;
  end;

  Year := 1;
  Month := 1;
  Day := 1;
  Hour := 0;
  Minute := 0;
  Second := 0;
  MSec := 0;
  Fmt := UpperCase(AFormat);
  Data := UpperCase(AValue);
  i := 1;
  Mask := '';
  AmPm := 0;

  while i < length(Fmt) do
  begin
    if CharInSet(Fmt[i], ['A','P','D','M','Y','H','N','S','Z']) then
//    if Fmt[i] in ['A','P','D','M','Y','H','N','S','Z'] then
    begin
      // Start of a date specifier
      Mask  := Fmt[i];
      ii := i + 1;

      // Keep going till not valid specifier
      while true do
      begin
        if ii > length(Fmt) then
          Break; // End of specifier string
        Spec := Mask + Fmt[ii];

        if (Spec = 'DD') or (Spec = 'DDD') or (Spec = 'DDDD') or
           (Spec = 'MM') or (Spec = 'MMM') or (Spec = 'MMMM') or
           (Spec = 'YY') or (Spec = 'YYY') or (Spec = 'YYYY') or
           (Spec = 'HH') or (Spec = 'NN') or (Spec = 'SS') or
           (Spec = 'ZZ') or (Spec = 'ZZZ') or
           (Spec = 'AP') or (Spec = 'AM') or (Spec = 'AMP') or
           (Spec = 'AMPM') then
        begin
          Mask := Spec;
          inc(ii);
        end
        else
        begin
          // End of or Invalid specifier
          Break;
        end;
      end;

      // Got a valid specifier ? - evaluate it from data string
      if (Mask <> '') and (length(Data) > 0) then
      begin
        // Day 1..31
        if (Mask = 'DD') then
        begin
           Day := StrToIntDef(trim(copy(Data,1,2)),0);
           delete(Data,1,2);
        end;

        // Day Sun..Sat (Just remove from data string)
        if Mask = 'DDD' then
          delete(Data,1,3);

        // Day Sunday..Saturday (Just remove from data string LEN)
        if Mask = 'DDDD' then
        begin
          Tmp := copy(Data,1,3);
          for iii := 1 to 7 do
          begin
            if Tmp = Uppercase(copy(lFormatSettings.LongDayNames[iii],1,3)) then
            begin
              delete(Data,1,length(lFormatSettings.LongDayNames[iii]));
              Break;
            end;
          end;
        end;

        // Month 1..12
        if (Mask = 'MM') then
        begin
           Month := StrToIntDef(trim(copy(Data,1,2)),0);
           delete(Data,1,2);
        end;

        // Month Jan..Dec
        if Mask = 'MMM' then
        begin
          Tmp := copy(Data,1,3);
          for iii := 1 to 12 do
          begin
            if Tmp = Uppercase(copy(lFormatSettings.LongMonthNames[iii],1,3)) then
            begin
              Month := iii;
              delete(Data,1,3);
              Break;
            end;
          end;
        end;

        // Month January..December
        if Mask = 'MMMM' then
        begin
          Tmp := copy(Data,1,3);
          for iii := 1 to 12 do
          begin
            if Tmp = Uppercase(copy(lFormatSettings.LongMonthNames[iii],1,3)) then
            begin
              Month := iii;
              delete(Data,1,length(lFormatSettings.LongMonthNames[iii]));
              Break;
            end;
          end;
        end;

        // Year 2 Digit
        if Mask = 'YY' then
        begin
          Year := StrToIntDef(copy(Data,1,2),0);
          delete(Data,1,2);
          if Year < lFormatSettings.TwoDigitYearCenturyWindow then
            Year := (YearOf(Date) div 100) * 100 + Year
          else
            Year := (YearOf(Date) div 100 - 1) * 100 + Year;
        end;

        // Year 4 Digit
        if Mask = 'YYYY' then
        begin
          Year := StrToIntDef(copy(Data,1,4),0);
          delete(Data,1,4);
        end;

        // Hours
        if Mask = 'HH' then
        begin
           Hour := StrToIntDef(trim(copy(Data,1,2)),0);
           delete(Data,1,2);
        end;

        // Minutes
        if Mask = 'NN' then
        begin
           Minute := StrToIntDef(trim(copy(Data,1,2)),0);
           delete(Data,1,2);
        end;

        // Seconds
        if Mask = 'SS' then
        begin
           Second := StrToIntDef(trim(copy(Data,1,2)),0);
           delete(Data,1,2);
        end;

        // Milliseconds
        if (Mask = 'ZZ') or (Mask = 'ZZZ') then
        begin
           MSec := StrToIntDef(trim(copy(Data,1,3)),0);
           delete(Data,1,3);
        end;

        // AmPm A or P flag
        if (Mask = 'AP') then
        begin
           if Data[1] = 'A' then
             AmPm := -1
           else
             AmPm := 1;
           delete(Data,1,1);
        end;

        // AmPm AM or PM flag
        if (Mask = 'AM') or (Mask = 'AMP') or (Mask = 'AMPM') then
        begin
           if copy(Data,1,2) = 'AM' then
             AmPm := -1
           else
             AmPm := 1;
           delete(Data,1,2);
        end;

        Mask := '';
        i := ii;
      end
      else
        Break;
    end
    else
    begin
      // Remove delimiter from data string
      if length(Data) > 1 then
        delete(Data,1,1);
      inc(i);
    end;
  end;

  if AmPm = 1 then
    Hour := Hour + 12;

  TryEncodeDateTime(Year, Month, Day, Hour, Minute, Second, MSec, Result);
end;

class function TRpDateTime.EndDateOfMonth(const ADate: TDateTime): TDate;
var
  D, M, A : Word;
begin
  DecodeDate(ADate, A, M, D);
  D := MonthDays[IsLeapYear(A), M];
  Result := EncodeDate(A, M, D);
end;

class function TRpDateTime.EndDateOfMonthStr(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(R_DATE_FORMAT, TRpDateTime.EndDateOfMonth(ADate));
end;

class function TRpDateTime.EndDateOfWeek(const ADate: TDateTime): TDate;
var
  i : Word;
begin
  i := DayOfWeek(ADate);
  Result := IncDay(ADate, (7 - i) );
end;

class function TRpDateTime.EndDateOfWeekStr(const ADate: TDateTime): string;
begin
  Result := FormatDateTime(R_DATE_FORMAT, TRpDateTime.EndDateOfWeek(ADate));
end;

class function TRpDateTime.StartDateOfMonth(const ADate: TDateTime): TDate;
var
  D, M, Y : Word;
begin
  DecodeDate(ADate, Y, M, D);
  Result := EncodeDate(Y, M, 1);
end;

end.
