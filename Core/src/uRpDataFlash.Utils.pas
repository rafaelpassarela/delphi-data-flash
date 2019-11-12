unit uRpDataFlash.Utils;

interface

uses
  IdStackWindows, SysUtils;

type
  TRpDataFlashUtils = class
  public
    class function GetLocalComputerName : string;
    class function GetLocalComputerIp : string;
  end;

  TRpDataFlashValidations = class
  protected
    class function RemoveInvalidChars(const Value : string) : string;
  public
    class function NameValidation(const Value : string) : string;
  end;

implementation

{ TRpDataFlashUtils }

class function TRpDataFlashUtils.GetLocalComputerIp: string;
var
  lStackWin : TIdStackWindows;
begin
  lStackWin := TIdStackWindows.Create;
  try
    Result := lStackWin.LocalAddress;
  finally
    lStackWin.Free;
  end;
end;

class function TRpDataFlashUtils.GetLocalComputerName: string;
var
  lStackWin : TIdStackWindows;
begin
  lStackWin := TIdStackWindows.Create;
  try
    Result := lStackWin.HostByAddress(lStackWin.LocalAddress);
  finally
    lStackWin.Free;
  end;
end;

{ TRpDataFlashValidations }

class function TRpDataFlashValidations.NameValidation(
  const Value: string): string;
begin
  Result := RemoveInvalidChars(Value);
end;

class function TRpDataFlashValidations.RemoveInvalidChars(
  const Value: string): string;
var
  i : Integer;
begin;
  Result := EmptyStr;
  for i := 1 to Length(Value) do
  {$IFDEF UNICODE}
    if CharInSet(Value[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
  {$ELSE}
    if Value[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
  {$ENDIF}
      Result := Result + Value[i];

end;

end.
