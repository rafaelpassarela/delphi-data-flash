unit uRpApplicationUtil;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils;
  {$ELSE}
  Windows, Messages, SysUtils;
  {$ENDIF}

type
  TRpApplicationUtil = class
  public
    class function HasParam(const AParamName : string) : Boolean;
  end;

implementation

{ TRpApplicationUtil }

class function TRpApplicationUtil.HasParam(const AParamName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do
  begin
    Result := AnsiUpperCase(ParamStr(i)) = AnsiUpperCase(AParamName);
    if Result then
      Break;
  end;
end;

end.
