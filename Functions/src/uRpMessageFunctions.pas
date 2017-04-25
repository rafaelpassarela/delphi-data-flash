unit uRpMessageFunctions;

{$I ..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  Winapi.Windows, Vcl.Forms, System.SysUtils;
  {$ELSE}
  Windows, Forms, SysUtils;
  {$ENDIF}

type
  TRpMessageFunctions = class
  protected
    class function VerificarQuebraLinha(const AMessage : string) : string;
  public
    class procedure Error(const AError : string); virtual;
    class procedure ErrorFmt(const AError : string; const Args : array of const);

    class procedure Information(const AInfo : string); virtual;
    class procedure InformationFmt(const AInfo : string; const Args : array of const);

    class procedure Warning(const AWarning : string); virtual;
    class procedure WarningFmt(const AWarning : string; const Args : array of const);

    class function Question(const AQuestion : string) : Boolean; virtual;
    class function QuestionFmt(const AQuestion : string; const Args : array of const) : Boolean;
  end;

implementation

{ TRpMessageFunctions }

class procedure TRpMessageFunctions.Error(const AError: string);
begin
  Application.MessageBox(PChar( VerificarQuebraLinha(AError)), 'Erro', MB_OK + MB_ICONERROR);
end;

class procedure TRpMessageFunctions.ErrorFmt(const AError: string;
  const Args: array of const);
begin
  TRpMessageFunctions.Error( Format(AError, Args) );
end;

class procedure TRpMessageFunctions.Information(const AInfo: string);
begin
  Application.MessageBox(PChar(VerificarQuebraLinha(AInfo)), 'Atenção', MB_OK + MB_ICONINFORMATION);
end;

class procedure TRpMessageFunctions.InformationFmt(const AInfo: string;
  const Args: array of const);
begin
  TRpMessageFunctions.Information( Format(AInfo, Args) );
end;

class function TRpMessageFunctions.Question(const AQuestion: string): Boolean;
begin
  Result := Application.MessageBox(PChar(VerificarQuebraLinha(AQuestion)), 'Atenção', MB_YESNO + MB_ICONQUESTION) = IDYES;
end;

class function TRpMessageFunctions.QuestionFmt(const AQuestion: string;
  const Args: array of const): Boolean;
begin
  Result := TRpMessageFunctions.Question( Format(AQuestion, Args) );
end;

class procedure TRpMessageFunctions.Warning(const AWarning: string);
begin
  Application.MessageBox(PChar(VerificarQuebraLinha(AWarning)), 'Atenção', MB_OK + MB_ICONWARNING);
end;

class procedure TRpMessageFunctions.WarningFmt(const AWarning: string;
  const Args: array of const);
begin
  TRpMessageFunctions.Warning( Format(AWarning, Args) );
end;

class function TRpMessageFunctions.VerificarQuebraLinha(
  const AMessage: string): string;
begin
  Result := StringReplace(AMessage, '\n', sLineBreak, [rfReplaceAll]);
end;

end.
