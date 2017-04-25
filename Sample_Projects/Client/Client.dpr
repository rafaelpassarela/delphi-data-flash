program Client;

uses
  Forms,
  uMainClient in 'uMainClient.pas' {FormMainClient},
  uGeneratedProxy in 'uGeneratedProxy.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True; //DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'LRDataFlash Client';
  Application.CreateForm(TFormMainClient, FormMainClient);
  Application.Run;
end.
