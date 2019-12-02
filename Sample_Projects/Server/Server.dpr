program Server;

uses
  Forms,
  uMainServer in 'uMainServer.pas' {FormMainServer},
  uDataModuleServer in 'uDataModuleServer.pas' {DataModuleServer: TDataModule},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True; //DebugHook <> 0;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DataFlash Server';
  Application.CreateForm(TFormMainServer, FormMainServer);
  Application.Run;
end.
