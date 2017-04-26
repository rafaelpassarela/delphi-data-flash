program SerializationDemo;

uses
  Forms,
  uFormSerializationDemo in 'uFormSerializationDemo.pas' {FormSerializationDemo},
  uSerializationModel in 'uSerializationModel.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Serialization Demo';
  Application.CreateForm(TFormSerializationDemo, FormSerializationDemo);
  Application.Run;
end.
