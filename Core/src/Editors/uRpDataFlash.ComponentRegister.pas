unit uRpDataFlash.ComponentRegister;

interface

uses
  Classes, DesignIntf, DesignEditors, ToolsAPI, SysUtils,
  uRpDataFlash.Components, uRpDataFlash.DataSet;

type
  TRpDataFlashProjectInfo = class
  public
    class function GetCurrentProject: IOTAProject;
  end;

  TRpDataFlashConexaoServerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TRpDataFlashCommandControllerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TRpDataFlashConexaoClientSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TRpDataFlashExecutorComandoSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  uRpDataFlash.ProxyFactory.ClientGenerator,
  uRpDataFlash.CommandControllerEditor,
  uRpDataFlash.DataSetProviderList,
  uRpDataFlash.ExecutorCommandList,
  uRpDataFlash.Types,
  uRpDataFlash.DataSetProvider,
  uRpDataFlash.EditorComandosProviderRegister,
  uRpDataFlash.CommandExecutor,
  uRpDataFlash.EditorComandosSemAutenticacao;

procedure Register;
begin
  RegisterComponents('RpDataFlash - Comunicação TCP',
    [TRpDataFlashServerConnection,
     TRpDataFlashClientConnection,
     TRpDataFlashRESTClient,
     TRpDataFlashCommandController,
     TRpDataFlashDataSet,
     TRpDataFlashDataSetProvider,
     TRpDataFlashDataSetFormatter,
     TRpDataFlashCommandExecutor]);

  RegisterComponentEditor(TRpDataFlashClientConnection, TRpDataFlashConexaoClienteEditor);
  RegisterComponentEditor(TRpDataFlashDataSetProvider, TRpDataFlashEditorComandosProvider);
  RegisterComponentEditor(TRpDataFlashCommandController, TRpDataFlashComandControllerEditor);

  RegisterPropertyEditor(TypeInfo(String), TRpDataFlashDataSet, 'ProviderClass', TRpDataFlashDataSetProviderList);
  RegisterPropertyEditor(TypeInfo(String), TRpDataFlashCommandExecutor, 'Command', TRpDataFlashExecutorComandoList);
  RegisterPropertyEditor(TypeInfo(String), TRpDataFlashServerConnection, 'ComandosSemAutenticacao', TRpDataFlashEditComandosSemAutenticacao);

  // units inseridas com o Servidor
  RegisterSelectionEditor(TRpDataFlashServerConnection, TRpDataFlashConexaoServerSelectionEditor);

  // units inseridas com o Client
  RegisterSelectionEditor(TRpDataFlashClientConnection, TRpDataFlashConexaoClientSelectionEditor);
  RegisterSelectionEditor(TRpDataFlashRESTClient,    TRpDataFlashConexaoClientSelectionEditor);

  // units inseridas com o CommandController
  RegisterSelectionEditor(TRpDataFlashCommandController, TRpDataFlashCommandControllerSelectionEditor);

  // units inseridas com o ExecutorComando
  RegisterSelectionEditor(TRpDataFlashCommandExecutor, TRpDataFlashExecutorComandoSelectionEditor);
end;

{ TRpDataFlashProjectInfo }

class function TRpDataFlashProjectInfo.GetCurrentProject: IOTAProject;
var
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;
  ModServices := BorlandIDEServices as IOTAModuleServices;
  if ModServices <> nil then
  begin
    for i := 0 to ModServices.ModuleCount - 1 do
    begin
      Module := ModServices.Modules[i];
      if Supports(Module, IOTAProjectGroup, ProjectGroup) then
      begin
        Result := ProjectGroup.ActiveProject;
        Exit;
      end
      else if Supports(Module, IOTAProject, Project) then
      begin // In the case of unbound packages, return the 1st
        if Result = nil then
          Result := Project;
      end;
    end;
  end;
end;

{ TRpDataFlashComandosControllerEditor }

//function TRpDataFlashComandosControllerEditor.GetAttributes: TPropertyAttributes;
//begin
//   Result := inherited GetAttributes + [paSubProperties];
//end;
//
//procedure TRpDataFlashComandosControllerEditor.GetValues(Proc: TGetStrProc);
//var
//  lView: TfrmComandosControllerView;
//begin
//  inherited;
//  lView := TfrmComandosControllerView.Create(nil);
//  lView.ShowModal;
//  lView.Free;
//end;

{ TRpDataFlashConexaoServerSelectionEditor }

procedure TRpDataFlashConexaoServerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  // force the provider and proxy methods on the server unit place
  Proc('uRpDataFlash.Types');
  Proc('uRpDataFlash.ProxyGenerator');
  Proc('uRpDataFlash.CommandHelper');
end;

{ TRpDataFlashCommandControllerSelectionEditor }

procedure TRpDataFlashCommandControllerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uRpDataFlash.Command');
end;

{ TRpDataFlashConexaoClientSelectionEditor }

procedure TRpDataFlashConexaoClientSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uRpDataFlash.Types');
end;

{ TRpDataFlashExecutorComandoSelectionEditor }

procedure TRpDataFlashExecutorComandoSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
begin
  inherited;
  // force the provider and proxy methods on the server unit place
  Proc('uRpDataFlash.Types');
  Proc('uRpDataFlash.ProxyGenerator');
  Proc('uRpDataFlash.CommandHelper');
end;

end.
