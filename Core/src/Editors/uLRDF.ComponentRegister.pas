unit uLRDF.ComponentRegister;

interface

uses
  Classes, DesignIntf, DesignEditors, ToolsAPI, SysUtils,
  uLRDF.Component, uLRDF.DataSet;

type
  TLRDataFlashProjectInfo = class
  public
    class function GetCurrentProject: IOTAProject;
  end;

  TTLRDataFlashConexaoServerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TTLRDataFlashCommandControllerSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TTLRDataFlashConexaoClientSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TTLRDataFlashExecutorComandoSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  uLRDF.ConexaoClienteEditor,
  uLRDF.ComandControllerEditor,
  uLRDF.DataSetProviderList,
  uLRDF.ExecutorComandoList,
  uLRDF.Types,
  uLRDF.DataSetProvider,
  uLRDF.EditorComandosProvider,
  uLRDF.ExecutorComandos,
  fLRDF.EditorComandosSemAutenticacao;

procedure Register;
begin
  RegisterComponents('LRDataFlash - Comunicação TCP',
    [TLRDataFlashConexaoServer,
     TLRDataFlashConexaoCliente,
     TLRDataFlashConexaoREST,
     TLRDataFlashComandController,
     TLRDataFlashDataSet,
     TLRDataFlashDataSetProvider,
     TLRDataFlashDataSetFormatter,
     TLRDataFlashExecutorComando]);

  RegisterComponentEditor(TLRDataFlashConexaoCliente, TLRDataFlashConexaoClienteEditor);
  RegisterComponentEditor(TLRDataFlashComandController, TLRDataFlashComandControllerEditor);
  RegisterComponentEditor(TLRDataFlashDataSetProvider, TLRDataFlashEditorComandosProvider);

  RegisterPropertyEditor(TypeInfo(String), TLRDataFlashDataSet, 'ProviderClass', TLRDataFlashDataSetProviderList);
  RegisterPropertyEditor(TypeInfo(String), TLRDataFlashExecutorComando, 'Comando', TLRDataFlashExecutorComandoList);
  RegisterPropertyEditor(TypeInfo(String), TLRDataFlashConexaoServer, 'ComandosSemAutenticacao', TLRDataFlashEditComandosSemAutenticacao);

  // units inseridas com o Servidor
  RegisterSelectionEditor(TLRDataFlashConexaoServer, TTLRDataFlashConexaoServerSelectionEditor);

  // units inseridas com o Client
  RegisterSelectionEditor(TLRDataFlashConexaoCliente, TTLRDataFlashConexaoClientSelectionEditor);
  RegisterSelectionEditor(TLRDataFlashConexaoREST,    TTLRDataFlashConexaoClientSelectionEditor);

  // units inseridas com o CommandController
  RegisterSelectionEditor(TLRDataFlashComandController, TTLRDataFlashCommandControllerSelectionEditor);

  // units inseridas com o ExecutorComando
  RegisterSelectionEditor(TLRDataFlashExecutorComando, TTLRDataFlashExecutorComandoSelectionEditor);
end;

{ TLRDataFlashProjectInfo }

class function TLRDataFlashProjectInfo.GetCurrentProject: IOTAProject;
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

{ TLRDataFlashComandosControllerEditor }

//function TLRDataFlashComandosControllerEditor.GetAttributes: TPropertyAttributes;
//begin
//   Result := inherited GetAttributes + [paSubProperties];
//end;
//
//procedure TLRDataFlashComandosControllerEditor.GetValues(Proc: TGetStrProc);
//var
//  lView: TfrmComandosControllerView;
//begin
//  inherited;
//  lView := TfrmComandosControllerView.Create(nil);
//  lView.ShowModal;
//  lView.Free;
//end;

{ TTLRDataFlashConexaoServerSelectionEditor }

procedure TTLRDataFlashConexaoServerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  // force the provider and proxy methods on the server unit place
  Proc('uLRDF.Types');
  Proc('uLRDF.ProxyGenerator');
  Proc('uLRDF.ComandoHelper');
end;

{ TTLRDataFlashCommandControllerSelectionEditor }

procedure TTLRDataFlashCommandControllerSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uLRDF.Comando');
end;

{ TTLRDataFlashConexaoClientSelectionEditor }

procedure TTLRDataFlashConexaoClientSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  inherited;
  Proc('uLRDF.Types');
end;

{ TTLRDataFlashExecutorComandoSelectionEditor }

procedure TTLRDataFlashExecutorComandoSelectionEditor.RequiresUnits(
  Proc: TGetStrProc);
begin
  inherited;
  // force the provider and proxy methods on the server unit place
  Proc('uLRDF.Types');
  Proc('uLRDF.ProxyGenerator');
  Proc('uLRDF.ComandoHelper');
end;

end.
