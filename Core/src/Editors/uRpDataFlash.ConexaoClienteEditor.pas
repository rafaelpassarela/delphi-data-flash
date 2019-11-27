unit uRpDataFlash.ConexaoClienteEditor;

interface

uses
  DesignEditors, DBClient, Classes, uRpDataFlash.ProxyFactory.CmdSelector, Controls,
  IniFiles, StrUtils, uRpDataFlash.Types;

type
  TRpDataFlashConexaoClienteEditor = class(TComponentEditor)
  private
    FCdsComandos : TClientDataSet;
    FListaSelecionados : TStrings;
    FNomeUnitProxy: string;

    FFileControlClass : string;
    FFileControlConfigUnit : string;

    FUsarClasseTransporte : Boolean;

//    FClassPrefix : string;
//    FClassUnit : string;

//    FDirProxyPHP: string;
//    FDirProxyJAVA: string;
    procedure GerarProxy;
    function ShowFormSelecao(const AConfigFileName : string) : Boolean;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  Windows,
  ShellAPI,
  uRpDataFlash.Command,
  uRpDataFlash.ProxyGenerator,
  uRpDataFlash.Components,
  uRpDataFlash.ComponentRegister;

{ TRpDataFlashConexaoClienteEditor }

procedure TRpDataFlashConexaoClienteEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0 : GerarProxy;
  end;
end;

procedure TRpDataFlashConexaoClienteEditor.GerarProxy;
var
  lClassesProxy: string;
  lCmdListaClasses: TRpDataFlashComandoList;
  lArquivoProxy : TStringList;
  lArquivo: string;
  lNomeArquivo: string;
  lSaveDlg : TSaveDialog;
  lContinua: Boolean;
  lConfigFileName: string;
  lNomeUnitClasse: string;
begin
  FListaSelecionados := TStringList.Create;
  lCmdListaClasses := TRpDataFlashComandoList.Create;

  lSaveDlg := TSaveDialog.Create(nil);
  lSaveDlg.Filter := 'Delphi Source File|*.pas';
  lSaveDlg.DefaultExt := '*.pas';
  try
    FCdsComandos := TClientDataSet.Create(nil);
    // chama o comando solicitando a lista de comandos disponiveis no servidor
    lCmdListaClasses.Param['TipoBusca'].AsInteger := Ord(trpFactoryList);
    (Component as TRpDataFlashClientConnection).Comunicar(lCmdListaClasses);

    FCdsComandos.XMLData := lCmdListaClasses.ResultParam['RetornoProxy'].AsBase64;

    // prepara a janela pasa selecao dos comandos
    lConfigFileName := StringReplace(TRpDataFlashProjectInfo.GetCurrentProject.FileName, '.dproj', '.tcpconf', [rfIgnoreCase]);
    if ShowFormSelecao(lConfigFileName) and (FListaSelecionados.Count > 0) then
    begin
      lCmdListaClasses.Param['TipoBusca'].AsInteger := Ord(trpFactory);

      FListaSelecionados.Insert(0, 'CONFIG=' + FFileControlConfigUnit + '|' + FFileControlClass);
      FListaSelecionados.Insert(1, 'TRANSP=' + IfThen(FUsarClasseTransporte, 'T', 'F'));

      lCmdListaClasses.Param['InfoString'].AsBase64 := FListaSelecionados.Text;

      if Component is TRpDataFlashRESTClient then
        (Component as TRpDataFlashRESTClient).Comunicar(lCmdListaClasses)
      else
        (Component as TRpDataFlashClientConnection).Comunicar(lCmdListaClasses);

      lClassesProxy := lCmdListaClasses.ResultParam['RetornoProxy'].AsBase64;
      lNomeArquivo := lCmdListaClasses.ResultParam['NomeArquivoProxy'].AsString;

      lArquivoProxy := TStringList.Create;
      try
        lArquivoProxy.Text := lClassesProxy;

        if (FNomeUnitProxy <> EmptyStr) or (lSaveDlg.Execute) then
        begin
          if FNomeUnitProxy = EmptyStr then
            FNomeUnitProxy := lSaveDlg.FileName;

          if FileExists(FNomeUnitProxy) then
            lContinua := MessageDlg('O arquivo já existe. Deseja continuar?'#10#13 + FNomeUnitProxy, mtWarning, mbYesNo, 0) = idYes
          else
            lContinua := True;

          if lContinua then
          begin
            lArquivo := FNomeUnitProxy;
            // corrige o nome da unit dentro do arquivo
            lArquivoProxy.Text := StringReplace(lArquivoProxy.Text, lNomeArquivo,
              StringReplace(ExtractFileName(lArquivo), '.pas', '', [rfIgnoreCase]), [rfIgnoreCase] );

            // corrige o noem da unit de classe
            if FUsarClasseTransporte then
            begin
              lNomeUnitClasse := StringReplace(ExtractFileName(lArquivo), '.pas', 'Class', [rfIgnoreCase]);
              lArquivoProxy.Text := StringReplace(lArquivoProxy.Text, C_TMP_UNIT_CLASS, lNomeUnitClasse, [rfIgnoreCase]);
            end;

            lArquivoProxy.SaveToFile(lArquivo);

            with TIniFile.Create(lConfigFileName) do
            try
              WriteString('config', 'proxy.filename', lArquivo);
            finally
              Free;
            end;

            // verifica se o proxy existe no projeto
            if (TRpDataFlashProjectInfo.GetCurrentProject.FindModuleInfo(lArquivo) = nil) then
              TRpDataFlashProjectInfo.GetCurrentProject.AddFile(lArquivo, True);

            ShellExecute(HInstance, 'open', PChar(lArquivo), nil, nil, SW_SHOWNORMAL);

            if FUsarClasseTransporte then
            begin
              lArquivo := StringReplace(lArquivo, '.pas', 'Class.pas', [rfIgnoreCase]);
              lArquivoProxy.Text := StringReplace(
                lCmdListaClasses.ResultParam['RetornoClass'].AsBase64,
                C_TMP_UNIT_CLASS, lNomeArquivo, [rfIgnoreCase]);
              lArquivoProxy.SaveToFile(lArquivo);

              // verifica se o proxy existe no projeto
              if (TRpDataFlashProjectInfo.GetCurrentProject.FindModuleInfo(lArquivo) = nil) then
                TRpDataFlashProjectInfo.GetCurrentProject.AddFile(lArquivo, True);
              ShellExecute(HInstance, 'open', PChar(lArquivo), nil, nil, SW_SHOWNORMAL);
            end;

            // verifica se o arquivo de configuracao existe no projeto
            if (TRpDataFlashProjectInfo.GetCurrentProject.FindModuleInfo(lConfigFileName) = nil) then
              TRpDataFlashProjectInfo.GetCurrentProject.AddFile(lConfigFileName, False);
          end;
        end;
      finally
        lArquivoProxy.Free;
      end;
    end;
  finally
    if not (Component is TRpDataFlashRESTClient) then
      (Component as TRpDataFlashClientConnection).Disconnect;

    lCmdListaClasses.Free;
    FreeAndNil(FListaSelecionados);
    FreeAndNil(FCdsComandos);
    FreeAndNil(lSaveDlg);
  end;
end;

function TRpDataFlashConexaoClienteEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0 : Result := 'Gerar Classes Proxy';
    else
      raise Exception.Create('Índice ' + IntToStr(Index) + ' não suportado !');
  end;
end;

function TRpDataFlashConexaoClienteEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TRpDataFlashConexaoClienteEditor.ShowFormSelecao(const AConfigFileName : string): Boolean;
var
  lFormProxyConfig : TFormLRDataFlashProxyGenerator;
begin
//  FDirProxyJAVA := '';
//  FDirProxyPHP := '';
  FNomeUnitProxy := '';
  FUsarClasseTransporte := False;

  lFormProxyConfig := TFormLRDataFlashProxyGenerator.Create(nil);
  try
    lFormProxyConfig.InitCommandList( FCdsComandos, AConfigFileName );
    Result := lFormProxyConfig.ShowModal = mrOk;
    if Result then
    begin
      lFormProxyConfig.GetListaSelecionados(FListaSelecionados);
      FNomeUnitProxy := lFormProxyConfig.EditProxyName.Text;

      FFileControlClass := lFormProxyConfig.EditClassName.Text;
      FFileControlConfigUnit := lFormProxyConfig.EditUnitConfig.Text;

      FUsarClasseTransporte := lFormProxyConfig.CheckBoxUsarBase.Checked;

//      FClassPrefix := lFormProxyConfig.EditBaseName.Text;
//      FClassUnit := lFormProxyConfig.EditUnitBaseClass.Text;
    end;
  finally
    FreeAndNil(lFormProxyConfig);
  end;
end;

end.
