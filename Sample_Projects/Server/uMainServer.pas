unit uMainServer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRpDataFlash.Types, uRpDataFlash.ProxyGenerator,
  uRpDataFlash.CommandHelper, uRpDataFlash.Command,
  uRpDataFlash.DataSetProvider, uRpDataFlash.Components, Vcl.AppEvnts;

type
  TComandoCodeInverter = class(TRpDataFlashCommand)
  protected
    function DoGetDescription: string; override;
    function DoExecute: Boolean; override;
    procedure DoRegisterParams; override;
  end;

  TComandoFavIco = class(TRpDataFlashCommand)
  protected
    procedure DoRegisterParams; override;
    function DoExecute: Boolean; override;
  end;

  TFormMainServer = class(TForm)
    RpDataFlashServerConnectionTeste: TRpDataFlashServerConnection;
    LabelNomeServer: TLabel;
    EditNomeServer: TEdit;
    LabelPorta: TLabel;
    EditPorta: TEdit;
    ButtonConectar: TButton;
    ButtonDesconectar: TButton;
    MemoLog: TMemo;
    ButtonVerLog: TButton;
    DFCControllerMath: TRpDataFlashCommandController;
    DFPCadastro_Pessoas: TRpDataFlashDataSetProvider;
    DFCControllerManipulaTexto: TRpDataFlashCommandController;
    DFCControllerFiles: TRpDataFlashCommandController;
    ApplicationEvents1: TApplicationEvents;
    CheckBoxAuthCli: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConectarClick(Sender: TObject);
    procedure ButtonDesconectarClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonVerLogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RpDataFlashServerConnectionTesteAuthenticateClient(
      Sender: TObject; const AConnectionItem: IAutenticationProvider;
      out AAutenticado: Boolean; out AErrorMessage: string);
    procedure RpDataFlashServerConnectionTesteNewLog(Sender: TObject;
      ALogType: TRpDataFlashServiceLog; const ALog: string;
      const AClientInfo: TRpDataFlashClientInfo);
    procedure RpDataFlashServerConnectionTesteClientConnection(Sender: TObject;
      const AConnectionItem: TRpDataFlashConnectionItem);
    function DFCControllerMathCommands0Execute(
      const AComando: IRpDataFlashCommandInterfaced): Boolean;
    function DFCControllerMathCommands1Execute(
      const AComando: IRpDataFlashCommandInterfaced): Boolean;
    function DFCControllerManipulaTextoCommands0Execute(
      const AComando: IRpDataFlashCommandInterfaced): Boolean;
    function DFCControllerFilesCommands1Execute(
      const AComando: IRpDataFlashCommandInterfaced): Boolean;
    function DFCControllerFilesCommands0Execute(
      const AComando: IRpDataFlashCommandInterfaced): Boolean;
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure CheckBoxAuthCliClick(Sender: TObject);
  private
    { Private declarations }
    FInternalLog: TStrings;
    procedure SetEnables(const Value : Boolean);
  public
    { Public declarations }
  end;

var
  FormMainServer: TFormMainServer;

implementation

uses uDataModuleServer;

{$R *.dfm}

procedure TFormMainServer.ButtonVerLogClick(Sender: TObject);
begin
  MemoLog.Lines.Assign( FInternalLog );
  FInternalLog.Clear;
end;

procedure TFormMainServer.CheckBoxAuthCliClick(Sender: TObject);
begin
  RpDataFlashServerConnectionTeste.AuthEnable := CheckBoxAuthCli.Checked;
end;

function TFormMainServer.DFCControllerFilesCommands0Execute(
  const AComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lFile : IRpFileProxy;
  lFileName: string;
  lPath : string;
begin
  lFile := TRpFileProxy.Create;
  lFileName := AComando.GetParams.Param['FileName'].AsString;
  lFileName := ExtractFileName(lFileName);

  lPath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
         + 'ServerFiles\';

  ForceDirectories(lPath);

  lFileName := lPath + lFileName;
  AComando.GetParams.Param['FileData'].SaveToFile( lFileName );
  AComando.GetParams.ResultParam['SavePath'].AsString := lFileName;

  Result := True;
end;

function TFormMainServer.DFCControllerFilesCommands1Execute(
  const AComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lNomeArquivo: string;
  lFile : TRpFileProxy;
begin
  lNomeArquivo := AComando.GetParams.Param['FileName'].AsString;
  lFile := TRpFileProxy.Create;
  try
    lFile.LoadFromFile( lNomeArquivo );
    AComando.GetParams.ResultParam['FileData'].AsBase64 := lFile.Save;
    Result := True;
  finally
    FreeAndNil( lFile );
  end;
end;

function TFormMainServer.DFCControllerManipulaTextoCommands0Execute(
  const AComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lText : string;
  lInv : string;
  i: Integer;
begin
  lText := AComando.GetParams.Param['Text'].AsString;
  lInv := EmptyStr;

  for i := 1 to Length(lText) do
    lInv := lText[i] + lInv;

  AComando.GetParams.ResultParam['Inverted'].AsString := lInv;

  Result := True;
end;

function TFormMainServer.DFCControllerMathCommands0Execute(
  const AComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lA, lB, lTotal : Double;
begin
  try
    // read input parans
    lA := AComando.GetParams.Param['A'].AsFloat;
    lB := AComando.GetParams.Param['B'].AsFloat;

    // execute the command event
    lTotal := lA + lB;

    // return the output value to client
    AComando.GetParams.ResultParam['X'].AsFloat := lTotal;
    Result := True;
  except
    // on any error, sinalize as "not executed"
    Result := False;
  end;
end;

function TFormMainServer.DFCControllerMathCommands1Execute(
  const AComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lA, lB, lTotal : Double;
begin
  try
    // read input parans
    lA := AComando.GetParams.Param['A'].AsFloat;
    lB := AComando.GetParams.Param['B'].AsFloat;

    // execute the command event
    lTotal := lA * lB;

    // return the output value to client
    AComando.GetParams.ResultParam['X'].AsFloat := lTotal;
    Result := True;
  except
    // on any error, sinalize as "not executed"
    Result := False;
  end;
end;

procedure TFormMainServer.ApplicationEvents1Exception(Sender: TObject; E: Exception);
begin
  MemoLog.Lines.BeginUpdate;
  try
    MemoLog.Lines.Add(Format('ERROR: [%s] %s', [E.ClassName, E.Message]));
  finally
    MemoLog.Lines.EndUpdate;
  end;
end;

procedure TFormMainServer.ButtonConectarClick(Sender: TObject);
begin
  try
    RpDataFlashServerConnectionTeste.ConfigTCPIP.Port := StrToInt64Def( EditPorta.Text, 8890 );
    EditPorta.Text := IntToStr(RpDataFlashServerConnectionTeste.ConfigTCPIP.Port);

    RpDataFlashServerConnectionTeste.Connect;
    if RpDataFlashServerConnectionTeste.Connected then
      SetEnables(False)
    else
      SetEnables(True);
  except
    on E: Exception do
    begin
      SetEnables(True);
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TFormMainServer.ButtonDesconectarClick(Sender: TObject);
begin
  RpDataFlashServerConnectionTeste.Disconnect;
  SetEnables(True);
end;

procedure TFormMainServer.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  RpDataFlashServerConnectionTeste.Disconnect;
end;

procedure TFormMainServer.FormCreate(Sender: TObject);
begin
  EditNomeServer.Text := RpDataFlashServerConnectionTeste.Server;
  FInternalLog := TStringList.Create;
  SetEnabled(True);
end;

procedure TFormMainServer.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FInternalLog);
end;

procedure TFormMainServer.RpDataFlashServerConnectionTesteAuthenticateClient(
  Sender: TObject; const AConnectionItem: IAutenticationProvider;
  out AAutenticado: Boolean; out AErrorMessage: string);
begin
  if CheckBoxAuthCli.Checked then
  begin
    AAutenticado := (AConnectionItem.UserName = 'TESTE') and (AConnectionItem.Password = '1234');
    if not AAutenticado then
      AErrorMessage := 'Usu�rio ou senha inv�lidos. Para login, utilize:'#10#13'Usu�rio = TESTE'#10#13'Senha = 1234';
  end else
  begin
    AAutenticado := True;
  end;
end;

procedure TFormMainServer.RpDataFlashServerConnectionTesteClientConnection(
  Sender: TObject; const AConnectionItem: TRpDataFlashConnectionItem);
begin
  AConnectionItem.Executor := (TDataModuleServer.Create(nil) as IRpPackageCommandExecutor);
end;

procedure TFormMainServer.RpDataFlashServerConnectionTesteNewLog(
  Sender: TObject; ALogType: TRpDataFlashServiceLog; const ALog: string;
  const AClientInfo: TRpDataFlashClientInfo);
var
  lLinha : string;
begin
  if Assigned(FInternalLog) then
  begin
    case ALogType of
      slOnConnection: lLinha := 'Connection: ';
      slOnDisconnection: lLinha := 'Disconnection: ';
      slOnSend: lLinha := 'Send: ';
      slOnReceive: lLinha := 'Receive: ';
      slOnError: lLinha := 'ERROR: ';
      slOnStatus: lLinha := 'Status: ';
      slOnBridge: lLinha := 'Brigde: ';
      slOnCommand: lLinha := 'Command: ';
      slOnApplyRule: lLinha := 'ApplyRule: ';
      slOnFile: lLinha := 'File: ';
      slOnSync: lLinha := 'Sync: ';
      slOnSyncXml: lLinha := 'Sync XML: ';
    end;

    lLinha := lLinha + '[ ' + AClientInfo.DisplayName + ' / ' + AClientInfo.IP + ' ] -> ' + ALog;

    FInternalLog.Add( lLinha );
    FInternalLog.Add( StringOfChar('-', 80 ) );
  end;
end;

procedure TFormMainServer.SetEnables(const Value: Boolean);
begin
  EditNomeServer.Enabled := Value;
  EditPorta.Enabled := Value;

  ButtonConectar.Enabled := Value;
  ButtonDesconectar.Enabled := not Value;
end;

{ TComandoHardCode }

function TComandoCodeInverter.DoExecute: Boolean;
var
  i: Integer;
  lPalavra: string;
  lInvertida: string;
begin
  lInvertida := EmptyStr;
  if Param['Palavra'].ValueType = tvpBase64 then
    lPalavra := Param['Palavra'].AsBase64
  else
    lPalavra := Param['Palavra'].AsString;

  if lPalavra = EmptyStr then
    raise Exception.Create('Nenhuma palavra informada.');

  for i := 1 to Length(lPalavra) do
    lInvertida := lPalavra[i] + lInvertida;

  ResultParam['Invertida'].AsString := lInvertida;
  Result := True;
end;

function TComandoCodeInverter.DoGetDescription: string;
begin
  Result := 'Comando de Teste declarado no c�digo, diferente dos outros que � no componente. ' + sLineBreak
          + 'Este comando recebe uma palavra e retorna ela invertida.';
end;

procedure TComandoCodeInverter.DoRegisterParams;
begin
  inherited;
  NewParam('Palavra', tvpString);
  NewResult('Invertida', tvpString);
end;

{ TComandoFavIco }

function TComandoFavIco.DoExecute: Boolean;
begin
  ResultParam['icon'].AsString := 'NULL';
  Result := True;
end;

procedure TComandoFavIco.DoRegisterParams;
begin
  inherited;
  NewResult('icon', tvpString);
end;

initialization
  TCPClassRegistrer.Registrate(TComandoCodeInverter, 'HARD_CODE', 'inverter');
  TCPClassRegistrer.Registrate(TComandoFavIco, 'HARD_CODE', 'favicon.ico');

end.
