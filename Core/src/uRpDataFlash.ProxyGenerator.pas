unit uRpDataFlash.ProxyGenerator;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, uRpDataFlash.Command, SysUtils, Contnrs, uRpDataFlash.Types,
  uRpDataFlash.Components, uRpDataFlash.CommandController, uRpDataFlash.DataSetProvider,
  uRpDataFlash.GetCommandList, DBClient, DB, Forms, uRpDataFlash.ObjectReg,
  uRpDataFlash.Utils, uRpResourceString;

const
  C_DEFAULT_PROXY_NAME = 'uClassesProxyGenerator';
  C_PREFIX_NAME = 'TProxy';
  C_PROXY_BASE = 'TCustomProxy';
  C_PROXY_BASE_COMPONENT = 'TCustomProxyClient';
  C_BASE_CLASS_NAME   = 'TRpDataFlashCommand';

type
  TRpDataFlashTipoRetornoProxy = (
    trpFactory,     // gera o arquivo proxy
    trpDataSetList, // retorna uma lista com todos os comandos do tipo DataSet
    trpCommandList, // retorna uma lista com todos os comandos que NAO SAO DataSet
    trpCommandInfo, // retorna os parametros e retornos do comando informado
    trpFactoryList  // retorna um XMLData com os grupos e comandos (passo que antecede a geração do proxy)
  );

  TProxyListaComandosSelecionados = class(TStringList)
  public
    function ComandoSelecionado(const AGrupo, AComando : string) : Boolean;
    function GrupoValido(const AGrupo : string) : Boolean;
  end;

  TProxyParametroComandoItem = class
  private
    FNome: string;
    FTipoParametro: TRpDataFlashParamType;
    FTipoValor: TRpDataFlashParamValueType;
    FBaseClass: string;
  public
    property Nome : string read FNome write FNome;
    property TipoParametro : TRpDataFlashParamType read FTipoParametro write FTipoParametro;
    property TipoValor : TRpDataFlashParamValueType read FTipoValor write FTipoValor;
    property BaseClass : string read FBaseClass write FBaseClass;
    function TipoValorAsString(const pNatural : Boolean = True) : string;
  end;

  TProxyParametroComandoList = class(TObjectList)
  private
    function GetParametros(const AIndex: Integer): TProxyParametroComandoItem;
    function GetListaParametros : string;
  public
    property Parametros[const AIndex : Integer] : TProxyParametroComandoItem read GetParametros; default;
    procedure AddParametro(const PParams : TRpDataFlashCommandParameter); overload;
    procedure AddParametro(const PParams : TRpDataFlashParametroItem); overload;
    function GetAsString(const pCommandName : string; const pIdentCount : Smallint;
      const pNomeGrupo : string) : string;
  end;

  TProxyClassSupport = class
  private class var
    PrefixoCmd : string;
  public
    class function GetInterfaceForClass(const pClass: TRpDataFlashAbstractClass): IRpDataFlashCommandInterfaced;
    class function GetNomeComando(const AClassName: string): string;
  end;

  TProxyComandosItem = class
  private
    FNome: string;
    FListaParametros: TProxyParametroComandoList;
    FNomeClasse: string;
    FDescricao: string;
    procedure SetNome(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Nome : string read FNome write SetNome;
    property NomeClasse : string read FNomeClasse write FNomeClasse;
    property ListaParametros : TProxyParametroComandoList read FListaParametros write FListaParametros;
    property Descricao : string read FDescricao write FDescricao;
    procedure LoadParams(const pRegistroComando: TTcpClassRegisterItem); overload;
    procedure LoadParams(const pListaParametros: TRpDataFlashParametrosCollection); overload;
  end;

  TProxyComandosList = class(TObjectList)
  private
    function GetComandos(const AIndex: Integer): TProxyComandosItem;
  public
    property Comandos[const AIndex : Integer] : TProxyComandosItem read GetComandos; default;
    function AddComando(const pCommandName, pDescricaoComando : string) : TProxyComandosItem;
    function Find(const pCommandName : string) : TProxyComandosItem;
    function DeclararComandos : string;
    function ImplementarComandos(const pNomeGrupo : string) : string;
  end;

  TProxyGruposItem = class
  private
    FNome: string;
    FListaComandos: TProxyComandosList;
    procedure SetNome(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Nome : string read FNome write SetNome;
    property ListaComandos : TProxyComandosList read FListaComandos write FListaComandos;
  end;

  TProxyGruposList = class(TObjectList)
  private
    function GetGrupos(const AIndex: Integer): TProxyGruposItem;
  public
    property Grupos[const AIndex : Integer] : TProxyGruposItem read GetGrupos; default;
    function AddGrupo(const pNomeGrupo : string) : TProxyGruposItem;
    function Find(const pNomeGrupo : string) : TProxyGruposItem;
    procedure RegistrarComando(const pRegistroComando : TTcpClassRegisterItem); overload;
    procedure RegistrarComando(const pRegistroComando : TRpDataFlashCommandItem;
      const pNomeGrupo : string); overload;
    procedure RegistrarComando(const pRegistroComando : TRpDataFlashCustomDataSetProvider); overload;

    function DeclararGrupos : string;
    function ImplementarGrupos : string;
  end;

  TRpDataFlashComandoList = class(TRpDataFlashCommand)
  private
    FGrupos : TProxyGruposList;
    FTipoBusca: TRpDataFlashTipoRetornoProxy;
    FInfoString: string;
    FListaSelecionados: TProxyListaComandosSelecionados;
    FGerarClasses: Boolean;
    FConfigUnit : string;
    FConfigClass : string;
    procedure CarregaListaDeGruposRegistrados;
    procedure CarregaListaDeGruposDoController;
    procedure CarregaListaDeDataSetProviders;
    function GetLookupClassName : string;

    function DeclararComentarioDaUnit : string;
    function DeclararGrupos : string;
    function DeclararFactory : string;
    function DeclararSingletonProxyFactory : string;
    function ImplementarSingletonProxyFactory : string;
    function ImplementarGrupos : string;
    function ImplementarFactory : string;
    function ImplementarServiceLookupFactory : string;
    function ImplementarFinalizationSection : string;

    // retornos
    function DoRetornaFactory : string;
    function DoRetornaDataSetList : string;
    function DoRetornaCommandList : string;
    function DoRetornaCommandInfo : string;
    function DoRetornaFactoryList : string;
  protected
    function GerarProxy : string;
    function GerarClassUnit : string;
    function DoExecute : Boolean; override;
    procedure DoRegisterParams; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property TipoBusca : TRpDataFlashTipoRetornoProxy read FTipoBusca write FTipoBusca;
    property InfoString : string read FInfoString write FInfoString;
  end;

implementation

uses
  StrUtils,
  Math;

function FormatParamLine(const pStartIdent : Byte; const pLine : string; const pDelimiter : Char) : string;
var
  lTxt : TStringList;
  i: Integer;
begin
  Result := StringOfChar(' ', pStartIdent) + pLine;
  lTxt := TStringList.Create;
  // quebra em 110 colunas
  try
    lTxt.LineBreak := '|';
    lTxt.Text := WrapText(Result, lTxt.LineBreak, [pDelimiter], 100);

    // para cada linha corrige a identacao
    for i := 0 to lTxt.Count - 1 do
      lTxt[i] := StringOfChar(' ', pStartIdent + (IfThen(i <> 0, 2))) + Trim(lTxt[i]);

    lTxt.LineBreak := #$A#$D;
    Result := TrimRight(lTxt.Text);
  finally
    FreeAndNil( lTxt );
  end;
end;

function TRpDataFlashComandoList.DeclararGrupos: string;
var
  lClassesProxyModelo: TStringList;
begin
  lClassesProxyModelo := TStringList.Create;
  try
    // cria a classe base para os "TProxy"
    lClassesProxyModelo.Add('  ' + C_PROXY_BASE + ' = class(' + C_PROXY_BASE_COMPONENT + ')' );
    lClassesProxyModelo.Add('  private');
    lClassesProxyModelo.Add('    FReconnect: Boolean;');
    lClassesProxyModelo.Add('  protected');
    lClassesProxyModelo.Add('    procedure DoAoProcessarErroComunicacao; override;');
    lClassesProxyModelo.Add('  public');
    lClassesProxyModelo.Add('    property Reconnect : Boolean read FReconnect write FReconnect;');
    // Ex.:  ShellExecute(0, nil, 'cmd.exe', '/C net start SomeService', nil, SW_HIDE);
    lClassesProxyModelo.Add('  end;');
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add( FGrupos.DeclararGrupos );

    Result := TrimRight( lClassesProxyModelo.Text );
  finally
    FreeAndNil(lClassesProxyModelo);
  end;
end;

function TRpDataFlashComandoList.ImplementarServiceLookupFactory: string;
var
  lService: TStringList;
  lLookuop: string;
begin
  lLookuop := GetLookupClassName;

  lService := TStringList.Create;
  try
    lService.Add('{ TServiceLookupFactory }');
    lService.Add('');

    // XMLData
    lService.Add('function TServiceLookupFactory.GetXMLData(const pSQL: string): string;');
    lService.Add('begin');
    lService.Add('  ProxyFactory.' + lLookuop + '.GetLookupXMLData(pSQL, Result);');
    lService.Add('end;');
    lService.Add('');

    // GetConfig
    lService.Add('function TServiceLookupFactory.GetConfigValues(const pConfigLookupClass: string;');
    lService.Add('  out ALookupKeys, ALookupDisplay, ALookupSQL, ADAOClass: string): Boolean;');
    lService.Add('begin');
    lService.Add('  Result := ProxyFactory.' + lLookuop + '.GetLookupConfigValues(');
    lService.Add('    pConfigLookupClass, ALookupKeys, ALookupDisplay, ALookupSQL, ADAOClass);');
    lService.Add('end;');
    Result := lService.Text;
  finally
    FreeAndNil( lService );
  end;
end;

function TRpDataFlashComandoList.DeclararComentarioDaUnit: string;
var
  lHint : TStringList;
begin
  lHint := TStringList.Create;
  try
    lHint.Add('//   Não modifique esta Unit, seu código é gerado automaticamente pelo Cliente de');
    lHint.Add('// TCP buscando as classes de serviço registradas no servidor.');
    lHint.Add('// - Gerado em...: '  + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) );
    lHint.Add('// - App Servidor: "' + Application.ExeName + '"');
    lHint.Add('// - Server......: '  + TRpDataFlashUtils.GetLocalComputerName);

    Result := TrimRight(lHint.Text);
  finally
    FreeAndNil(lHint);
  end;
end;

function TRpDataFlashComandoList.ImplementarFinalizationSection: string;
var
  lImplement : TStringList;
begin
  lImplement := TStringList.Create;
  try
    lImplement.Add('initialization');
    lImplement.Add('');
    lImplement.Add('finalization');
    lImplement.Add('  if FProxyFactory <> nil then');
    lImplement.Add('    FProxyFactory.Free;');
    lImplement.Add('  if FProxyFactoryRest <> nil then');
    lImplement.Add('    FProxyFactoryRest.Free;');
    lImplement.Add('');

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TRpDataFlashComandoList.ImplementarGrupos: string;
var
  lImplementFuncoes: TStringList;
begin
  lImplementFuncoes := TStringList.Create;
  try
    lImplementFuncoes.Add('{ ' + C_PROXY_BASE + ' }' );
    lImplementFuncoes.Add('');
    lImplementFuncoes.Add('procedure ' + C_PROXY_BASE + '.DoAoProcessarErroComunicacao;');
    lImplementFuncoes.Add('begin');
    lImplementFuncoes.Add('  inherited;');
    lImplementFuncoes.Add('  //Sample for Restart the service');
    lImplementFuncoes.Add('  //if FReconnect then');
    lImplementFuncoes.Add('  //  ShellExecute(0, nil, ''cmd.exe'', ''/C net start SomeServiceName'', nil, SW_HIDE);');
    lImplementFuncoes.Add('end;');
    lImplementFuncoes.Add('');

    lImplementFuncoes.Add( FGrupos.ImplementarGrupos );
    Result := TrimRight( lImplementFuncoes.Text );
  finally
  FreeAndNil(lImplementFuncoes);
  end;
end;

function TRpDataFlashComandoList.ImplementarFactory: string;
var
  lImplement : TStringList;
  I: Integer;
begin
  lImplement := TStringList.Create;

  try
    lImplement.Add('{ TProxyFactory }');
    lImplement.Add('');
    lImplement.Add('constructor TProxyFactory.Create(AClient : TRpDataFlashCustomClientConnection; const AConnectionType : TConnectionType);');
    if FConfigClass <> '' then
    begin
      lImplement.Add('var');
      lImplement.Add('  lFileConf : IRpDataFlashConfig;');
    end;
    lImplement.Add('begin');
    lImplement.Add('  FConnectionType := AConnectionType;');
    lImplement.Add('  FSerializationFormat := sfXML;');
    lImplement.Add('  FReconnect := True;');
    lImplement.Add('  FSharedClientTCP := False;');
    lImplement.Add('  FSharedClientREST := False;');
    lImplement.Add('  FClientTCP := nil;');
    lImplement.Add('  FClientREST := nil;');
    lImplement.Add('  FBusy := False;');
    lImplement.Add('  FCheckBusy := True;');
    lImplement.Add(' ');
    lImplement.Add('  case AConnectionType of');
    lImplement.Add('    ctTcpIp:');
    lImplement.Add('      if Assigned(AClient) then');
    lImplement.Add('        FClientTCP := AClient as TRpDataFlashClientConnection;');
    lImplement.Add('    ctREST:');
    lImplement.Add('      if Assigned(AClient) then');
    lImplement.Add('        FClientREST := AClient as TRpDataFlashRESTClient;');
    lImplement.Add('  end;');
    lImplement.Add('  FSharedClientTCP := Assigned(FClientTCP);');
    lImplement.Add('  FSharedClientREST := Assigned(FClientREST);');
//--------------------
    if FConfigClass <> '' then
    begin
      lImplement.Add('  if (not FSharedClientREST) or (not FSharedClientTCP) then');
      lImplement.Add('  begin');
      // se criar o client, sempre fica como "localhost"
      lImplement.Add('    lFileConf := ' + FConfigClass + '.Create;');
      lImplement.Add('    try');
      lImplement.Add('      if not FSharedClientTCP then');
      lImplement.Add('      begin');
      lImplement.Add('        FClientTCP := TRpDataFlashClientConnection.Create(nil);');
      lImplement.Add('        FClientTCP.Server := lFileConf.ServerName;');
      lImplement.Add('        FClientTCP.Port := lFileConf.ServerPort;');
      lImplement.Add('        FClientTCP.FileTransfer.Port := lFileConf.FTPPort;');
      lImplement.Add('        FClientTCP.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;');
      lImplement.Add('        FClientTCP.EncryptionType := lFileConf.EncryptionType;');
      lImplement.Add('        FClientTCP.CommunicationType := lFileConf.CommunicationType;');
      lImplement.Add('        FClientTCP.ConvertLocalHostToIP := lFileConf.LocalHostToIP;');
      lImplement.Add('        FClientTCP.Password := lFileConf.Password;');
      lImplement.Add('        FClientTCP.UserName := lFileConf.UserName;');
      lImplement.Add('      end;');
      lImplement.Add('      if not FSharedClientREST then');
      lImplement.Add('      begin');
      lImplement.Add('        FClientREST := TRpDataFlashRESTClient.Create(nil);');
      lImplement.Add('        FClientREST.Server := lFileConf.ServerName;');
      lImplement.Add('        FClientREST.Port := lFileConf.RestPort;');
      lImplement.Add('        FClientREST.FileTransfer.Port := lFileConf.FTPPort;');
      lImplement.Add('        FClientREST.FileTransfer.Enabled := FClientREST.FileTransfer.Port > 0;');
      lImplement.Add('        FClientREST.EncryptionType := lFileConf.EncryptionType;');
      lImplement.Add('        FClientREST.CommunicationType := lFileConf.CommunicationType;');
      lImplement.Add('        FClientREST.ConvertLocalHostToIP := lFileConf.LocalHostToIP;');
      lImplement.Add('        FClientREST.Password := lFileConf.Password;');
      lImplement.Add('        FClientREST.UserName := lFileConf.UserName;');
      lImplement.Add('      end;');
      lImplement.Add('    finally');
      lImplement.Add('      lFileConf := nil;');
      lImplement.Add('    end;');
      lImplement.Add('  end;');
    end;
//--------------------
    lImplement.Add('end;');

    lImplement.Add('');
    lImplement.Add('destructor TProxyFactory.Destroy;');
    lImplement.Add('begin');

    // libera todos os proxies que criou
    for i := 0 to FGrupos.Count - 1 do
    begin
      lImplement.Add('  if Assigned(FProxy' + FGrupos[i].Nome + ') then' );
      lImplement.Add('    FreeAndNil(FProxy' + FGrupos[i].Nome + ');' );
    end;
    lImplement.Add('  if Assigned(FClientREST) and (not FSharedClientREST) then');
    lImplement.Add('    FreeAndNil(FClientREST);');
    lImplement.Add('  if Assigned(FClientTCP) and (not FSharedClientTCP) then');
    lImplement.Add('  begin');
    lImplement.Add('    try');
    lImplement.Add('      if FClientTCP.Connected then');
    lImplement.Add('        FClientTCP.Disconnect;');
    lImplement.Add('    finally');
    lImplement.Add('      FreeAndNil(FClientTCP);');
    lImplement.Add('    end;');
    lImplement.Add('  end;');
    lImplement.Add('  inherited;');
    lImplement.Add('end;');
    lImplement.Add('');

    if GetLookupClassName <>  EmptyStr then
    begin
      lImplement.Add('function TProxyFactory.GetServiceLookupFactory: TServiceLookupFactory;');
      lImplement.Add('begin');
      lImplement.Add('  Result := TServiceLookupFactory.Create;');
      lImplement.Add('end;');
      lImplement.Add('');
    end;

    lImplement.Add('function TProxyFactory.GetLazyConection: Boolean;');
    lImplement.Add('begin');
    lImplement.Add('  Result := False;');
    lImplement.Add('  if FConnectionType = ctTcpIp then');
    lImplement.Add('  begin');
    lImplement.Add('    if FClientTCP <> nil then');
    lImplement.Add('      Result := FClientTCP.LazyConnection;');
    lImplement.Add('  end else if FClientREST <> nil then');
    lImplement.Add('    Result := FClientREST.LazyConnection;');
    lImplement.Add('end;');
    lImplement.Add(' ');

    lImplement.Add('procedure TProxyFactory.SetLazyConection(const Value: Boolean);');
    lImplement.Add('begin');
    lImplement.Add('  if FConnectionType = ctTcpIp then');
    lImplement.Add('  begin');
    lImplement.Add('    if FClientTCP <> nil then');
    lImplement.Add('      FClientTCP.LazyConnection := Value;');
    lImplement.Add('  end else if FClientREST <> nil then');
    lImplement.Add('    FClientREST.LazyConnection := Value;');
    lImplement.Add('end;');
    lImplement.Add(' ');

    lImplement.Add('procedure TProxyFactory.Config(const AHost: string; const APorta, APortaFTP: Integer;');
    lImplement.Add('  const AConnectionType : TConnectionType;');
    lImplement.Add('  const ALocalHostToIP : Boolean;');
    lImplement.Add('  const ATipoCriptografia : TRpDataFlashEncryptionType;');
    lImplement.Add('  const ATipoComunicacao : TRpDataFlashCommunicationType;');
    lImplement.Add('  const AUserName, APassword : string);');
    lImplement.Add('begin');
    lImplement.Add('  if AConnectionType = ctTcpIp then');
    lImplement.Add('  begin');
    lImplement.Add('    if FClientTCP <> nil then');
    lImplement.Add('    begin');
    lImplement.Add('      if FSharedClientTCP then');
    lImplement.Add('        FClientTCP.Disconnect');
    lImplement.Add('      else');
    lImplement.Add('        FreeAndNil(FClientTCP);');
    lImplement.Add('    end;');
    lImplement.Add('');
    lImplement.Add('    if not FSharedClientTCP then');
    lImplement.Add('      FClientTCP := TRpDataFlashClientConnection.Create(nil);');
    lImplement.Add('');
    lImplement.Add('    FClientTCP.Server := AHost;');
    lImplement.Add('    FClientTCP.Port := APorta;');
    lImplement.Add('    FClientTCP.EncryptionType := ATipoCriptografia;');
    lImplement.Add('    FClientTCP.CommunicationType := ATipoComunicacao;');
    lImplement.Add('    FClientTCP.FileTransfer.Port := APortaFTP;');
    lImplement.Add('    FClientTCP.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;');
    lImplement.Add('    FClientTCP.ConvertLocalHostToIP := ALocalHostToIP;');
    lImplement.Add('    FClientTCP.UserName := AUserName;');
    lImplement.Add('    FClientTCP.Password := APassword;');
    lImplement.Add('  end else');
    lImplement.Add('  begin');
    lImplement.Add('    if (FClientREST <> nil) and (not FSharedClientREST) then');
    lImplement.Add('      FreeAndNil(FClientREST);');
    lImplement.Add('');
    lImplement.Add('    if not FSharedClientREST then');
    lImplement.Add('      FClientREST := TRpDataFlashRESTClient.Create(nil);');
    lImplement.Add('');
    lImplement.Add('    FClientREST.Server := AHost;');
    lImplement.Add('    FClientREST.Port := APorta;');
    lImplement.Add('    FClientREST.EncryptionType := ATipoCriptografia;');
    lImplement.Add('    FClientREST.CommunicationType := ATipoComunicacao;');
    lImplement.Add('    FClientREST.FileTransfer.Port := APortaFTP;');
    lImplement.Add('    FClientREST.FileTransfer.Enabled := FClientTCP.FileTransfer.Port > 0;');
    lImplement.Add('    FClientREST.ConvertLocalHostToIP := ALocalHostToIP;');
    lImplement.Add('    FClientREST.UserName := AUserName;');
    lImplement.Add('    FClientREST.Password := APassword;');
    lImplement.Add('  end;');
    lImplement.Add('end;');
    lImplement.Add('');
    lImplement.Add('function TProxyFactory.ServerOnline: Boolean;');
    lImplement.Add('begin');
    lImplement.Add('  Result := FClientTCP.ServerOnline;');
    lImplement.Add('end;');
    lImplement.Add('');
    lImplement.Add('procedure TProxyFactory.DoCheckBusy;');
    lImplement.Add('begin');
    lImplement.Add('  if FCheckBusy then');
    lImplement.Add('  begin');
    lImplement.Add('    while FBusy do');
    lImplement.Add('      Sleep(500);');
    lImplement.Add('  end;');
    lImplement.Add('end;');
    lImplement.Add('');
    // Gerar Implementacoes de Classes
    for i := 0 to FGrupos.Count - 1 do
    begin
      lImplement.Add(Format('function TProxyFactory.%s: TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome ]));
      lImplement.Add(       'begin');
      lImplement.Add(Format('  if FProxy%s = nil then', [FGrupos[i].Nome ]));
      lImplement.Add(       '  begin');
      lImplement.Add(       '    if FConnectionType = ctTcpIp then');
      lImplement.Add(Format('      FProxy%s := TProxy%s.Create(FClientTCP, BusyCallback, FSharedClientTCP)', [FGrupos[i].Nome, FGrupos[i].Nome]));
      lImplement.Add(       '    else');
      lImplement.Add(Format('      FProxy%s := TProxy%s.Create(FClientREST, BusyCallback, FSharedClientREST);', [FGrupos[i].Nome, FGrupos[i].Nome]));
      lImplement.Add(Format('    FProxy%s.Reconnect := FReconnect;', [FGrupos[i].Nome]));
      lImplement.Add(       '  end;');
      lImplement.Add(Format('  FProxy%s.SerializationFormat := FSerializationFormat;', [FGrupos[i].Nome]));
      lImplement.Add(       '  DoCheckBusy;');
      lImplement.Add(Format('  Result := FProxy%s;', [FGrupos[i].Nome ]));
      lImplement.Add(       'end;');
      lImplement.Add('');
    end;
    lImplement.Add('procedure TProxyFactory.Config(const AConexaoTCP: TRpDataFlashClientConnection);');
    lImplement.Add('begin');
    lImplement.Add('  ProxyFactory.Config(');
    lImplement.Add('    AConexaoTCP.Server,');
    lImplement.Add('    AConexaoTCP.Port,');
    lImplement.Add('    AConexaoTCP.FileTransfer.Port,');
    lImplement.Add('    ctTcpIp,');
    lImplement.Add('    AConexaoTCP.ConvertLocalHostToIP,');
    lImplement.Add('    AConexaoTCP.EncryptionType,');
    lImplement.Add('    AConexaoTCP.CommunicationType,');
    lImplement.Add('    AConexaoTCP.UserName,');
    lImplement.Add('    AConexaoTCP.Password);');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure TProxyFactory.Config(const AConexaoREST: TRpDataFlashRESTClient);');
    lImplement.Add('begin');
    lImplement.Add('  ProxyFactory.Config(');
    lImplement.Add('    AConexaoREST.Server,');
    lImplement.Add('    AConexaoREST.Port,');
    lImplement.Add('    AConexaoREST.FileTransfer.Port,');
    lImplement.Add('    ctREST,');
    lImplement.Add('    AConexaoREST.ConvertLocalHostToIP,');
    lImplement.Add('    AConexaoREST.EncryptionType,');
    lImplement.Add('    AConexaoREST.CommunicationType,');
    lImplement.Add('    AConexaoREST.UserName,');
    lImplement.Add('    AConexaoREST.Password);');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure TProxyFactory.BusyCallback(const AStart: Boolean);');
    lImplement.Add('begin');
    lImplement.Add('  FBusy := AStart;');
    lImplement.Add('end;');

    if FGerarClasses then
    begin
      lImplement.Add('{ TFileCustomObjectHelper }');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.Delete(const AProxy : TProxyFactory): Boolean;');
      lImplement.Add('begin');
      lImplement.Add('  Result := DoEnviarHelper(haDelete, AProxy);');
      lImplement.Add('end;');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.DoEnviarHelper(const AOperacao: TRpDataFlashHelperAction;');
      lImplement.Add('  const AProxy : TProxyFactory): Boolean;');
      lImplement.Add('var');
      lImplement.Add('  lCmd: TRpDataFlashComandoEnvio;');
      lImplement.Add('begin');
      lImplement.Add('  SetLastError(EmptyStr);');
      lImplement.Add('  lCmd := TRpDataFlashComandoEnvio.Create;');
      lImplement.Add('  try');
      lImplement.Add('    lCmd.SetComando(''TRpDataFlashComandoHelper'');');
      lImplement.Add('    lCmd.Parametros.Novo(''Object'', '' '', tpEntrada, tvpBase64);');
      lImplement.Add('    lCmd.Parametros.Parametro[''Object''].AsBase64 := Self.SaveToXmlString;');
      lImplement.Add('    lCmd.Parametros.Novo(''ObjectClass'', Self.ClassName, tpEntrada, tvpString);');
      lImplement.Add('    lCmd.Parametros.Novo(''Operacao'', Ord(AOperacao), tpEntrada, tvpInteger);');
      lImplement.Add('    if AProxy = nil then');
      lImplement.Add('    begin');
      lImplement.Add('      lCmd.SerializationFormat := ProxyFactory.SerializationFormat;');
      lImplement.Add('      ProxyFactory.FClientTCP.Comunicar(lCmd)');
      lImplement.Add('    end');
      lImplement.Add('    else');
      lImplement.Add('    begin');
      lImplement.Add('      lCmd.SerializationFormat := AProxy.SerializationFormat;');
      lImplement.Add('      AProxy.FClientTCP.Comunicar(lCmd);');
      lImplement.Add('    end;');
      lImplement.Add('    Result := lCmd.StatusRetorno;');
      lImplement.Add('    if not Result then');
      lImplement.Add('      SetLastError(lCmd.LastError)');
      lImplement.Add('    else');
      lImplement.Add('      Self.LoadFromString(lCmd.Parametros.Parametro[''Object''].AsBase64);');
      lImplement.Add('  except');
      lImplement.Add('    on E:Exception do');
      lImplement.Add('    begin');
      lImplement.Add('      Result := False;');
      lImplement.Add('      SetLastError(E.Message);');
      lImplement.Add('    end;');
      lImplement.Add('  end;');
      lImplement.Add('  FreeAndNil(lCmd);');
      lImplement.Add('end;');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.Execute(const AProxy : TProxyFactory): Boolean;');
      lImplement.Add('begin');
      lImplement.Add('  Result := DoEnviarHelper(haExecute, AProxy);');
      lImplement.Add('end;');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.GetLastError: string;');
      lImplement.Add('begin');
      lImplement.Add('  Result := Self.LastError;');
      lImplement.Add('end;');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.Load(const AProxy : TProxyFactory): Boolean;');
      lImplement.Add('begin');
      lImplement.Add('  Result := DoEnviarHelper(haLoad, AProxy);');
      lImplement.Add('end;');
      lImplement.Add(' ');
      lImplement.Add('function TFileCustomObjectHelper.Save(const AProxy : TProxyFactory): Boolean;');
      lImplement.Add('begin');
      lImplement.Add('  Result := DoEnviarHelper(haSave, AProxy);');
      lImplement.Add('end;');
      lImplement.Add(' ');
    end;

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TRpDataFlashComandoList.ImplementarSingletonProxyFactory: string;
var
  lImplement : TStringList;
begin
  lImplement := TStringList.Create;
  try
    lImplement.Add('function ProxyFactory(ATcpClient : TRpDataFlashClientConnection) : TProxyFactory;');
    lImplement.Add('begin');
    lImplement.Add('  if FProxyFactory = nil then');
    lImplement.Add('    FProxyFactory := TProxyFactory.Create(ATcpClient, ctTcpIp);');
    lImplement.Add('  Result := FProxyFactory;');
    lImplement.Add('end;');
    lImplement.Add(' ');

    lImplement.Add('function ProxyFactoryRest(ARestClient : TRpDataFlashRESTClient) : TProxyFactory;');
    lImplement.Add('begin');
    lImplement.Add('  if FProxyFactoryRest = nil then');
    lImplement.Add('    FProxyFactoryRest := TProxyFactory.Create(ARestClient, ctREST);');
    lImplement.Add('  Result := FProxyFactoryRest;');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure NewProxyFactory(out AProxyFactory : TProxyFactory; const ATcpClient : TRpDataFlashClientConnection = nil);');
    lImplement.Add('begin');
    lImplement.Add('  AProxyFactory := TProxyFactory.Create(ATcpClient, ctTcpIp);');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure NewProxyFactoryRest(out AProxyFactory : TProxyFactory; const ARestClient : TRpDataFlashRESTClient = nil);');
    lImplement.Add('begin');
    lImplement.Add('  AProxyFactory := TProxyFactory.Create(ARestClient, ctREST);');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure RenewProxyFactory;');
    lImplement.Add('var');
    lImplement.Add('  lClient: TRpDataFlashClientConnection;');
    lImplement.Add('begin');
    lImplement.Add('  if Assigned(FProxyFactory) then');
    lImplement.Add('  begin');
    lImplement.Add('    if FProxyFactory.FSharedClientTCP then');
    lImplement.Add('      lClient := FProxyFactory.FClientTCP');
    lImplement.Add('    else');
    lImplement.Add('      lClient := nil;');
    lImplement.Add(' ');
    lImplement.Add('    FreeAndNil( FProxyFactory );');
    lImplement.Add('    ProxyFactory( lClient );');
    lImplement.Add('  end');
    lImplement.Add('  else');
    lImplement.Add('    ProxyFactory;');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure RenewProxyFactoryRest;');
    lImplement.Add('var');
    lImplement.Add('  lClientRest: TRpDataFlashRESTClient;');
    lImplement.Add('begin');
    lImplement.Add('  if Assigned(FProxyFactoryRest) then');
    lImplement.Add('  begin');
    lImplement.Add('    if FProxyFactoryRest.FSharedClientREST then');
    lImplement.Add('      lClientRest := FProxyFactoryRest.FClientREST');
    lImplement.Add('    else');
    lImplement.Add('      lClientRest := nil;');
    lImplement.Add(' ');
    lImplement.Add('    FreeAndNil( FProxyFactoryRest );');
    lImplement.Add('    ProxyFactoryRest( lClientRest );');
    lImplement.Add('  end');
    lImplement.Add('  else');
    lImplement.Add('    ProxyFactoryRest;');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure FreeProxyFactory;');
    lImplement.Add('begin');
    lImplement.Add('  if Assigned(FProxyFactory) then');
    lImplement.Add('    FreeAndNil( FProxyFactory );');
    lImplement.Add('end;');
    lImplement.Add(' ');
    lImplement.Add('procedure FreeProxyFactoryRest;');
    lImplement.Add('begin');
    lImplement.Add('  if Assigned(FProxyFactoryRest) then');
    lImplement.Add('    FreeAndNil( FProxyFactoryRest );');
    lImplement.Add('end;');
    lImplement.Add(' ');

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TRpDataFlashComandoList.DeclararFactory: string;
var
  lImplement : TStringList;
  i: Integer;
  lLookup : string;
begin
  lLookup := GetLookupClassName;
  lImplement := TStringList.Create;

  try
    lImplement.Add('  TProxyFactory = class');
    lImplement.Add('  protected');
    lImplement.Add('    function GetLazyConection: Boolean;');
    lImplement.Add('    procedure SetLazyConection(const Value: Boolean);');

    if lLookup <> EmptyStr then
      lImplement.Add('    function GetServiceLookupFactory : TServiceLookupFactory;');

    lImplement.Add('  private');
    lImplement.Add('    FClientTCP : TRpDataFlashClientConnection;');
    lImplement.Add('    FClientREST : TRpDataFlashRESTClient;');
    lImplement.Add('    FSharedClientTCP: Boolean;');
    lImplement.Add('    FSharedClientREST: Boolean;');
    lImplement.Add('    FSerializationFormat: TSerializationFormat;');
    lImplement.Add('    FReconnect: Boolean;');
    lImplement.Add('    FConnectionType: TConnectionType;');
    lImplement.Add('    FBusy : Boolean;');
    lImplement.Add('    FCheckBusy: Boolean;');

    // Gerar os fields conforme Classes de proxy ( FProxyVenda : TProxyVenda; )
    for i := 0 to FGrupos.Count - 1 do
      lImplement.Add(Format('    FProxy%s : TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome] ));

    lImplement.Add('    procedure DoCheckBusy;');
    lImplement.Add('    procedure BusyCallback(const AStart : Boolean);');
    lImplement.Add('  public');
    lImplement.Add('    constructor Create(AClient : TRpDataFlashCustomClientConnection; const AConnectionType : TConnectionType);');
    lImplement.Add('    destructor Destroy; override;');
    lImplement.Add('    procedure Config(const AHost : string; const APorta, APortaFTP : Integer;');
    lImplement.Add('      const AConnectionType : TConnectionType;');
    lImplement.Add('      const ALocalHostToIP : Boolean; ');
    lImplement.Add('      const ATipoCriptografia : TRpDataFlashEncryptionType = tcBase64Compressed;');
    lImplement.Add('      const ATipoComunicacao : TRpDataFlashCommunicationType = ctText;');
    lImplement.Add('      const AUserName : string = ''''; const APassword : string = ''''); overload;');
    lImplement.Add('    procedure Config(const AConexaoTCP : TRpDataFlashClientConnection); overload;');
    lImplement.Add('    procedure Config(const AConexaoREST : TRpDataFlashRESTClient); overload;');
    lImplement.Add('    function ServerOnline : Boolean;');
    lImplement.Add('    property Reconnect : Boolean read FReconnect write FReconnect;');
    lImplement.Add('    property ConnectionType : TConnectionType read FConnectionType;');
    lImplement.Add('    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat;');
    lImplement.Add('    property CheckBusy : Boolean read FCheckBusy write FCheckBusy default True;');
    lImplement.Add('    property LazyConection : Boolean read GetLazyConection write SetLazyConection;');

    if lLookup <> '' then
      lImplement.Add('    property ServiceLookupFactory : TServiceLookupFactory read GetServiceLookupFactory;');

    // Gerar os métodos conforme Classes de proxy
    for I := 0 to FGrupos.Count - 1 do
      lImplement.Add(Format('    function %s : TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome]));

    lImplement.Add('  end;');

    if FGerarClasses then
    begin
      lImplement.Add(' ');
      lImplement.Add('  TFileCustomObjectHelper = class helper for TFileCustomObject');
      lImplement.Add('  private');
      lImplement.Add('    function DoEnviarHelper(const AOperacao : TRpDataFlashHelperAction; const AProxy : TProxyFactory) : Boolean;');
      lImplement.Add('  public');
      lImplement.Add('    function Save(const AProxy : TProxyFactory = nil) : Boolean;');
      lImplement.Add('    function Load(const AProxy : TProxyFactory = nil) : Boolean;');
      lImplement.Add('    function Execute(const AProxy : TProxyFactory = nil) : Boolean;');
      lImplement.Add('    function Delete(const AProxy : TProxyFactory = nil) : Boolean;');
      lImplement.Add('    function GetLastError : string;');
      lImplement.Add('  end;');
    end;

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TRpDataFlashComandoList.DeclararSingletonProxyFactory: string;
var
  lList : TStringList;
begin
  lList := TStringList.Create;
  try
    lList.Add('function ProxyFactory(ATcpClient : TRpDataFlashClientConnection = nil) : TProxyFactory;');
    lList.Add('function ProxyFactoryRest(ARestClient : TRpDataFlashRESTClient = nil) : TProxyFactory;');
    lList.Add(' ');
    lList.Add('procedure NewProxyFactory(out AProxyFactory : TProxyFactory; const ATcpClient : TRpDataFlashClientConnection = nil);');
    lList.Add('procedure NewProxyFactoryRest(out AProxyFactory : TProxyFactory; const ARestClient : TRpDataFlashRESTClient = nil);');
    lList.Add(' ');
    lList.Add('procedure RenewProxyFactory;');
    lList.Add('procedure RenewProxyFactoryRest;');
    lList.Add(' ');
    lList.Add('procedure FreeProxyFactory;');
    lList.Add('procedure FreeProxyFactoryRest;');

    Result := TrimRight( lList.Text );
  finally
    FreeAndNil(lList);
  end;
end;

{ TRpDataFlashComandoList }

procedure TRpDataFlashComandoList.CarregaListaDeDataSetProviders;
var
  lEnumProvider: TListEnumerator;
  lProvider: TRpDataFlashCustomDataSetProvider;
begin
  // registrar os comandos do Collection
  // retorna todos os componentes Provider ligados ao server
  if GetServer.UseControllers then
  begin
    lEnumProvider := GetServer.Providers.GetEnumerator;
    while (lEnumProvider.MoveNext) do
    begin
      // provider nao tem controle sobre selecionados
      lProvider := TRpDataFlashCustomDataSetProvider(lEnumProvider.Current);
      FGrupos.RegistrarComando(lProvider);
    end;
    FreeAndNil(lEnumProvider);
  end;
end;

procedure TRpDataFlashComandoList.CarregaListaDeGruposDoController;
var
  lEnumGrupos: TListEnumerator;
  lEnumComandos: TCollectionEnumerator;

  lController: TRpDataFlashCommandController;
  lItemComando: TRpDataFlashCommandItem;
begin
  // registrar os comandos do Collection
  if GetServer.UseControllers then
  begin
    // pega os controllers do servidor
    lEnumGrupos := GetServer.Controllers.GetEnumerator;
    while (lEnumGrupos.MoveNext) do
    begin
      lController := TRpDataFlashCommandController(lEnumGrupos.Current);
      lEnumComandos := lController.Commands.GetEnumerator;
      try
        while lEnumComandos.MoveNext do
        begin
          lItemComando := TRpDataFlashCommandItem(lEnumComandos.Current);
          if FListaSelecionados.GrupoValido(lController.GroupName)
          and FListaSelecionados.ComandoSelecionado(lController.GroupName, lItemComando.Name) then
            FGrupos.RegistrarComando(lItemComando, lController.GroupName);
        end;
      finally
        FreeAndNil(lEnumComandos);
      end;
    end;
    FreeAndNil(lEnumGrupos);
  end;
end;

procedure TRpDataFlashComandoList.CarregaListaDeGruposRegistrados;
var
  lRegistrados: TTcpClassRegister;
  i: Integer;

  function InternalPodeGerarGrupo(const ANomeGrupo : string) : Boolean;
  begin
    Result := ANomeGrupo <> C_GROUP_INTERNAL;
    // se não for interno, realiza testes especificos para cada FTipoBusca
    if Result then
    begin
      Result := ((ANomeGrupo =  C_GROUP_DATASET) and (FTipoBusca = trpDataSetList))   // se for um DataSet
             or ((ANomeGrupo <> C_GROUP_DATASET) and (FTipoBusca <> trpDataSetList)); // se for um grupo qualquer mas nao buscando por DataSet
    end;
  end;

begin
  lRegistrados := nil;
  TCPClassRegistrer.Registrados(lRegistrados);

  // percorre todos os comandos registrados e envia para o grupo registrar
  for i := 0 to lRegistrados.Count - 1 do
    if InternalPodeGerarGrupo(lRegistrados[i].ProxyGroup)
    and FListaSelecionados.GrupoValido(lRegistrados[i].ProxyGroup)
    and FListaSelecionados.ComandoSelecionado(lRegistrados[i].ProxyGroup, lRegistrados[i].ProxyClass.ClassName) then
      FGrupos.RegistrarComando( lRegistrados[i] );

  FreeAndNil(lRegistrados);
end;

constructor TRpDataFlashComandoList.Create;
begin
  inherited Create;
  FGrupos := TProxyGruposList.Create(True);
  FListaSelecionados := TProxyListaComandosSelecionados.Create;
  FConfigClass := EmptyStr;
  FConfigUnit := EmptyStr;
  FGerarClasses := False;
end;

destructor TRpDataFlashComandoList.Destroy;
begin
  if Assigned(FGrupos) then
    FreeAndNil( FGrupos );

  if Assigned(FListaSelecionados) then
    FreeAndNil( FListaSelecionados );

  inherited Destroy;
end;

function TRpDataFlashComandoList.DoExecute: Boolean;
begin
  FTipoBusca := TRpDataFlashTipoRetornoProxy(Param['TipoBusca'].AsInteger);
  FInfoString := Trim( Param['InfoString'].AsBase64 );

  if TipoBusca = trpFactory then
  begin
    ResultParam['NomeArquivoProxy'].AsString := C_DEFAULT_PROXY_NAME;
    FListaSelecionados.Text := FInfoString;
    FGerarClasses := FListaSelecionados.Values['TRANSP'] = 'T';
    FConfigClass := FListaSelecionados.Values['CONFIG'];
    if FConfigClass <> '|' then
    begin
      FConfigUnit  := Copy(FConfigClass, 1, Pos('|', FConfigClass) - 1);
      FConfigClass := Copy(FConfigClass, Pos('|', FConfigClass) + 1, Length(FConfigClass));
    end
    else
    begin
      FConfigClass := EmptyStr;
      FConfigUnit := EmptyStr;
    end;
    FListaSelecionados.Delete(1);
    FListaSelecionados.Delete(0);
    FInfoString := '';
  end
  else
  begin
    ResultParam['NomeArquivoProxy'].AsString := EmptyStr;
    FListaSelecionados.Clear;
  end;

  TProxyClassSupport.PrefixoCmd := GetServer.BaseCommandPrefix;
  try
    ResultParam['RetornoProxy'].AsBase64 := GerarProxy;
    if (TipoBusca = trpFactory) and (FGerarClasses) then
      ResultParam['RetornoClass'].AsBase64 := GerarClassUnit;
    Result := True;
  except
    on E:Exception do
      raise Exception.Create('Erro gerando arquivo proxy. ' + E.Message );
  end;
end;

procedure TRpDataFlashComandoList.DoRegisterParams;
begin
  inherited;
{
 InfoString:
   trpFactory     -> Lista de comandos selecionados (GRUPO|COMANDO) - Linha 0 = Info Config | Linha 1 = Info Classe Base
   trpCommandInfo -> Nome do Comando
   trpFactoryList -> nada
   trpDataSetList -> nada
   trpCommandList -> nada
}
  NewParam('InfoString', tvpBase64);
  NewParam('TipoBusca', tvpInteger);

  NewResult('RetornoProxy', tvpBase64);
  NewResult('RetornoClass', tvpBase64);
  NewResult('NomeArquivoProxy', tvpString);
end;

function TRpDataFlashComandoList.DoRetornaCommandInfo: string;
var
  i: Integer;
  j: Integer;
  lFind: Boolean;
  lInfoComando: TRpDataFlashInfoCommand;
  lParam: TRpDataFlashParamInfoComando;
  p: Integer;
begin
  lFind := False;
  lInfoComando := TRpDataFlashInfoCommand.Create(nil);
  try
    for i := 0 to FGrupos.Count - 1 do
    begin
      if lFind then
        Break;

      for j := 0 to FGrupos[i].ListaComandos.Count - 1 do
      begin
        lFind := FGrupos[i].ListaComandos[j].FNome = FInfoString;
        if lFind then
        begin
          lInfoComando.NomeComando := FInfoString;
          for p := 0 to FGrupos[i].ListaComandos[j].ListaParametros.Count - 1 do
          begin
            lParam := lInfoComando.ListaParametros.AddParam;
            lParam.Nome := FGrupos[i].ListaComandos[j].ListaParametros[p].Nome;
            lParam.Tipo := FGrupos[i].ListaComandos[j].ListaParametros[p].TipoParametro;
            lParam.TipoValor := FGrupos[i].ListaComandos[j].ListaParametros[p].TipoValor;
          end;
          Break;
        end;
      end;
    end;
    Result := lInfoComando.SaveToXmlString;
  finally
    FreeAndNil(lInfoComando);
  end;
end;

function TRpDataFlashComandoList.DoRetornaCommandList: string;
begin
  Result := DoRetornaDataSetList;
end;

function TRpDataFlashComandoList.DoRetornaDataSetList: string;
var
  i: Integer;
  j: Integer;
  lLista: TStringList;
begin
  lLista := TStringList.Create;
  try
    for i := 0 to FGrupos.Count - 1 do
    begin
      for j := 0 to FGrupos[i].ListaComandos.Count - 1 do
        lLista.Append( FGrupos[i].ListaComandos[j].FNome );
    end;
    lLista.Sort;

    Result := Trim( lLista.Text );
  finally
    FreeAndNil( lLista );
  end;
end;

function TRpDataFlashComandoList.DoRetornaFactory: string;
var
  lClassesProxyModelo: TStringList;
  lLookupClass: string;
begin
  lLookupClass := GetLookupClassName;

  lClassesProxyModelo := TStringList.Create;
  try
    lClassesProxyModelo.Add('unit ' + C_DEFAULT_PROXY_NAME + ';');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add(DeclararComentarioDaUnit);
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('interface');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('uses');
    lClassesProxyModelo.Add('  SysUtils, uRpDataFlash.Command, uRpDataFlash.Components, uRpDataFlash.Types, ');
    lClassesProxyModelo.Add('  uRpSerialization, Windows, '
                           + IfThen(FConfigUnit <> EmptyStr, FConfigUnit + ', ') + 'ShellApi' + IfThen(FGerarClasses, ',', ';') );

    if FGerarClasses then
      lClassesProxyModelo.Add('  uRpFileHelper, ' + C_TMP_UNIT_CLASS + ';');

    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('type');
    lClassesProxyModelo.Add('  TBase64 = string;');
    lClassesProxyModelo.Add('  TConnectionType = (ctTcpIp, ctREST);');
    lClassesProxyModelo.Add('');
    if lLookupClass <> '' then
    begin
      lClassesProxyModelo.Add('  TServiceLookupFactory = class(TInterfacedObject, IDAOLookupFactory)');
      lClassesProxyModelo.Add('  public');
      lClassesProxyModelo.Add('    function GetXMLData(const pSQL: string): string;');
      lClassesProxyModelo.Add('    function GetConfigValues(const pConfigLookupClass: string;');
      lClassesProxyModelo.Add('      out ALookupKeys: string; out ALookupDisplay: string;');
      lClassesProxyModelo.Add('      out ALookupSQL: string; out ADAOClass: string): Boolean;');
      lClassesProxyModelo.Add('  end;');
      lClassesProxyModelo.Add('');
    end;

    lClassesProxyModelo.Add(DeclararGrupos);
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add(DeclararFactory);
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add(DeclararSingletonProxyFactory);
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add('implementation');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('var');
    lClassesProxyModelo.Add('  FProxyFactory : TProxyFactory;');
    lClassesProxyModelo.Add('  FProxyFactoryRest : TProxyFactory;');
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add( ImplementarSingletonProxyFactory );
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add( ImplementarGrupos );
    lClassesProxyModelo.Add('');

    lClassesProxyModelo.Add(ImplementarFactory);
    lClassesProxyModelo.Add('');
    if lLookupClass <> '' then
      lClassesProxyModelo.Add( ImplementarServiceLookupFactory );

    lClassesProxyModelo.Add( ImplementarFinalizationSection );
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('end.');

    Result := StringReplace(lClassesProxyModelo.Text, #$A#$D, #$D, [rfReplaceAll]);
  finally
    lClassesProxyModelo.Free;
  end;
end;

function TRpDataFlashComandoList.DoRetornaFactoryList: string;
var
  i: Integer;
  j: Integer;
  lCds : TClientDataSet;
begin
  lCds := TClientDataSet.Create(nil);
  try
    lCds.Close;
    lCds.FieldDefs.Clear;
    lCds.FieldDefs.Add('GRUPO',     ftString, 250);
    lCds.FieldDefs.Add('COMANDO',   ftString, 250);
    lCds.FieldDefs.Add('DESCRICAO', ftString, 1500);
    lCds.CreateDataSet;
    if not lCds.Active then
      lCds.Open;

    for i := 0 to FGrupos.Count - 1 do
      for j := 0 to FGrupos[i].ListaComandos.Count - 1 do
      begin
        lCds.Append;
        lCds.FieldByName('GRUPO').AsString := FGrupos[i].Nome;
        lCds.FieldByName('COMANDO').AsString := FGrupos[i].ListaComandos[j].Nome;

        if FGrupos[i].ListaComandos[j].Descricao <> EmptyStr then
          lCds.FieldByName('DESCRICAO').AsString := FGrupos[i].ListaComandos[j].Descricao
        else
          lCds.FieldByName('DESCRICAO').Clear;

        lCds.Post;
      end;
    Result := lCds.XMLData;
  finally
    FreeAndNil( lCds );
  end;
end;

function TRpDataFlashComandoList.GerarClassUnit: string;
var
  lDecode : TRpDataFlashClassSerialization;
begin
  lDecode := TRpDataFlashClassSerialization.Create;
  try
    try
      if lDecode.LoadXMLFromProxyCommands(FListaSelecionados) then
      begin

        if lDecode.GetClassMetadata <> EmptyStr then
        begin
          lDecode.LoadClassFromMetadata(lDecode.GetClassMetadata);
          Result := lDecode.GetImplementedUnit( C_TMP_UNIT_CLASS );
        end else
          Result := lDecode.GetErrorUnit('Nenhuma classe foi encontrada para ser gerada com as configurações ' + sLineBreak + ' proxy informadas.')
      end
      else
        Result := lDecode.GetErrorUnit('Não foi possível extrair o metadata.');
    except
      on E:Exception do
        Result := lDecode.GetErrorUnit(E.Message);
    end;
  finally
    FreeAndNil(lDecode);
  end;
end;

function TRpDataFlashComandoList.GerarProxy: string;
begin
  CarregaListaDeGruposRegistrados;

  if FTipoBusca = trpDataSetList then
    CarregaListaDeDataSetProviders
  else
    CarregaListaDeGruposDoController;

  case FTipoBusca of
    trpFactory:     Result := DoRetornaFactory;
    trpDataSetList: Result := DoRetornaDataSetList;
    trpCommandList: Result := DoRetornaCommandList;
    trpCommandInfo: Result := DoRetornaCommandInfo;
    trpFactoryList: Result := DoRetornaFactoryList;
  end;
end;

function TRpDataFlashComandoList.GetLookupClassName: string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to FGrupos.Count - 1 do
  begin
    if Pos('lookup', LowerCase(FGrupos[i].Nome) ) > 0 then
    begin
      // nome do grupo
      Result := FGrupos[i].Nome;
      Break;
    end;
  end;
end;

{ TProxyGruposList }

function TProxyGruposList.AddGrupo(const pNomeGrupo: string): TProxyGruposItem;
begin
  // procura por um grupo com o nome informado
  Result := Find(pNomeGrupo);
  if Result = nil then
  begin
    Result := TProxyGruposItem.Create;
    Result.Nome := pNomeGrupo;

    Add(Result);
  end;
end;

function TProxyGruposList.Find(const pNomeGrupo: string): TProxyGruposItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Self.Count - 1 do
    if Self[i].Nome = pNomeGrupo then
    begin
      Result := Self[i];
      Break;
    end;
end;

function TProxyGruposList.DeclararGrupos: string;
var
  i: Integer;
  lTexto: TStrings;
begin
  lTexto := TStringList.Create;
  try
    for i := 0 to Self.Count - 1 do
    begin
      // so gera se o grupo tem algum comando válido
      if Self[i].ListaComandos.Count > 0 then
      begin
        lTexto.Add('  ' + C_PREFIX_NAME + Self[i].Nome + ' = class(' + C_PROXY_BASE + ')');
        lTexto.Add('  public');
        lTexto.Add( Self[i].ListaComandos.DeclararComandos );
        lTexto.Add('  end;');
        lTexto.Add('');
      end;
    end;

    Result := TrimRight( lTexto.Text );
  finally
    FreeAndNil(lTexto);
  end;
end;

function TProxyGruposList.GetGrupos(const AIndex: Integer): TProxyGruposItem;
begin
  Result := TProxyGruposItem( inherited Get(AIndex) );
end;

function TProxyGruposList.ImplementarGrupos: string;
var
  i: Integer;
  lTexto: TStrings;
begin
  lTexto := TStringList.Create;
  try
    for i := 0 to Self.Count - 1 do
    begin
      lTexto.Add('{ ' + C_PREFIX_NAME + Self[i].Nome + ' }' );
      lTexto.Add('');
      lTexto.Add( Self[i].ListaComandos.ImplementarComandos(Self[i].Nome) );
      lTexto.Add('');
    end;
    Result := TrimRight( lTexto.Text );
  finally
    FreeAndNil(lTexto);
  end;
end;

procedure TProxyGruposList.RegistrarComando(
  const pRegistroComando: TRpDataFlashCustomDataSetProvider);
var
  lGrupo : TProxyGruposItem;
begin
  // busca o grupo do comando ou cria um novo
  lGrupo := AddGrupo( pRegistroComando.GroupName );

  if Assigned(lGrupo) then
  begin
    // retorna o comando
    lGrupo.ListaComandos.AddComando( pRegistroComando.Name, pRegistroComando.Description );
  end;
end;

procedure TProxyGruposList.RegistrarComando(const pRegistroComando: TRpDataFlashCommandItem;
  const pNomeGrupo : string);
var
  lGrupo : TProxyGruposItem;
  lComando : TProxyComandosItem;
begin
  // busca o grupo do comando ou cria um novo
  lGrupo := AddGrupo( pNomeGrupo );

  if Assigned(lGrupo) then
  begin
    // retorna o comando
    lComando := lGrupo.ListaComandos.AddComando( pRegistroComando.Name, pRegistroComando.Description );

    // adiciona os parametros do comando
    lComando.LoadParams( pRegistroComando.Params );
  end;
end;

procedure TProxyGruposList.RegistrarComando(const pRegistroComando: TTcpClassRegisterItem);
var
  lGrupo : TProxyGruposItem;
  lComando : TProxyComandosItem;

  function InternalGetDescription : string;
  var
    lCmd : IRpDataFlashCommandInterfaced;
  begin
    lCmd := TProxyClassSupport.GetInterfaceForClass( pRegistroComando.ProxyClass );
    if (lCmd <> nil) and (Assigned(lCmd.GetParams)) then
      Result := lCmd.GetDescription;
  end;

begin
  // busca o grupo do comando ou cria um novo
  lGrupo := AddGrupo(pRegistroComando.ProxyGroup);

  if Assigned(lGrupo) then
  begin
    // retorna o comando
    lComando := lGrupo.ListaComandos.AddComando(
      pRegistroComando.ProxyClass.ClassName,
      InternalGetDescription);
    // adiciona os parametros do comando
    lComando.LoadParams(pRegistroComando);
  end;
end;

{ TProxyGruposItem }

constructor TProxyGruposItem.Create;
begin
  FListaComandos := TProxyComandosList.Create(True);
end;

destructor TProxyGruposItem.Destroy;
begin
  if Assigned(FListaComandos) then
    FreeAndNil(FListaComandos);

  inherited Destroy;
end;

procedure TProxyGruposItem.SetNome(const Value: string);
begin
  FNome := Trim(Value);
end;

{ TProxyComandosItem }

constructor TProxyComandosItem.Create;
begin
  FListaParametros := TProxyParametroComandoList.Create(True);
end;

destructor TProxyComandosItem.Destroy;
begin
  if Assigned(FListaParametros) then
    FreeAndNil(FListaParametros);

  inherited Destroy;
end;

procedure TProxyComandosItem.LoadParams(const pListaParametros: TRpDataFlashParametrosCollection);
var
  lEnum: TCollectionEnumerator;
  lItem: TRpDataFlashParametroItem;
begin
  inherited;
  lEnum := pListaParametros.GetEnumerator;
  try
    while lEnum.MoveNext do
    begin
      lItem := TRpDataFlashParametroItem(lEnum.Current);
      ListaParametros.AddParametro( lItem );
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

procedure TProxyComandosItem.LoadParams(const pRegistroComando: TTcpClassRegisterItem);
var
  lCmd : IRpDataFlashCommandInterfaced;
  i : Integer;
begin
  lCmd := TProxyClassSupport.GetInterfaceForClass( pRegistroComando.ProxyClass );
  if (lCmd <> nil) and (Assigned(lCmd.GetParams)) then
  begin
    // cria a lista de parametros de entrada
    for i := 0 to lCmd.GetParams.Count - 1 do
      if lCmd.GetParams[i].ParamType <> tpInternal then
        ListaParametros.AddParametro(lCmd.GetParams[i]);
  end;
end;

procedure TProxyComandosItem.SetNome(const Value: string);
begin
  FNome := TProxyClassSupport.GetNomeComando( Value );
end;

{ TProxyComandosList }

function TProxyComandosList.AddComando(const pCommandName, pDescricaoComando: string): TProxyComandosItem;
var
  lName : string;
begin
  lName := TProxyClassSupport.GetNomeComando(pCommandName);

  Result := Find(lName);
  if Result = nil then
  begin
    Result := TProxyComandosItem.Create;
    Result.Nome := lName;
    Result.NomeClasse := pCommandName;
    Result.Descricao := pDescricaoComando;
    Add(Result);
  end;
end;

function TProxyComandosList.DeclararComandos: string;
var
  lTexto: TStringList;
  lComent : string;
  i: Integer;
begin
  lTexto := TStringList.Create;
  try
    for i := 0 to Self.Count - 1 do
    begin
      lComent := 'Comando: ' + Self[i].NomeClasse;
      if (Self[i].Descricao <> EmptyStr) and (Self[i].Descricao <> R_DATAFLASH_CMD_NO_DESCRIPTION) then
        lComent := lComent + '|' + Self[i].Descricao;

      lTexto.Add( FormatParamLine(4, '{ ' + lComent + ' }', ' ') );
      lTexto.Add(Self[i].ListaParametros.GetAsString(Self[i].Nome, 4, EmptyStr) );
    end;
    Result := TrimRight( lTexto.Text );
  finally
    FreeAndNil(lTexto);
  end;
end;

function TProxyComandosList.Find(const pCommandName: string): TProxyComandosItem;
var
  i: Integer;
  lName: string;
begin
  lName := TProxyClassSupport.GetNomeComando(pCommandName);

  Result := nil;
  for i := 0 to Self.Count - 1 do
    if Self[i].Nome = lName then
    begin
      Result := Self[i];
      Break;
    end;
end;

function TProxyComandosList.GetComandos(const AIndex: Integer): TProxyComandosItem;
begin
  Result := TProxyComandosItem(inherited Get(AIndex));
end;

function TProxyComandosList.ImplementarComandos(const pNomeGrupo : string): string;
var
  lTexto : TStringList;
  lParamList : string;
  i, j : Integer;
  lAsType : string;
  lValorFuncao: string;
  lNumeroRetornos: Integer;
  lEnviaArquivo: Boolean;
begin
  lTexto := TStringList.Create;
  try
    for i := 0 to Self.Count - 1 do
    begin
      lParamList := Self[i].ListaParametros.GetAsString( Self[i].Nome, 0, pNomeGrupo );
      lTexto.Add(lParamList);
      lTexto.Add('var');
      lTexto.Add('  lParametros : TRpDataFlashCommandParameterList;');
      lTexto.Add('begin');
      lTexto.Add('  lParametros := TRpDataFlashCommandParameterList.Create(nil);');
      lTexto.Add('  try');

      // para o tipo tvpBinaryFile, o arquivo deve ser enviado antes por FTP
      lEnviaArquivo := False;
      for j := 0 to Self[i].ListaParametros.Count - 1 do
        if  (Self[i].ListaParametros[j].TipoParametro in [tpInput, tpInputNoReload])
        and (Self[i].ListaParametros[j].TipoValor = tvpBinaryFile) then
        begin
          lEnviaArquivo := True;
          lTexto.Add('    FClient.PutFile(p' + Self[i].ListaParametros[j].Nome + ');');
        end;

      // passa os parâmetros para o TcpClient
      // lCmd.Parametros.Novo('VAL1', pVAL1, tpEntrada, tvpFloat);
      for j := 0 to Self[i].ListaParametros.Count - 1 do
        if Self[i].ListaParametros[j].TipoParametro in [tpInput, tpInputNoReload] then
        begin
          lValorFuncao := 'p' + Self[i].ListaParametros[j].Nome;

          if Self[i].ListaParametros[j].TipoValor in [tvpBase, tvpBase64, tvpDAO, tvpFile] then
            lValorFuncao := ' '' '' ';

          if (Self[i].ListaParametros[j].TipoValor = tvpBase) and (Self[i].ListaParametros[j].BaseClass <> EmptyStr) then
            lTexto.Add(Format('    lParametros.AddNew(''%s'', tpInput, ''%s'');', [
              Self[i].ListaParametros[j].Nome,
              Self[i].ListaParametros[j].BaseClass]) )
          else
            lTexto.Add(Format('    lParametros.AddNew(''%s'', %s, tpInput, %s);', [
              Self[i].ListaParametros[j].Nome,
              lValorFuncao,
              Self[i].ListaParametros[j].TipoValorAsString(False) ]) );

          case Self[i].ListaParametros[j].TipoValor of
            tvpBase64 :
              lTexto.Add('    lParametros.Param[''' + Self[i].ListaParametros[j].Nome + '''].AsBase64 := p' + Self[i].ListaParametros[j].Nome + ';');
            tvpBase,
            tvpDAO :
              lTexto.Add('    lParametros.Param[''' + Self[i].ListaParametros[j].Nome + '''].AsBase64 := p' + Self[i].ListaParametros[j].Nome + '.SaveToXmlString;');
            tvpFile:
              lTexto.Add('    lParametros.Param[''' + Self[i].ListaParametros[j].Nome + '''].AsBase64 := p' + Self[i].ListaParametros[j].Nome + '.Save;');
          end;
        end;
      // comunicar com o servidor
      lTexto.Add('    Result := DoEnviar(''' + Self[i].NomeClasse + ''', lParametros);');
      // depois de comunicar, percorre os parametros de saída
      lNumeroRetornos := 0;
      for j := 0 to Self[i].ListaParametros.Count - 1 do
      begin
        if Self[i].ListaParametros[j].TipoParametro = tpOutput then
        begin
          Inc(lNumeroRetornos);
          if lNumeroRetornos = 1 then
          begin
            lTexto.Add('    if Result then');
            lTexto.Add('    begin');
          end;

          lAsType := Self[i].ListaParametros[j].TipoValorAsString;

          case Self[i].ListaParametros[j].TipoValor of
          tvpBase,
          tvpDAO :
            begin
              lTexto.Add(Format('      %s.LoadFromXmlString(lParametros.ResultParam[''%s''].AsBase64);', [
                'A' + Self[i].ListaParametros[j].Nome,
                Self[i].ListaParametros[j].Nome]));
            end;
          tvpFile:
            begin
              lTexto.Add(Format('      %s.Load(lParametros.ResultParam[''%s''].AsBase64);', [
                'A' + Self[i].ListaParametros[j].Nome,
                Self[i].ListaParametros[j].Nome]));
            end
          else
            begin
              case Self[i].ListaParametros[j].TipoValor of
                tvpBase64 : lAsType := 'Base64';
                tvpFloat : lAsType := 'Float';
                tvpDateTime : lAsType := 'DateTime';
              end;

              lTexto.Add(Format('      %s := lParametros.ResultParam[''%s''].As%s;', [
                'A' + Self[i].ListaParametros[j].Nome,
                Self[i].ListaParametros[j].Nome,
                lAsType]) );
            end;
          end;
        end
        else
        begin
          if (Self[i].ListaParametros[j].TipoParametro <> tpInputNoReload)
          and (Self[i].ListaParametros[j].TipoValor in [tvpBase, tvpDAO]) then
          begin
            Inc(lNumeroRetornos);
            if lNumeroRetornos = 1 then
            begin
              lTexto.Add('    if Result then');
              lTexto.Add('    begin');
            end;
            lTexto.Add(Format('      %s.LoadFromXmlString(lParametros.ResultParam[''%s''].AsBase64);', [
              'p' + Self[i].ListaParametros[j].Nome,
              Self[i].ListaParametros[j].Nome]));
          end;
        end;
      end;

      if lNumeroRetornos > 0 then
        lTexto.Add('    end;');

      lTexto.Add('  finally');
      lTexto.Add('    FreeAndNil(lParametros);');
      lTexto.Add('  end;');

      if lEnviaArquivo then
      begin
        lTexto.Add('    if Result then');
        lTexto.Add('    try');
        // para o tipo tvpBinaryFile, envia confirmacao de recebimento para o servidor
        for j := 0 to Self[i].ListaParametros.Count - 1 do
          if  (Self[i].ListaParametros[j].TipoParametro in [tpInput, tpInputNoReload])
          and (Self[i].ListaParametros[j].TipoValor = tvpBinaryFile) then
          begin
            lTexto.Add('      FClient.ConfirmFileReceipt(p' + Self[i].ListaParametros[j].Nome + '.FileID, True, False, nil);');
          end;
        lTexto.Add('    except');
        lTexto.Add('      Sleep(1);');
        lTexto.Add('    end;');
      end;

      lTexto.Add('end;');
      lTexto.Add('');
    end;

    Result := TrimRight(lTexto.Text);
  finally
    FreeAndNil(lTexto);
  end;
end;

{ TProxyParametroComandoList }

procedure TProxyParametroComandoList.AddParametro(const PParams: TRpDataFlashCommandParameter);
var
  lParametro : TProxyParametroComandoItem;
begin
  lParametro := TProxyParametroComandoItem.Create;

  lParametro.Nome := PParams.Name;
  lParametro.TipoParametro := PParams.ParamType;
  lParametro.TipoValor := PParams.ValueType;
  lParametro.BaseClass := PParams.BaseClass;

  Self.Add( lParametro );
end;

procedure TProxyParametroComandoList.AddParametro(const PParams: TRpDataFlashParametroItem);
var
  lParametro : TProxyParametroComandoItem;
begin
  lParametro := TProxyParametroComandoItem.Create;

  lParametro.Nome := PParams.Name;
  lParametro.TipoParametro := PParams.Tipo;
  lParametro.TipoValor := PParams.TipoValor;
  lParametro.BaseClass := PParams.BaseClass;

  Self.Add( lParametro );
end;

function TProxyParametroComandoList.GetAsString(const pCommandName : string;
  const pIdentCount: Smallint; const pNomeGrupo : string): string;
var
  lMember: string;
begin
  Result := GetListaParametros;

  if pNomeGrupo <> EmptyStr then
    lMember := C_PREFIX_NAME + pNomeGrupo + '.'
  else
    lMember := EmptyStr;

  Result := 'function ' + lMember + pCommandName + IfThen(Result <> EmptyStr, '(' + Result +')' ) + ' : Boolean;';
  Result := FormatParamLine(pIdentCount, Result, ';' );
end;

function TProxyParametroComandoList.GetListaParametros: string;
var
  i: Integer;
  lAux: string;
begin
  Result := EmptyStr;
  // cria a lista de parametros de entrada
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].TipoParametro in [tpInput, tpInputNoReload] then
      Result := Result + '; const p' + Self[i].Nome + ': ' + Self[i].TipoValorAsString;
  end;

  // cria a lista de parametros de entrada
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].TipoParametro = tpOutput then
    begin
      if Self[i].TipoValor in [tvpFile, tvpBinaryFile] then
        lAux := ''
      else
        lAux := 'out ';

      Result := Result + '; ' + lAux + 'A' + Self[i].Nome + ': ' + Self[i].TipoValorAsString;
    end;
  end;

  if Result <> EmptyStr then
    System.Delete(Result, 1, 2);
end;

function TProxyParametroComandoList.GetParametros(
  const AIndex: Integer): TProxyParametroComandoItem;
begin
  Result := TProxyParametroComandoItem(inherited Get(AIndex));
end;

{ TProxyParametroComandoItem }

function TProxyParametroComandoItem.TipoValorAsString(const pNatural: Boolean): string;
begin
  case Self.TipoValor of
    tvpInteger:  Result := IfThen(pNatural, 'Integer',      'tvpInteger');
    tvpString:   Result := IfThen(pNatural, 'String',       'tvpString');
    tvpBoolean:  Result := IfThen(pNatural, 'Boolean',      'tvpBoolean');
    tvpFloat:    Result := IfThen(pNatural, 'Double',       'tvpFloat');
    tvpBase64:   Result := IfThen(pNatural, 'TBase64',      'tvpBase64');
    tvpDAO:      Result := IfThen(pNatural, 'TDAO',         'tvpDAO');
    tvpBase:     Result := IfThen(pNatural, Self.BaseClass, 'tvpBase');
    tvpDateTime: Result := IfThen(pNatural, 'TDateTime',    'tvpDateTime');
    tvpFile:     Result := IfThen(pNatural, 'IFileProxy',   'tvpFile');
  end
end;

{ TProxyClassSupport }

class function TProxyClassSupport.GetInterfaceForClass(
  const pClass: TRpDataFlashAbstractClass): IRpDataFlashCommandInterfaced;
begin
  if (pClass <> nil) then
  begin
    if Supports(pClass, IRpDataFlashCommandInterfaced) then
    begin
      if pClass.InheritsFrom(TComponent) then
        Result := (TComponentClass(pClass).Create(nil) as IRpDataFlashCommandInterfaced)
      else
        if pClass.InheritsFrom(TRpDataFlashCommand) then
          Result := (TRpDataFlashCommandClass(pClass).Create as IRpDataFlashCommandInterfaced)
        else
          Result  := (TRpDataFlashCommand(pClass.Create) as IRpDataFlashCommandInterfaced);
    end;
  end;
end;

class function TProxyClassSupport.GetNomeComando(const AClassName: string): string;
begin
  Result := Trim(StringReplace(AClassName, C_BASE_CLASS_NAME,   EmptyStr, [rfIgnoreCase]));
  Result := Trim(StringReplace(AClassName, TProxyClassSupport.PrefixoCmd, EmptyStr, [rfIgnoreCase]));
end;

{ TProxyListaComandosSelecionados }

function TProxyListaComandosSelecionados.ComandoSelecionado(const AGrupo,
  AComando: string): Boolean;
var
  lComando : string;
begin
  lComando := TProxyClassSupport.GetNomeComando(AComando);

  Result := (Self.Count <= 0)
         or (Self.IndexOf( AGrupo + '|' + lComando ) > -1);
end;

function TProxyListaComandosSelecionados.GrupoValido(const AGrupo: string): Boolean;
begin
  Result := (Self.Count <= 0)
         or (Pos(AGrupo + '|', Self.Text) > 0);
end;

initialization
  TCPClassRegistrer.Registrar(TRpDataFlashComandoList, C_GROUP_INTERNAL);

end.
