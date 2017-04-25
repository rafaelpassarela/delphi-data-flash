unit uLRDataFlashProxyGenerator;

interface

uses
  Classes, uLRDataFlashComando, SysUtils, Contnrs, uLRDataFlashTypes, uLRDataFlashComponent, 
  uLRDataFlashComandController;

const
  C_PREFIX_NAME = 'TProxy';
  C_PROXY_BASE = 'TCustomProxy';
  C_BASE_CLASS_NAME = 'TLRDataFlashComando';

type
  TProxyParametroComandoItem = class
  private
    FNome: string;
    FTipoParametro: TLRDataFlashTipoParametro;
    FTipoValor: TLRDataFlashTipoValorParametro;
  public
    property Nome : string read FNome write FNome;
    property TipoParametro : TLRDataFlashTipoParametro read FTipoParametro write FTipoParametro;
    property TipoValor : TLRDataFlashTipoValorParametro read FTipoValor write FTipoValor;
    function TipoValorAsString(const pNatural : Boolean = True) : string;
  end;

  TProxyParametroComandoList = class(TObjectList)
  private
    function GetParametros(const AIndex: Integer): TProxyParametroComandoItem;
    function GetListaParametros : string;
  public
    property Parametros[const AIndex : Integer] : TProxyParametroComandoItem read GetParametros; default;
    procedure AddParametro(const PParams : TLRDataFlashParametroComando); overload;
    procedure AddParametro(const PParams : TLRDataFlashParametroItem); overload;
    function GetAsString(const pCommandName : string; const pIdentCount : Smallint;
      const pNomeGrupo : string) : string;
  end;

  TProxyComandosItem = class
  private
    FNome: string;
    FListaParametros: TProxyParametroComandoList;
    FNomeClasse: string;
    FDescricao: string;
    procedure SetNome(const Value: string);
    function GetInterfaceForClass(const pClass: TLRDataFlashAbstractClass): IComandoTCPInterfaced;
  public
    constructor Create;
    destructor Destroy; override;
    property Nome : string read FNome write SetNome;
    property NomeClasse : string read FNomeClasse write FNomeClasse;
    property ListaParametros : TProxyParametroComandoList read FListaParametros write FListaParametros;
    property Descricao : string read FDescricao write FDescricao;
    procedure LoadParams(const pRegistroComando: TTcpClassRegisterItem); overload;
    procedure LoadParams(const pListaParametros: TLRDataFlashParametrosCollection); overload;
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
    procedure RegistrarComando(const pRegistroComando : TLRDataFlashComandItem;
      const pNomeGrupo : string); overload;

    function DeclararGrupos : string;
    function ImplementarGrupos : string;
  end;

  TLRDataFlashComandoList = class(TLRDataFlashComando)
  private
    FGrupos : TProxyGruposList;
    procedure CarregaListaDeGruposRegistrados;
    procedure CarregaListaDeGruposDoController;
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
  protected
    function GerarProxy(const pUnitName : string) : string;
    procedure DoRegistrarParametros; override;
    function DoExecutar : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  StrUtils, Math;

function GetNomeComando(const AClassName: string): string;
begin
  Result := Trim(StringReplace(AClassName, C_BASE_CLASS_NAME, '', [rfIgnoreCase]));
end;

function FormatParamLine(const pStartIdent : Byte; const pLine : string; const pDelimiter : Char) : string;
var
  lTxt : TStringList;
  i: Integer;
begin
  Result := StringOfChar(' ', pStartIdent) + pLine;
  lTxt := TStringList.Create;
  // quebra em 110 colunas
  lTxt.LineBreak := '|';
  lTxt.Text := WrapText(Result, lTxt.LineBreak, [pDelimiter], 100);

  // para cada linha corrige a identacao
  for i := 0 to lTxt.Count - 1 do
    lTxt[i] := StringOfChar(' ', pStartIdent + (IfThen(i <> 0, 2))) + Trim(lTxt[i]);

  lTxt.LineBreak := #$A#$D;
  Result := TrimRight(lTxt.Text);
end;

function TLRDataFlashComandoList.DeclararGrupos: string;
var
  lClassesProxyModelo: TStringList;
begin
  lClassesProxyModelo := TStringList.Create;

  // cria a classe base para os "TProxy"
  lClassesProxyModelo.Add('  ' + C_PROXY_BASE + ' = class');
  lClassesProxyModelo.Add('  protected');
  lClassesProxyModelo.Add('    FTcpClient: TLRDataFlashConexaoCliente;');
  lClassesProxyModelo.Add('    FLastError: string;');
  lClassesProxyModelo.Add('    FStatusProcessamento: TLRDataFlashStatusProcessamento;');
  lClassesProxyModelo.Add('  public');
  lClassesProxyModelo.Add('    constructor Create(ATcpClient: TLRDataFlashConexaoCliente);');
  lClassesProxyModelo.Add('    function GetLastError : string;');
  lClassesProxyModelo.Add('    function GetStatusProcessamento : TLRDataFlashStatusProcessamento;');
  lClassesProxyModelo.Add('  end;');
  lClassesProxyModelo.Add('');

  lClassesProxyModelo.Add( FGrupos.DeclararGrupos );

  Result := TrimRight( lClassesProxyModelo.Text );
  FreeAndNil(lClassesProxyModelo);
end;

function TLRDataFlashComandoList.ImplementarServiceLookupFactory: string;
var
  lService: TStringList;
  lLookuop: string;
begin
  lLookuop := GetLookupClassName;

  lService := TStringList.Create;
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
  FreeAndNil( lService );
end;

function TLRDataFlashComandoList.DeclararComentarioDaUnit: string;
var
  lHint : TStringList;
begin
  lHint := TStringList.Create;
  lHint.Add('//   Não modifique esta Unit, seu código é gerado automaticamente pelo Cliente de');
  lHint.Add('// TCP buscando as classes de serviço registradas no servidor.');
  Result := TrimRight(lHint.Text);
  FreeAndNil(lHint);
end;

function TLRDataFlashComandoList.ImplementarFinalizationSection: string;
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
    lImplement.Add('');
//    lImplement.Add('end.');

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TLRDataFlashComandoList.ImplementarGrupos: string;
var
  lImplementFuncoes: TStringList;
begin
  lImplementFuncoes := TStringList.Create;

  lImplementFuncoes.Add('{ ' + C_PROXY_BASE + ' }' );
  lImplementFuncoes.Add('');
  lImplementFuncoes.Add('constructor ' + C_PROXY_BASE + '.Create(ATcpClient: TLRDataFlashConexaoCliente);');
  lImplementFuncoes.Add('begin');
  lImplementFuncoes.Add('  FTcpClient := ATcpClient;');
  lImplementFuncoes.Add('  FLastError := EmptyStr;');
  lImplementFuncoes.Add('end;');
  lImplementFuncoes.Add('');
  lImplementFuncoes.Add('function ' + C_PROXY_BASE + '.GetLastError : string;');
  lImplementFuncoes.Add('begin');
  lImplementFuncoes.Add('  Result := FLastError;');
  lImplementFuncoes.Add('  if Pos(''Sem conex'', Result) > 0 then');
  lImplementFuncoes.Add('    Result := ''Sem conexão com o serviço.'';');
  lImplementFuncoes.Add('end;');
  lImplementFuncoes.Add('');
  lImplementFuncoes.Add('function ' + C_PROXY_BASE + '.GetStatusProcessamento : TLRDataFlashStatusProcessamento;');
  lImplementFuncoes.Add('begin');
  lImplementFuncoes.Add('  Result := FStatusProcessamento;');
  lImplementFuncoes.Add('end;');
  lImplementFuncoes.Add('');

  lImplementFuncoes.Add( FGrupos.ImplementarGrupos );

  Result := TrimRight( lImplementFuncoes.Text );
  FreeAndNil(lImplementFuncoes);
end;

function TLRDataFlashComandoList.ImplementarFactory: string;
var
  lImplement : TStringList;
  I: Integer;
begin
  lImplement := TStringList.Create;

  try
    lImplement.Add('{ TProxyFactory }');
    lImplement.Add('');
    lImplement.Add('constructor TProxyFactory.Create(ATcpClient : TLRDataFlashConexaoCliente);');

    lImplement.Add('begin');
    lImplement.Add('  FSharedClient := Assigned(ATcpClient);');
    lImplement.Add('  if FSharedClient then');
    lImplement.Add('    FTcpClient := ATcpClient');
    lImplement.Add('  else');
    lImplement.Add('  begin');
    lImplement.Add('    lFileConf := TFileControlConfig.Create;');
    lImplement.Add('    try');
    lImplement.Add('      FTcpClient := TLRDataFlashConexaoCliente.Create(nil);');
    // se criar o client, sempre fica como "localhost"
    lImplement.Add('      FTcpClient.Servidor := ''localhost'';       //lFileConf.ServerName;');
    lImplement.Add('      FTcpClient.Porta := lFileConf.ServerPort; //ClientPort');
    lImplement.Add('      FTcpClient.TipoCriptografia := tcBase64Compressed;');
    lImplement.Add('      FTcpClient.TipoComunicacao := lFileConf.ModoComunicacao;');
    lImplement.Add('    finally');
    lImplement.Add('      FreeAndNil(lFileConf);');
    lImplement.Add('    end;');
    lImplement.Add('  end;');
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

    lImplement.Add('  if Assigned(FTcpClient) and (not FSharedClient) then');
    lImplement.Add('  begin');
    lImplement.Add('    try');
    lImplement.Add('      if FTcpClient.Conectado then');
    lImplement.Add('        FTcpClient.Desconectar;');
    lImplement.Add('    finally');
    lImplement.Add('      FreeAndNil(FTcpClient);');
    lImplement.Add('    end;');
    lImplement.Add('  end;');
    lImplement.Add('  inherited;');
    lImplement.Add('end;');
    lImplement.Add('');

    if GetLookupClassName <> '' then
    begin
      lImplement.Add('function TProxyFactory.GetServiceLookupFactory: TServiceLookupFactory;');
      lImplement.Add('begin');
      lImplement.Add('  Result := TServiceLookupFactory.Create;');
      lImplement.Add('end;');
      lImplement.Add('');
    end;

    // Gerar Implementacoes de Classes
    for i := 0 to FGrupos.Count - 1 do
    begin
      lImplement.Add(Format('function TProxyFactory.%s: TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome ]));
      lImplement.Add(       'begin');
      lImplement.Add(Format('  if FProxy%s = nil then', [FGrupos[i].Nome ]));
      lImplement.Add(Format('    FProxy%s := TProxy%s.Create(FTcpClient);', [FGrupos[i].Nome, FGrupos[i].Nome]));
      lImplement.Add(Format('  Result := FProxy%s;', [FGrupos[i].Nome ]));
      lImplement.Add(       'end;');
      lImplement.Add('');
    end;

    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TLRDataFlashComandoList.ImplementarSingletonProxyFactory: string;
var
  lImplement : TStringList;
begin
  lImplement := TStringList.Create;
  try
    lImplement.Add('function ProxyFactory(ATcpClient : TLRDataFlashConexaoCliente) : TProxyFactory;');
    lImplement.Add('begin');
    lImplement.Add('  if FProxyFactory = nil then');
    lImplement.Add('    FProxyFactory := TProxyFactory.Create(ATcpClient);');
    lImplement.Add('  Result := FProxyFactory;');
    lImplement.Add('end;');
    lImplement.Add('');
    lImplement.Add('procedure NewProxyFactory(out AProxyFactory : TProxyFactory);');
    lImplement.Add('begin');
    lImplement.Add('  AProxyFactory := TProxyFactory.Create(nil);');
    lImplement.Add('end;');
    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TLRDataFlashComandoList.DeclararFactory: string;
var
  lImplement : TStringList;
  i: Integer;
  lLookup : string;
begin
  lLookup := GetLookupClassName;
  lImplement := TStringList.Create;

  try
    lImplement.Add('  TProxyFactory = class');

    if lLookup <> '' then
    begin
      lImplement.Add('  protected');
      lImplement.Add('    function GetServiceLookupFactory : TServiceLookupFactory;');
    end;

    lImplement.Add('  private');
    lImplement.Add('    FTcpClient : TLRDataFlashConexaoCliente;');	
    lImplement.Add('    FSharedClient: Boolean;');

    // Gerar os fields conforme Classes de proxy ( FProxyVenda : TProxyVenda; )
    for i := 0 to FGrupos.Count - 1 do
      lImplement.Add(Format('    FProxy%s : TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome] ));

    lImplement.Add('  public');
    lImplement.Add('    constructor Create(ATcpClient : TLRDataFlashConexaoCliente);');
    lImplement.Add('    destructor Destroy; override;');
    if lLookup <> '' then
      lImplement.Add('    property ServiceLookupFactory : TServiceLookupFactory read GetServiceLookupFactory;');

    // Gerar os métodos conforme Classes de proxy
    for I := 0 to FGrupos.Count - 1 do
      lImplement.Add(Format('    function %s : TProxy%s;', [FGrupos[i].Nome, FGrupos[i].Nome]));

    lImplement.Add('  end;');
    Result := TrimRight(lImplement.Text);
  finally
    FreeAndNil(lImplement);
  end;
end;

function TLRDataFlashComandoList.DeclararSingletonProxyFactory: string;
var
  lList : TStringList;
begin
  lList := TStringList.Create;
  lList.Add('function ProxyFactory(ATcpClient : TLRDataFlashConexaoCliente = nil) : TProxyFactory;');
  lList.Add('procedure NewProxyFactory(out AProxyFactory : TProxyFactory);');

  Result := TrimRight( lList.Text );

  FreeAndNil(lList);
end;

{ TLRDataFlashComandoList }

procedure TLRDataFlashComandoList.CarregaListaDeGruposDoController;
var
  lEnumGrupos: TListEnumerator;
  lEnumComandos: TCollectionEnumerator;

  lController: TLRDataFlashComandController;
  lItemComando: TLRDataFlashComandItem;
begin
  // registrar os comandos do Collection
  if GetServer.UtilizarControllers then
  begin
    // pega os controllers do servidor
    lEnumGrupos := GetServer.Controllers.GetEnumerator;
    while (lEnumGrupos.MoveNext) do
    begin
      lController := TLRDataFlashComandController(lEnumGrupos.Current);
      lEnumComandos := lController.Comandos.GetEnumerator;

      while lEnumComandos.MoveNext do
      begin
        lItemComando := TLRDataFlashComandItem(lEnumComandos.Current);
        FGrupos.RegistrarComando(lItemComando, lController.Grupo);
      end;
    end;

    FreeAndNil(lEnumComandos);
    FreeAndNil(lEnumGrupos);
  end;
end;

procedure TLRDataFlashComandoList.CarregaListaDeGruposRegistrados;
var
  lRegistrados: TTcpClassRegister;
  i: Integer;
begin
  lRegistrados := nil;
  TCPClassRegistrer.Registrados(lRegistrados);

  // percorre todos os comandos registrados e envia para o grupo registrar
  for i := 0 to lRegistrados.Count - 1 do
    if  (lRegistrados[i].ProxyGroup <> C_GRUPO_INTERNO)
    and (lRegistrados[i].ProxyGroup <> C_GRUPO_DATASET) then
      FGrupos.RegistrarComando( lRegistrados[i] );

  FreeAndNil(lRegistrados);
end;

constructor TLRDataFlashComandoList.Create;
begin
  inherited Create;
  FGrupos := TProxyGruposList.Create(True);
end;

destructor TLRDataFlashComandoList.Destroy;
begin
  if Assigned(FGrupos) then
    FreeAndNil(FGrupos);

  inherited Destroy;
end;

function TLRDataFlashComandoList.DoExecutar: Boolean;
begin
  if Parametro['UnitName'].AsString = '' then
    Retorno['NomeArquivoProxy'].AsString := 'uClassesProxyGenerator'
  else
    Retorno['NomeArquivoProxy'].AsString := Parametro['UnitName'].AsString;

  try
    Retorno['ArquivoProxy'].AsBase64 := GerarProxy(Retorno['NomeArquivoProxy'].AsString);
    Result := True;
  except
    on E:Exception do
      raise Exception.Create('Erro gerando arquivo proxy. ' + E.Message );
  end;
end;

procedure TLRDataFlashComandoList.DoRegistrarParametros;
begin
  inherited;
  NovoParametro('UnitName', tvpString);
  NovoRetorno('ArquivoProxy', tvpBase64);
  NovoRetorno('NomeArquivoProxy', tvpString);
end;

function TLRDataFlashComandoList.GerarProxy(const pUnitName: string): string;
var
  lClassesProxyModelo: TStringList;
  lLookupClass: string;
begin
  CarregaListaDeGruposRegistrados;
  CarregaListaDeGruposDoController;
  
  lLookupClass := GetLookupClassName;

  lClassesProxyModelo := TStringList.Create;
  try
    lClassesProxyModelo.Add('unit ' + pUnitName + ';');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add(DeclararComentarioDaUnit);
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('interface');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('uses');
    lClassesProxyModelo.Add('  uLRDataFlashComando, uLRDataFlashComponent, SysUtils, uLRDataFlashTypes; ');
    lClassesProxyModelo.Add('');
    lClassesProxyModelo.Add('type');
    lClassesProxyModelo.Add('  TBase64 = string;');
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

function TLRDataFlashComandoList.GetLookupClassName: string;
var
  i: Integer;
begin
  Result := '';
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
  FreeAndNil(lTexto);
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

  for i := 0 to Self.Count - 1 do
  begin
    lTexto.Add('{ ' + C_PREFIX_NAME + Self[i].Nome + ' }' );
    lTexto.Add('');
    lTexto.Add( Self[i].ListaComandos.ImplementarComandos(Self[i].Nome) );
    lTexto.Add('');
  end;

  Result := TrimRight( lTexto.Text );
  FreeAndNil(lTexto);
end;

procedure TProxyGruposList.RegistrarComando(const pRegistroComando: TLRDataFlashComandItem;
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
    lComando := lGrupo.ListaComandos.AddComando( pRegistroComando.Nome, pRegistroComando.Descricao );

    // adiciona os parametros do comando
    lComando.LoadParams( pRegistroComando.Parametros );
  end;
end;

procedure TProxyGruposList.RegistrarComando(const pRegistroComando: TTcpClassRegisterItem);
var
  lGrupo : TProxyGruposItem;
  lComando : TProxyComandosItem;
begin
  // busca o grupo do comando ou cria um novo
  lGrupo := AddGrupo(pRegistroComando.ProxyGroup);

  if Assigned(lGrupo) then
  begin
    // retorna o comando
    lComando := lGrupo.ListaComandos.AddComando( pRegistroComando.ProxyClass.ClassName, '');
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

function TProxyComandosItem.GetInterfaceForClass(const pClass: TLRDataFlashAbstractClass): IComandoTCPInterfaced;
//var
//  lObjeto: TObject;
begin
  if (pClass <> nil) then
  begin
    if Supports(pClass, IComandoTCPInterfaced) then
    begin
      if pClass.InheritsFrom(TComponent) then
        Result := (TComponentClass(pClass).Create(nil) as IComandoTCPInterfaced)
      else
        if pClass.InheritsFrom(TLRDataFlashComando) then
          Result := (TLRDataFlashComandoClass(pClass).Create as IComandoTCPInterfaced)
        else
          Result  := (TLRDataFlashComando(pClass.Create) as IComandoTCPInterfaced);
    end;
  end;
end;

procedure TProxyComandosItem.LoadParams(const pListaParametros: TLRDataFlashParametrosCollection);
var
  lEnum: TCollectionEnumerator;
  lItem: TLRDataFlashParametroItem;
begin
  inherited;
  lEnum := pListaParametros.GetEnumerator;
  try
    while lEnum.MoveNext do
    begin
      lItem := TLRDataFlashParametroItem(lEnum.Current);
      ListaParametros.AddParametro( lItem );
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

procedure TProxyComandosItem.LoadParams(const pRegistroComando: TTcpClassRegisterItem);
var
  lCmd : IComandoTCPInterfaced;
  i : Integer;
begin
  lCmd := GetInterfaceForClass( pRegistroComando.ProxyClass );
  if (lCmd <> nil) and (Assigned(lCmd.GetParametros)) then
  begin
    // cria a lista de parametros de entrada
    for i := 0 to lCmd.GetParametros.Count - 1 do
      if lCmd.GetParametros[i].Tipo <> tpInterno then
        ListaParametros.AddParametro(lCmd.GetParametros[i]);
  end;
end;

procedure TProxyComandosItem.SetNome(const Value: string);
begin
  FNome := GetNomeComando( Value );
end;

{ TProxyComandosList }

function TProxyComandosList.AddComando(const pCommandName, pDescricaoComando: string): TProxyComandosItem;
var
  lName : string;
begin
  lName := GetNomeComando(pCommandName);

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
  i: Integer;
begin
  lTexto := TStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    lTexto.Add('    //' + Self[i].NomeClasse);
    if Self[i].Descricao <> '' then
      lTexto.Add( FormatParamLine(4, '{ ' + Self[i].Descricao + ' }', ' ') );
    lTexto.Add(Self[i].ListaParametros.GetAsString(Self[i].Nome, 4, '') );
  end;
  Result := TrimRight( lTexto.Text );
  FreeAndNil(lTexto);
end;

function TProxyComandosList.Find(const pCommandName: string): TProxyComandosItem;
var
  i: Integer;
  lName: string;
begin
  lName := GetNomeComando(pCommandName);

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
begin
  lTexto := TStringList.Create;
  for i := 0 to Self.Count - 1 do
  begin
    lParamList := Self[i].ListaParametros.GetAsString( Self[i].Nome, 0, pNomeGrupo );
    lTexto.Add(lParamList);
    lTexto.Add('var');
    lTexto.Add('  lCmd : TLRDataFlashComandoEnvio;');
    lTexto.Add('begin');
    lTexto.Add('  FLastError := EmptyStr;');
    lTexto.Add('  FStatusProcessamento := tspNenhum;');
    lTexto.Add('  lCmd := TLRDataFlashComandoEnvio.Create;');
    lTexto.Add('  try');
    lTexto.Add('    lCmd.SetComando(''' + Self[i].NomeClasse + ''');');

    // passa os parâmetros para o TcpClient
    // lCmd.Parametros.Novo('VAL1', pVAL1, tpEntrada, tvpFloat);
    for j := 0 to Self[i].ListaParametros.Count - 1 do
      if Self[i].ListaParametros[j].TipoParametro in [tpEntrada, tpEntradaSemRecaregar] then
      begin
        lValorFuncao := 'p' + Self[i].ListaParametros[j].Nome;

        if Self[i].ListaParametros[j].TipoValor in [tvpBase, tvpBase64, tvpDAO] then
          lValorFuncao := ' '' '' ';

        lTexto.Add(Format('    lCmd.Parametros.Novo(''%s'', %s, tpEntrada, %s);', [
          Self[i].ListaParametros[j].Nome,
          lValorFuncao,
          Self[i].ListaParametros[j].TipoValorAsString(False) ]) );

        case Self[i].ListaParametros[j].TipoValor of
          tvpBase64 :
            lTexto.Add('    lCmd.Parametro[''' + Self[i].ListaParametros[j].Nome + '''].AsBase64 := p' + Self[i].ListaParametros[j].Nome + ';');
          tvpBase,
          tvpDAO :
            lTexto.Add('    lCmd.Parametro[''' + Self[i].ListaParametros[j].Nome + '''].AsBase64 := p' + Self[i].ListaParametros[j].Nome + '.SaveToXmlString;');
        end;
      end;
    // comunicar com o servidor
    lTexto.Add('    FTcpClient.Comunicar(lCmd);');

    // depois de comunicar, percorre os parametros de saída
    for j := 0 to Self[i].ListaParametros.Count - 1 do
    begin
      if Self[i].ListaParametros[j].TipoParametro = tpSaida then
      begin
        lAsType := Self[i].ListaParametros[j].TipoValorAsString;

        case Self[i].ListaParametros[j].TipoValor of
          tvpBase64 : lAsType := 'Base64';
          tvpFloat : lAsType := 'Float';
          tvpDateTime : lAsType := 'DateTime';
        end;  

        if Self[i].ListaParametros[j].TipoValor in [tvpBase, tvpDAO] then
        begin
          lTexto.Add(Format('    %s.LoadFromXmlString(lCmd.Parametro[''%s''].AsBase64);', [
            'A' + Self[i].ListaParametros[j].Nome,
            Self[i].ListaParametros[j].Nome]));
        end
        else
        begin
          lTexto.Add(Format('    %s := lCmd.Retorno[''%s''].As%s;', [
            'A' + Self[i].ListaParametros[j].Nome,
            Self[i].ListaParametros[j].Nome,
            lAsType]) );
        end;
      end
      else
      begin
        if (Self[i].ListaParametros[j].TipoParametro <> tpEntradaSemRecaregar)
        and (Self[i].ListaParametros[j].TipoValor in [tvpBase, tvpDAO]) then
          lTexto.Add(Format('    %s.LoadFromXmlString(lCmd.Parametro[''%s''].AsBase64);', [
            'p' + Self[i].ListaParametros[j].Nome,
            Self[i].ListaParametros[j].Nome]));
      end;
    end;

    lTexto.Add('    Result := lCmd.StatusRetorno;');
    lTexto.Add('    FStatusProcessamento := lCmd.StatusProcessamento;');
    lTexto.Add('    if not Result then');
    lTexto.Add('      FLastError := lCmd.LastError;');
    lTexto.Add('  except');
    lTexto.Add('    on E:Exception do');
    lTexto.Add('    begin');
    lTexto.Add('      Result := False;');
    lTexto.Add('      FLastError := E.Message;');
    lTexto.Add('    end;');
    lTexto.Add('  end;');
    lTexto.Add('  FreeAndNil(lCmd);');
    lTexto.Add('end;');
    lTexto.Add('');
  end;

  Result := TrimRight(lTexto.Text);
  FreeAndNil(lTexto);
end;

{ TProxyParametroComandoList }

procedure TProxyParametroComandoList.AddParametro(const PParams: TLRDataFlashParametroComando);
var
  lParametro : TProxyParametroComandoItem;
begin
  lParametro := TProxyParametroComandoItem.Create;

  lParametro.Nome := PParams.Nome;
  lParametro.TipoParametro := PParams.Tipo;
  lParametro.TipoValor := PParams.TipoValor;

  Self.Add( lParametro );
end;

procedure TProxyParametroComandoList.AddParametro(const PParams: TLRDataFlashParametroItem);
var
  lParametro : TProxyParametroComandoItem;
begin
  lParametro := TProxyParametroComandoItem.Create;

  lParametro.Nome := PParams.Nome;
  lParametro.TipoParametro := PParams.Tipo;
  lParametro.TipoValor := PParams.TipoValor;

  Self.Add( lParametro );
end;

function TProxyParametroComandoList.GetAsString(const pCommandName : string;
  const pIdentCount: Smallint; const pNomeGrupo : string): string;
var
  lMember: string;
begin
  Result := GetListaParametros;

  if pNomeGrupo <> '' then
    lMember := C_PREFIX_NAME + pNomeGrupo + '.'
  else
    lMember := '';

  Result := 'function ' + lMember + pCommandName + IfThen(Result <> EmptyStr, '(' + Result +')' ) + ' : Boolean;';
  Result := FormatParamLine(pIdentCount, Result, ';' );
end;

function TProxyParametroComandoList.GetListaParametros: string;
var
  i : Integer;
begin
  Result := '';
  // cria a lista de parametros de entrada
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].TipoParametro in [tpEntrada, tpEntradaSemRecaregar] then
      Result := Result + '; const p' + Self[i].Nome + ': ' + Self[i].TipoValorAsString;
  end;

  // cria a lista de parametros de entrada
  for i := 0 to Self.Count - 1 do
  begin
    if Self[i].TipoParametro = tpSaida then
      Result := Result + '; out A' + Self[i].Nome + ': ' + Self[i].TipoValorAsString;
  end;

  if Result <> '' then
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
    tvpInteger:  Result := IfThen(pNatural, 'Integer',   'tvpInteger');
    tvpString:   Result := IfThen(pNatural, 'String',    'tvpString');
    tvpBoolean:  Result := IfThen(pNatural, 'Boolean',   'tvpBoolean');
    tvpFloat:    Result := IfThen(pNatural, 'Double',    'tvpFloat');
    tvpBase64:   Result := IfThen(pNatural, 'TBase64',   'tvpBase64');
    tvpDAO:      Result := IfThen(pNatural, 'TDAO',      'tvpDAO');
    tvpBase:     Result := IfThen(pNatural, 'TBase',     'tvpBase');
    tvpDateTime: Result := IfThen(pNatural, 'TDateTime', 'tvpDateTime');
  end
end;

initialization
  TCPClassRegistrer.Registrar(TLRDataFlashComandoList, C_GRUPO_INTERNO);

end.
