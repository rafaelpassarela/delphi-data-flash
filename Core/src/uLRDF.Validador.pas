unit uLRDF.Validador;

interface

uses
  Classes, uLRDF.Protocolo, Contnrs, uLRDF.Component, SysUtils, TypInfo;

type
  TLRDataFlashValidadorItem = class;
  TLRDataFlashValidadorRegrasServer = class;
  TLRDataFlashValidadorRegras = class;

  TLRDataFlashExecucaoValidacaoEvent = function(Sender : TLRDataFlashValidadorItem; AProtocolo : TProtocolo) : string of object;
  TValidadorNaoEncontradoEvent = procedure(Sender : TLRDataFlashValidadorRegrasServer; const AProtocolo : TProtocolo; var ARetorno : string) of object;

  TRegraValidacaoClass = class of TLRDataFlashRegraValidacao;

  TLRDataFlashTipoProcessamentoRegra = (tprErro, tprConectarServer, tprDesconectarServer, tprConectarCliente, tprDesconectarCliente,
    tprAlteracaoSituacao, tprLiberacao, tprValidacao, tprComunicacao, tprConfiguracao, tprValidadorNaoEncontrado);

  TLRDataFlashProcessamentoLogRegraEvent = procedure (Sender : TObject; const pTipoProcessamento : TLRDataFlashTipoProcessamentoRegra;
    const pTipoProcessamentoStr, pOrigem, pDestino, pInfoProcessamento : string) of object;

  TLRDataFlashRegraValidacao = class(TPersistent)
  private
    FPai : TLRDataFlashValidadorItem;
    FListaInconsistencias : TStringList;
    FOnProcessamentoLog: TLRDataFlashProcessamentoLogRegraEvent;
    procedure SetOnProcessamentoLog(const Value: TLRDataFlashProcessamentoLogRegraEvent);
  protected
    procedure DefinirPadroesInicializacao; virtual;
    property ListaInconsistencias : TStringList read FListaInconsistencias;

    function ValidarRetorno(const pCondicao : Boolean; var pRetorno : string) : Boolean; virtual;
    procedure LimparInconsistencias;
    procedure NovaInconsistencia(const pInconsistencia : string);
  public
    constructor Create(pPai: TLRDataFlashValidadorItem); virtual;
    destructor Destroy; override;

    function NomeAmigavel : string; virtual;
    function ExecutarComValidacao : String; virtual;

    function Executar : String; virtual; abstract;
    function Validacoes : String; virtual;

    property Pai: TLRDataFlashValidadorItem read FPai;
    property OnProcessamentoLog : TLRDataFlashProcessamentoLogRegraEvent read FOnProcessamentoLog write SetOnProcessamentoLog;

    class function Identificador : string; virtual; abstract;
  end;

  TLRDataFlashValidadorItem = class(TCollectionItem)
  private
    FIdentificador: string;
    FOnExecutar: TLRDataFlashExecucaoValidacaoEvent;
    FProtocolo: TProtocolo;
    FValidadorExterno: TLRDataFlashRegraValidacao;
    FNomeAmigavel: string;
    FPai: TLRDataFlashValidadorRegras;
    procedure SetIdentificador(const Value: string);
    procedure SetOnExecutar(const Value: TLRDataFlashExecucaoValidacaoEvent);
    function GetNomeAmigavel: string;
    { Private declaration }
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function Executar(const pComando : string) : string;

    property Protocolo : TProtocolo read FProtocolo;
    property ValidadorExterno : TLRDataFlashRegraValidacao read FValidadorExterno;

    procedure SetValidadorExterno(const Value: TRegraValidacaoClass);

    property Pai : TLRDataFlashValidadorRegras read FPai write FPai;
  published
    property Identificador : string read FIdentificador write SetIdentificador;
    property NomeAmigavel : string read GetNomeAmigavel write FNomeAmigavel;
    property OnExecutar : TLRDataFlashExecucaoValidacaoEvent read FOnExecutar write SetOnExecutar;
  end;

  TLRDataFlashCollectionValidadorIems = class(TOwnedCollection);

  TLRDataFlashValidadorRegras = class(TComponent)
  private
    FValidadores: TLRDataFlashCollectionValidadorIems;
    FListaUtilizadores : TObjectList;
    FOnProcessamento: TLRDataFlashProcessamentoLogRegraEvent;
    procedure SetValidadores(const Value: TLRDataFlashCollectionValidadorIems);

    function LocalizarValidador(const pIdentificador : string) : Integer;
    procedure SetOnProcessamento(const Value: TLRDataFlashProcessamentoLogRegraEvent);
    function TipoProcessamentoToStr(const pTipo : TLRDataFlashTipoProcessamentoRegra) : string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ProcessamentoLogRegra(const pTipo : TLRDataFlashTipoProcessamentoRegra; const pOrigem, pDestino, pInfo : string);

    function PorIdentificador(const pIdentificador : string) : TLRDataFlashValidadorItem;

    function NovoValidador(const pIdentificador : string; const pEvento : TLRDataFlashExecucaoValidacaoEvent) : TLRDataFlashValidadorItem; overload;
    procedure NovoValidador(const pClasse : TRegraValidacaoClass); overload;

    function Inconsistencias : string;
  published
    property Validadores : TLRDataFlashCollectionValidadorIems read FValidadores write SetValidadores;
    property OnProcessamento : TLRDataFlashProcessamentoLogRegraEvent read FOnProcessamento write SetOnProcessamento;
  end;

  TLRDataFlashValidadorRegrasServer = class(TLRDataFlashValidadorRegras)
  private const
    IDENTIFICADOR_ITEM_PADRAO = 'ID_IP_GT_IT';
  private
    FServidor: TLRDataFlashConexaoServer;
    FOnValidadorNaoEncontrado: TValidadorNaoEncontradoEvent;

    function IsInvalid(const pResultado : string) : Boolean;
    function RetornoMensagemInvalida(const pResultado : string) : string;

    procedure SetServidor(const Value: TLRDataFlashConexaoServer);
    function ProcedimentoExecutar(Sender : TObject; pItemConexao : TLRDataFlashConexaoItem) : string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property Servidor : TLRDataFlashConexaoServer read FServidor write SetServidor;
    property OnValidadorNaoEncontrado : TValidadorNaoEncontradoEvent read FOnValidadorNaoEncontrado write FOnValidadorNaoEncontrado;
  end;

implementation

{ TRpValidadorItem }

constructor TLRDataFlashValidadorItem.Create(Collection: TCollection);
begin
  inherited;
  FProtocolo := TProtocolo.Create(tcBase64Compressed);
  FProtocolo.Identificador := FIdentificador;
end;

destructor TLRDataFlashValidadorItem.Destroy;
begin
  FProtocolo.Free;
  FValidadorExterno.Free;
  inherited;
end;

function TLRDataFlashValidadorItem.Executar(const pComando : string): string;
begin
  FProtocolo.Mensagem := pComando;
  if Assigned(FOnExecutar) then
  begin
    Result := FOnExecutar(Self, FProtocolo);
  end else if Assigned(FValidadorExterno) then begin
    Result := FValidadorExterno.ExecutarComValidacao;
  end;
end;

function TLRDataFlashValidadorItem.GetNomeAmigavel: string;
begin
  if FNomeAmigavel = EmptyStr then
  begin
    Result := FIdentificador;
  end else begin
    Result := FNomeAmigavel;
  end;
end;

procedure TLRDataFlashValidadorItem.SetIdentificador(const Value: string);
begin
  FIdentificador := Value;
  FProtocolo.Identificador := FIdentificador;
end;

procedure TLRDataFlashValidadorItem.SetOnExecutar(const Value: TLRDataFlashExecucaoValidacaoEvent);
begin
  FOnExecutar := Value;
end;

procedure TLRDataFlashValidadorItem.SetValidadorExterno(const Value: TRegraValidacaoClass);
begin
  FValidadorExterno := Value.Create(Self);
  if Assigned(FValidadorExterno) then
  begin
    FIdentificador := FValidadorExterno.Identificador;
    FNomeAmigavel  := FValidadorExterno.NomeAmigavel;
  end;
end;

{ TLRDataFlashValidadorRegras }

constructor TLRDataFlashValidadorRegras.Create(AOwner: TComponent);
begin
  inherited;
  FListaUtilizadores := TObjectList.Create(False);
  FValidadores := TLRDataFlashCollectionValidadorIems.Create(Self, TLRDataFlashValidadorItem);
end;

destructor TLRDataFlashValidadorRegras.Destroy;
begin
  FValidadores.Free;
  FListaUtilizadores.Free;
  inherited;
end;

function TLRDataFlashValidadorRegras.Inconsistencias: string;
var
  lCountValidador: Integer;
  lValidadorExterno: TLRDataFlashRegraValidacao;
  lInconsistencias : TStringList;

  procedure NovaInconsistencia(const pInconsistencia : string);
  begin
    if (pInconsistencia <> EmptyStr) and (lInconsistencias.IndexOf(pInconsistencia) = -1) then
    begin
      lInconsistencias.Add(pInconsistencia);
    end;
  end;

begin
  lInconsistencias := TStringList.Create;
  try
    for lCountValidador := 0 to FValidadores.Count - 1 do
    begin
      lValidadorExterno := TLRDataFlashValidadorItem(FValidadores.Items[lCountValidador]).ValidadorExterno;
      if Assigned(lValidadorExterno) then
      begin
        NovaInconsistencia(lValidadorExterno.Validacoes);
      end;
    end;
    Result := lInconsistencias.Text;
  finally
    lInconsistencias.Free;
  end;
end;

function TLRDataFlashValidadorRegras.LocalizarValidador(const pIdentificador: string): Integer;
var
  lIndexValidador: Integer;
  lCountValidador: Integer;
begin
  lIndexValidador := -1;
  for lCountValidador := 0 to FValidadores.Count - 1 do
  begin
    if TLRDataFlashValidadorItem(FValidadores.Items[lCountValidador]).Identificador = pIdentificador then
    begin
      lIndexValidador := lCountValidador;
    end;
  end;
  Result := lIndexValidador;
end;


procedure TLRDataFlashValidadorRegras.NovoValidador(const pClasse: TRegraValidacaoClass);
var
  lItem: TLRDataFlashValidadorItem;
begin
  Assert(not Assigned(PorIdentificador(pClasse.Identificador)), 'O identificador <' + pClasse.Identificador +
    '> já está registrado como regra para a classe <' + pClasse.ClassName + '>');
  lItem := TLRDataFlashValidadorItem(FValidadores.Add);
  lItem.Pai := Self;
  lItem.SetValidadorExterno(pClasse);
  lItem.ValidadorExterno.OnProcessamentoLog := FOnProcessamento;
end;

function TLRDataFlashValidadorRegras.NovoValidador(const pIdentificador: string; const pEvento: TLRDataFlashExecucaoValidacaoEvent): TLRDataFlashValidadorItem;
begin
  Assert(not Assigned(PorIdentificador(pIdentificador)), 'O identificador <' + pIdentificador +
    '> já está registrado como regra para um evento !');
  Result := TLRDataFlashValidadorItem(FValidadores.Add);
  Result.Pai := Self;
  Result.Identificador := pIdentificador;
  Result.OnExecutar := pEvento;
end;

function TLRDataFlashValidadorRegras.PorIdentificador(const pIdentificador: string): TLRDataFlashValidadorItem;
var
  lIndexValidador: Integer;
begin
  Result := nil;
  lIndexValidador := LocalizarValidador(pIdentificador);
  if lIndexValidador <> -1 then
  begin
    Result := TLRDataFlashValidadorItem(FValidadores.Items[lIndexValidador]);
  end;
end;


procedure TLRDataFlashValidadorRegras.ProcessamentoLogRegra(const pTipo : TLRDataFlashTipoProcessamentoRegra; const pOrigem, pDestino, pInfo : string);
begin
  if Assigned(FOnProcessamento) then
  begin
    FOnProcessamento(Self, pTipo, TipoProcessamentoToStr(pTipo), pOrigem, pDestino, pInfo);
  end;
end;

procedure TLRDataFlashValidadorRegras.SetOnProcessamento(const Value: TLRDataFlashProcessamentoLogRegraEvent);
begin
  FOnProcessamento := Value;
end;

procedure TLRDataFlashValidadorRegras.SetValidadores(const Value: TLRDataFlashCollectionValidadorIems);
begin
  FValidadores.Assign(Value);
end;

function TLRDataFlashValidadorRegras.TipoProcessamentoToStr(const pTipo: TLRDataFlashTipoProcessamentoRegra): string;
begin
  Result := GetEnumName(TypeInfo(TLRDataFlashTipoProcessamentoRegra), Integer(pTipo));
end;

{ TLRDataFlashValidadorRegrasServer }

function TLRDataFlashValidadorRegrasServer.IsInvalid(const pResultado: string): Boolean;
begin
  Result := Pos(TAG_RET_INVALID, pResultado) > 0;
end;

procedure TLRDataFlashValidadorRegrasServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServidor) then
  begin
    FServidor.OnInternalProcessar := nil;
    FServidor := nil;
  end;
end;

function TLRDataFlashValidadorRegrasServer.ProcedimentoExecutar(Sender : TObject; pItemConexao : TLRDataFlashConexaoItem): string;
var
  lValidador: TLRDataFlashValidadorItem;
begin
  if pItemConexao.Protocolo.Identificador = IDENTIFICADOR_ITEM_PADRAO then
  begin
    Result := EmptyStr; // TProtocolo.MontarMensagem(GetListaValidadores, pItemConexao.Protocolo.Identificador);
  end
  else
  begin
    lValidador := PorIdentificador(pItemConexao.Protocolo.Identificador);
    if Assigned(lValidador) then
    begin
      ProcessamentoLogRegra(tprComunicacao, pItemConexao.NomeCliente, FServidor.Servidor, lValidador.NomeAmigavel);
      Result := lValidador.Executar(pItemConexao.Protocolo.Mensagem);
      if IsInvalid(Result) then
      begin
        Result := RetornoMensagemInvalida(Result);
        pItemConexao.Protocolo.Identificador := TAG_RET_ERRO;
        ProcessamentoLogRegra(tprErro, FServidor.Servidor, pItemConexao.NomeCliente, Result);
      end;
    end
    else
    begin
      Result := EmptyStr;
      if Assigned(FOnValidadorNaoEncontrado) then
      begin
        FOnValidadorNaoEncontrado(Self, pItemConexao.Protocolo, Result);
        ProcessamentoLogRegra(tprValidadorNaoEncontrado, FServidor.Servidor, pItemConexao.NomeCliente, Result);
      end;
    end;
  end;
end;

function TLRDataFlashValidadorRegrasServer.RetornoMensagemInvalida(const pResultado: string): string;
begin
  if IsInvalid(pResultado) then
  begin
    Result := pResultado;
    Delete(Result, 1, length(TAG_RET_INVALID));
  end
  else
    Result := EmptyStr;
end;

procedure TLRDataFlashValidadorRegrasServer.SetServidor(const Value: TLRDataFlashConexaoServer);
begin
  if Assigned(FServidor) then
  begin
    FServidor.RemoveFreeNotification(Self);
  end;
  FServidor := Value;
  FServidor.OnInternalProcessar := ProcedimentoExecutar;
  if Assigned(FServidor) then
  begin
    FServidor.FreeNotification(Self);
  end;
end;

{ TLRDataFlashRegraValidacao }

constructor TLRDataFlashRegraValidacao.Create(pPai: TLRDataFlashValidadorItem);
begin
  inherited Create;
  FListaInconsistencias := TStringList.Create;
  FPai := pPai;
  DefinirPadroesInicializacao;
end;

procedure TLRDataFlashRegraValidacao.DefinirPadroesInicializacao;
begin
// implementada nas classes filhas
end;

destructor TLRDataFlashRegraValidacao.Destroy;
begin
  FListaInconsistencias.Free;
  inherited;
end;

function TLRDataFlashRegraValidacao.ExecutarComValidacao: String;
var
  lValidacoes: string;
begin
  lValidacoes := Validacoes;
  if lValidacoes <> EmptyStr then
  begin
    Result := lValidacoes;
    Pai.Pai.ProcessamentoLogRegra(tprValidacao,  Pai. NomeAmigavel, Pai. NomeAmigavel, lValidacoes);
  end else begin
    try
      Result := Executar;
    except on E:Exception do
      begin
        Result := E.Message;
        Pai.Pai.ProcessamentoLogRegra(tprErro,  Pai. NomeAmigavel, Pai. NomeAmigavel, E.Message);
      end;
    end;
  end;
end;

procedure TLRDataFlashRegraValidacao.LimparInconsistencias;
begin
  FListaInconsistencias.Clear;
end;

function TLRDataFlashRegraValidacao.NomeAmigavel: string;
begin
  Result := Identificador;
end;

procedure TLRDataFlashRegraValidacao.NovaInconsistencia(const pInconsistencia: string);
begin
  if (pInconsistencia <> EmptyStr) and (FListaInconsistencias.IndexOf(TAG_RET_INVALID + pInconsistencia) = -1) then
  begin
    FListaInconsistencias.Add(TAG_RET_INVALID + pInconsistencia);
  end;
end;

procedure TLRDataFlashRegraValidacao.SetOnProcessamentoLog(const Value: TLRDataFlashProcessamentoLogRegraEvent);
begin
  FOnProcessamentoLog := Value;
end;

function TLRDataFlashRegraValidacao.Validacoes: String;
begin
  Result := FListaInconsistencias.Text;
  LimparInconsistencias;
end;

function TLRDataFlashRegraValidacao.ValidarRetorno(const pCondicao : Boolean; var pRetorno: string) : Boolean;
begin
  if not pCondicao then
  begin
    pRetorno := EmptyStr;
  end;
  Result := pCondicao;
end;

end.
