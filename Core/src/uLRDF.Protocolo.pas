unit uLRDF.Protocolo;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  SysUtils, Classes, DBClient, Variants, uLRDF.ManipuladorXML, uLRDF.Types,
  Contnrs, ActiveX, uRpEncryption;

type
  TLRDataFlashQuebraProtocolo = class
  strict private const
    FORMATADOR = '%s';
    MARCADOR_QUEBRA = '\::%s::/';
    MARCADOR_TOTAL_QUEBRA = '@#*%s*#@';
    MARCADOR_GUID = '\$id*%s*di$/';
    MARCADOR_INICIO = '\*{INICIO}*/';
    MARCADOR_FINAL = '\*{FINAL}*/';
    TAMANHO_MAXIMO = 1000;
  private
    FInicio : Boolean;
    FFinal : Boolean;
    FIndexAtual : Integer;
    FQuebras : TStringList;
    FGUID: string;
    FUltimaParteQuebra : Integer;
    FOnStatus: TLRDataFlashOnStatus;
    function IndexExiste(const pIndex : Integer) : Boolean;

    function ValorMarcador(const pMarcador, pValor : string) : Integer;
    function ValorParteDeQuebra(const pValor : string) : Integer;
    function ValorTotalDeQuebra : Integer;

    function GetMarcadorCompleto(const pMarcador : string; const pIndex : Integer) : string; overload;
    function GetMarcadorQuebraCompleto: string;
    function GetMarcadorTotalQuebraCompleto(const pIndex : integer): string;
    function GetMarcadorGuidCompleto: string;
    function GetMarcadorInicioCompleto : string;
    function GetMarcadorFinalCompleto: string;

    function AdicionarMarcadorQuebra(const pValor : string) : string;
    function RemoverMarcadorQuebra(const pValor : string) : string;

    procedure AdicionarMarcadorTotalQuebra;
    function RemoverMarcadorTotalQuebra(const pValor : string) : string;

    function AdicionarMarcadorGuid(const pValor : string) : string;
    function RemoverMarcadorGuid(const pValor : string) : string;

    procedure AdicionarMarcadorFinalMensagem;
    function RemoverMarcadorFinalMensagem(const pValor : string) : string;
    function IsFinalMensagem(const pValor : string) : Boolean;

    procedure AdicionarMarcadorInicioMensagem;
    function RemoverMarcadorInicioMensagem(const pValor : string) : string;
    function IsInicioMensagem(const pValor : string) : Boolean;

    function GetQuantidadeQuebras: Integer;
    function GetQuebra(const pIndex: Integer): string;
    function GetProxima : Boolean;
    function GetContinua: Boolean;
    function GetValor: string;
    function Quebrar(const pValor : string): Integer;
    function GetQuantidadeQuebrasPrevistas: Integer;
    function GetAtual: string;
    function GetValida: Boolean;
    procedure GerarStatus(const ASituacao : TLRDataFlashStatusType; const AProcessamentoTotal, AProcessamentoAtual : Integer;
      const AStatusMens : string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Limpar;
    procedure AdicionarValor(const pValor : string);
    function QuantidadeQuebrasCalculada(const pValor : string) : Integer;

    property GUID: string read FGUID;
    property Valor : string read GetValor;
    property QuantidadeQuebras : Integer read GetQuantidadeQuebras;
    property QuantidadeQuebrasPrevistas : Integer read GetQuantidadeQuebrasPrevistas;
    property Valida : Boolean read GetValida;

    property Quebra[const pIndex : Integer] : string read GetQuebra;
    property Primeira : string index 0 read GetQuebra;
    property Continua : Boolean read GetContinua;
    property Proxima : Boolean read GetProxima;
    property Atual : string read GetAtual;
    property OnStatus : TLRDataFlashOnStatus read FOnStatus write FOnStatus;

    class function GuidValor(const pValor : string) : TGuid;
  end;

  TProtocolo = class
  private const
    FORMATADOR = '%s';
    LIMITADOR = '|';
    IDENTIFICADOR_INI_FORMAT = '@>%s|';
    IDENTIFICADOR_FIM_FORMAT = '|%s<@';
    IDENTIFICADOR_CRIPTO = '@*CRYPTO';
    NME_PROTOCOLO_ERRO = 'PROT_ERRO';
  private
    FIdentificador : string;
    FMensagem : string;
    FNomeNodoMsgRetorno: string;
    FCriptografia : TRpEncryption;
    FTipoMensagem: TLRDataFlashTipoMensagem;
    function GetIdentificadorInicialCompleto: string;
    function GetIdentificadorFinalCompleta: string;
    procedure SetMensagem(const Value: string);
    function MensagemComIdentificadorExistente(const pValor : string): Integer;
    function MensagemCriptografada(const pValor : string): Integer;
    function RemoverEspacosInicio(const pValor : string) : string;
    function RemoverEspacosFinal(const pValor : string) : string;

    function DescobrirIDentificador(const pMensagemCompleta : string) : string;

    function SubstituirInvalidos(const pValor : string) : string;
    function RetornarInvalidos(const pValor : string) : string;
    procedure SetIdentificador(const Value: string);
    procedure SetNomeNodoMsgRetorno(const Value: string);

    function RemoverFlagCriptografia(const Value : string) : string;
  public
    constructor Create(const ATipoCriptografia : TLRDataFlashTipoCriptografia); reintroduce;
    destructor Destroy; override;
    property Mensagem : string read FMensagem write SetMensagem;

    property Identificador : string read FIdentificador  write SetIdentificador;
    property IdentificadorInicialCompleto : string read GetIdentificadorInicialCompleto;
    property IdentificadorFinalCompleto : string read GetIdentificadorFinalCompleta;
    property TipoMensagem : TLRDataFlashTipoMensagem read FTipoMensagem write FTipoMensagem;

    property NomeNodoMsgRetorno : string read FNomeNodoMsgRetorno write SetNomeNodoMsgRetorno;
    procedure SetTipoCriptografia(const pTipoCriptografia : TRpEncryptionClass);

    function GetMarcaCriptoPadrao : string;
    function GetMarcaCripto(const pValue : string) : string;

    function Criptografar(const pValue : string) : string;
    function Descriptografar(const pValue : string) : string;


    function Montar : string;
    function Desmontar(const pMensagemCompleta : string) : string;

    procedure Limpar;

    function ToTexto : string;
    function ToData : Olevariant;
    procedure ToFile(const pFileName : string);

    function ToErro(const pMensagem : string) : string;
    function IsErro : Boolean;

    class function NovoErro(const ATipoCriptografia : TLRDataFlashTipoCriptografia; const pMensagem : string) : string;
    class function GetErrorTag : string;
  end;

  TRpQuebraProtocoloList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLRDataFlashQuebraProtocolo;
    procedure SetItem(Index: Integer; const Value: TLRDataFlashQuebraProtocolo);
    function AddQuebra : TLRDataFlashQuebraProtocolo;
  public
    function Find(const AGuid : string) : TLRDataFlashQuebraProtocolo;

    procedure Delete(const AQuebra : TLRDataFlashQuebraProtocolo); overload;

    property Item[Index : Integer]: TLRDataFlashQuebraProtocolo read GetItem write SetItem; default;

    function Quebrar(const AValor : string) : TLRDataFlashQuebraProtocolo;
    function Carregar(const AValor : string) : TLRDataFlashQuebraProtocolo;
  end;


function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;

implementation

uses
  ComObj;

{ TProtocolo }

function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  {$IFDEF UNICODE}
    Result := SysUtils.CharInSet(C, CharSet);
  {$ELSE}
    Result := C in CharSet;
  {$ENDIF}
end;

function TProtocolo.Criptografar(const pValue: string): string;
begin
  Result := pValue;
  if Assigned(FCriptografia) then
  begin
    Result := IDENTIFICADOR_CRIPTO + FCriptografia.Encrypt(pValue);
  end;
end;

function TProtocolo.Desmontar(const pMensagemCompleta : string) : string;
var
  lPosicaoInicial: Integer;
  lPosicaoInicioMensagem: Integer;
  lPosicaoFinalMensagem: Integer;
  lMensagem: string;
begin
  if FTipoMensagem = tmTexto then
    Result := pMensagemCompleta
  else
  begin
    lMensagem := pMensagemCompleta;
    // remove o identificador
    lPosicaoInicial := MensagemComIdentificadorExistente(lMensagem);
    if lPosicaoInicial <> -1 then
    begin
      lPosicaoInicioMensagem := lPosicaoInicial + Length(IdentificadorInicialCompleto);
      lPosicaoFinalMensagem := (Length(lMensagem) - Length(GetIdentificadorFinalCompleta)) + 1;
      lMensagem := Copy(lMensagem, lPosicaoInicioMensagem, (lPosicaoFinalMensagem - lPosicaoInicioMensagem));
    end;

    lMensagem := Descriptografar(lMensagem);
    Result := RetornarInvalidos(lMensagem);
  end;
end;

destructor TProtocolo.Destroy;
begin
  if Assigned(FCriptografia) then
  begin
    FCriptografia.Free;
  end;
  inherited;
end;

constructor TProtocolo.Create(const ATipoCriptografia : TLRDataFlashTipoCriptografia);
begin
  FIdentificador := EmptyStr;
  FMensagem := EmptyStr;
  FNomeNodoMsgRetorno := EmptyStr;
  FTipoMensagem := tmComando;  
  
  case ATipoCriptografia of
    tcSemCriptografia : SetTipoCriptografia(nil);
    tcCriptografiaCustomizada : SetTipoCriptografia(nil);
    tcBase64 : SetTipoCriptografia(TRpEncryptionBase64);
    tcBase64Compressed : SetTipoCriptografia(TRpEncryptionBase64Compressed);
  end;
end;

function TProtocolo.DescobrirIDentificador(const pMensagemCompleta: string): string;
var
  lPrimeiroCaractereAntesIdentificador: string;
  lPrimeiroCaractereDepoisIdentificador: string;
  lPosicaoInicialIdentificador: Integer;
  lTamanhoIdentificador: Integer;
  lValido: Boolean;
begin
  Result := EmptyStr;
  lValido := (pMensagemCompleta > IDENTIFICADOR_INI_FORMAT);

  if lValido then
  begin
    lPrimeiroCaractereAntesIdentificador := IDENTIFICADOR_INI_FORMAT[Pos(FORMATADOR, IDENTIFICADOR_INI_FORMAT) - 1];
    lPrimeiroCaractereDepoisIdentificador := IDENTIFICADOR_INI_FORMAT[Pos(lPrimeiroCaractereAntesIdentificador, IDENTIFICADOR_INI_FORMAT) + Length(FORMATADOR) + 1];

    lPosicaoInicialIdentificador := Pos(lPrimeiroCaractereAntesIdentificador, pMensagemCompleta) + 1;
    lValido := lPosicaoInicialIdentificador > 0;

    if lValido then
    begin
      lTamanhoIdentificador := (Pos(lPrimeiroCaractereDepoisIdentificador, pMensagemCompleta) - lPosicaoInicialIdentificador);
      Result := Copy(pMensagemCompleta, lPosicaoInicialIdentificador, lTamanhoIdentificador);
    end;
  end;
end;

function TProtocolo.Descriptografar(const pValue: string): string;
begin
  Result := pValue;
  if (MensagemCriptografada(pValue) <> -1) and (Assigned(FCriptografia)) then
  begin
    Result := FCriptografia.Decrypt(RemoverFlagCriptografia(pValue));
  end;
end;

class function TProtocolo.GetErrorTag: string;
begin
  Result := Format(IDENTIFICADOR_INI_FORMAT, [NME_PROTOCOLO_ERRO]);
end;

function TProtocolo.GetIdentificadorFinalCompleta: string;
begin
  Result := Format(IDENTIFICADOR_FIM_FORMAT, [FIdentificador]);
end;

function TProtocolo.GetIdentificadorInicialCompleto: string;
begin
  Result := Format(IDENTIFICADOR_INI_FORMAT, [FIdentificador]);
end;

function TProtocolo.GetMarcaCripto(const pValue: string): string;
var
  lIndice: Integer;
begin
  lIndice := MensagemCriptografada(pValue);
  if lIndice <> -1 then
    Result := GetMarcaCriptoPadrao
  else
    Result := EmptyStr;
end;

function TProtocolo.GetMarcaCriptoPadrao: string;
begin
  if Assigned(FCriptografia) then
    Result := FCriptografia.GetDefaultEncryptionIdentifier
  else
    Result := IDENTIFICADOR_CRIPTO;
end;

function TProtocolo.IsErro: Boolean;
begin
  Result := (FIdentificador = NME_PROTOCOLO_ERRO);
end;

procedure TProtocolo.Limpar;
begin
  FNomeNodoMsgRetorno := EmptyStr;
  FMensagem := EmptyStr;
  FIdentificador := EmptyStr;
end;

function TProtocolo.Montar: string;
var
  lMensagem: string;
begin
  if FTipoMensagem = tmTexto then
    Result := FMensagem
  else
  begin
    lMensagem := SubstituirInvalidos(FMensagem);
    lMensagem := Criptografar(lMensagem);

    // adiciona o identificador da mensagem
    if (MensagemComIdentificadorExistente(lMensagem)) = -1 then
      Result := IdentificadorInicialCompleto + lMensagem + IdentificadorFinalCompleto
    else
      Result := lMensagem;
  end;
end;

class function TProtocolo.NovoErro(const ATipoCriptografia: TLRDataFlashTipoCriptografia;
  const pMensagem: string): string;
var
  lProtocolo: TProtocolo;
begin
  lProtocolo := Self.Create(ATipoCriptografia);
  try
    Result := lProtocolo.ToErro(pMensagem);
  finally
    lProtocolo.Free;
  end;
end;

function TProtocolo.RemoverEspacosFinal(const pValor: string): string;
var
  lMensagem: string;
begin
  lMensagem := pValor;
  while (Length(lMensagem) > 0) and CharInSet(lMensagem[Length(lMensagem)], [#10, #13]) do
  begin
    Delete(lMensagem, Length(lMensagem), 1);
  end;
  Result := lMensagem;
end;

function TProtocolo.RemoverEspacosInicio(const pValor: string): string;
var
  lMensagem: string;
begin
  lMensagem := pValor;
  while (Length(lMensagem) > 0) and CharInSet(lMensagem[1], [#10, #13]) do
  begin
    Delete(lMensagem, 1, 1);
  end;
  Result := lMensagem;
end;

function TProtocolo.RemoverFlagCriptografia(const Value: string): string;
var
  lIndice: Integer;
  lMensagem: string;
begin
  lIndice := MensagemCriptografada(Value);
  lMensagem := Value;
  if lIndice <> -1 then
    Delete(lMensagem, lIndice, Length(IDENTIFICADOR_CRIPTO));
  Result := lMensagem;
end;

function TProtocolo.RetornarInvalidos(const pValor: string): string;
var
  lResultado: string;
begin
  lResultado := pValor;
  lResultado := StringReplace(lResultado, '%CRLF%', #13#10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%LF%', #10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%CR%', #13, [rfReplaceAll]);
  Result := lResultado;
end;

function TProtocolo.SubstituirInvalidos(const pValor: string): string;
var
  lResultado: string;
begin
  lResultado := pValor;
  lResultado := StringReplace(lResultado, #13#10, '%CRLF%', [rfReplaceAll]);
  lResultado := StringReplace(lResultado, #10, '%LF%', [rfReplaceAll]);
  lResultado := StringReplace(lResultado, #13, '%CR%', [rfReplaceAll]);
  Result := lResultado;
end;

function TProtocolo.ToData: Olevariant;
var
  lGrupo: TGrupo;
  lIndex: Integer;
  lGrupoNodo: TGrupo;
begin
  Result := Null;
  lGrupo := TGrupo.FromXML(ToTexto, false);
  if Assigned(lGrupo) then
  begin
    try
      lGrupoNodo := lGrupo.Primeiro(FNomeNodoMsgRetorno, lIndex);
      Result := lGrupoNodo.ToDataPacket.ToData;
    finally
      lGrupo.Free;
    end;
  end;
end;

function TProtocolo.ToErro(const pMensagem: string): string;
begin
  Identificador := NME_PROTOCOLO_ERRO;
  Mensagem := pMensagem;
  Result := Montar;
end;

procedure TProtocolo.ToFile(const pFileName: string);
var
  lTexto: TStringList;
begin
  lTexto := TStringList.Create;
  try
    lTexto.Text := ToTexto;
    lTexto.SaveToFile(pFileName);
  finally
    lTexto.Free;
  end;
end;

function TProtocolo.ToTexto: string;
begin
  Result := Desmontar(Mensagem);
end;

procedure TProtocolo.SetIdentificador(const Value: string);
begin
  FIdentificador := Value;
  FMensagem := EmptyStr;
end;

procedure TProtocolo.SetMensagem(const Value: string);
var
  lMensagem: string;
begin
  if FTipoMensagem = tmTexto then
  begin
    FIdentificador := '';
    FMensagem := Value;
  end
  else
  begin
    lMensagem := RemoverEspacosInicio(Value);
    lMensagem := RemoverEspacosFinal(lMensagem);

    if Trim(FIdentificador) = EmptyStr then
      FIdentificador := DescobrirIDentificador(lMensagem);

    if MensagemComIdentificadorExistente(lMensagem) = -1 then
      FMensagem := RemoverFlagCriptografia(lMensagem)
    else
    begin
      FMensagem := Desmontar(lMensagem);
      if DescobrirIDentificador(FMensagem) = NME_PROTOCOLO_ERRO then
      begin
        FIdentificador := NME_PROTOCOLO_ERRO;
        FMensagem := Desmontar(FMensagem);
      end;
    end;
  end;
end;

procedure TProtocolo.SetNomeNodoMsgRetorno(const Value: string);
begin
  if Trim(Value) = EmptyStr then
    FNomeNodoMsgRetorno := 'Retorno'
  else
    FNomeNodoMsgRetorno := Value;
end;

procedure TProtocolo.SetTipoCriptografia(const pTipoCriptografia: TRpEncryptionClass);
var
  lRecriar: Boolean;
begin
  lRecriar := (FCriptografia = nil);

  if (not lRecriar) and (not pTipoCriptografia.ClassNameIs(FCriptografia.ClassName)) then
    lRecriar := True;

  if (not lRecriar) then
    lRecriar := pTipoCriptografia <> nil;

  if (pTipoCriptografia = nil) and (FCriptografia <> nil) then
    FreeAndNil(FCriptografia);

  if lRecriar and (pTipoCriptografia <> nil) then
    FCriptografia := pTipoCriptografia.Create;
end;

function TProtocolo.MensagemComIdentificadorExistente(const pValor : string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(GetIdentificadorInicialCompleto, pValor);
  if lPosicaoInicial <> 0 then
    Result := lPosicaoInicial
  else
    Result := -1;
end;

function TProtocolo.MensagemCriptografada(const pValor: string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(IDENTIFICADOR_CRIPTO, pValor);
  if lPosicaoInicial <> 0 then
    Result := lPosicaoInicial
  else
    Result := -1;
end;

{ TListaRetornos }

procedure TLRDataFlashQuebraProtocolo.AdicionarMarcadorFinalMensagem;
begin
  if QuantidadeQuebras > 0 then
  begin
    FQuebras[FQuebras.Count - 1] := FQuebras[FQuebras.Count - 1] + GetMarcadorFinalCompleto;
    FFinal := True;
  end;
end;

function TLRDataFlashQuebraProtocolo.AdicionarMarcadorGuid(const pValor : string) : string;
begin
  Result := pValor + GetMarcadorGuidCompleto;
end;

procedure TLRDataFlashQuebraProtocolo.AdicionarMarcadorInicioMensagem;
begin
  if QuantidadeQuebras > 0 then
  begin
    FQuebras[0] := GetMarcadorInicioCompleto +  FQuebras[0];
    FInicio := True;
  end;
end;

function TLRDataFlashQuebraProtocolo.AdicionarMarcadorQuebra(const pValor: string): string;
begin
  Result := pValor + GetMarcadorQuebraCompleto;
  Result := AdicionarMarcadorGuid(Result);
end;

procedure TLRDataFlashQuebraProtocolo.AdicionarMarcadorTotalQuebra;
begin
  if QuantidadeQuebras > 0 then
  begin
    FQuebras[0] := RemoverMarcadorTotalQuebra(FQuebras[0]);
    FQuebras[0] := GetMarcadorTotalQuebraCompleto(QuantidadeQuebras) + FQuebras[0];
  end;
end;

procedure TLRDataFlashQuebraProtocolo.AdicionarValor(const pValor: string);
var
  lParteQuebra: Integer;
  lGuidRecebida: string;
begin
  lGuidRecebida := GuidToString(GuidValor(pValor));
  if FGuid = '' then
    FGuid := lGuidRecebida;
  if FGuid <> lGuidRecebida then
    raise Exception.Create('Guid esperada : ' + FGUID + ' - Guid recebida : ' + lGuidRecebida);

  lParteQuebra := ValorParteDeQuebra(pValor);
  if lParteQuebra <> - 1 then
  begin
    if IsInicioMensagem(pValor) then
    begin
      FInicio := True;
      FFinal := False;
    end;

    if lParteQuebra = FUltimaParteQuebra + 1 then
      FUltimaParteQuebra := lParteQuebra
    else
      raise Exception.Create('Item de sequencia ' + IntToStr(FUltimaParteQuebra) + ' não recebido ou fora da ordem !');

    FQuebras.Insert(lParteQuebra - 1, pValor);
    if IsFinalMensagem(pValor) then
      FFinal := True;
  end
  else
    Quebrar(pValor);
  FIndexAtual := -1;
end;

constructor TLRDataFlashQuebraProtocolo.Create;
begin
  FFinal := False;
  FQuebras := TStringList.Create;
  Limpar;
end;

destructor TLRDataFlashQuebraProtocolo.Destroy;
begin
  FQuebras.Free;
  inherited;
end;

procedure TLRDataFlashQuebraProtocolo.GerarStatus(const ASituacao: TLRDataFlashStatusType;
  const AProcessamentoTotal, AProcessamentoAtual: Integer; const AStatusMens : string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, ASituacao, AProcessamentoTotal, AProcessamentoAtual, AStatusMens);
end;

function TLRDataFlashQuebraProtocolo.GetAtual: string;
begin
  Result := GetQuebra(FIndexAtual);
end;

function TLRDataFlashQuebraProtocolo.GetContinua: Boolean;
begin
  Result := (FIndexAtual <> -1)
        and (QuantidadeQuebras >= FIndexAtual);
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorCompleto(const pMarcador: string; const pIndex: Integer): string;
begin
  Result := Format(pMarcador, [IntToStr(pIndex)]);
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorFinalCompleto: string;
begin
  Result := MARCADOR_FINAL;
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorGuidCompleto: string;
begin
  Result := Format(MARCADOR_GUID, [FGUID]);
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorInicioCompleto: string;
begin
  Result := MARCADOR_INICIO;
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorQuebraCompleto: string;
begin
  Result := GetMarcadorCompleto(MARCADOR_QUEBRA, QuantidadeQuebras + 1);
end;

function TLRDataFlashQuebraProtocolo.GetMarcadorTotalQuebraCompleto(const pIndex : integer): string;
begin
  Result := GetMarcadorCompleto(MARCADOR_TOTAL_QUEBRA, pIndex);
end;

function TLRDataFlashQuebraProtocolo.GetProxima: Boolean;
begin
  Result := IndexExiste(FIndexAtual + 1);
  if Result then
    FIndexAtual := FIndexAtual + 1;
end;

function TLRDataFlashQuebraProtocolo.GetQuantidadeQuebras: Integer;
begin
  Result := FQuebras.Count;
end;

function TLRDataFlashQuebraProtocolo.GetQuantidadeQuebrasPrevistas: Integer;
begin
  Result := ValorTotalDeQuebra;
end;

function TLRDataFlashQuebraProtocolo.GetQuebra(const pIndex: Integer): string;
begin
  if IndexExiste(pIndex) then
  begin
    Result := FQuebras[pIndex];
  end else begin
    Result := EmptyStr;
  end;
end;

function TLRDataFlashQuebraProtocolo.GetValida: Boolean;
begin
  Result := FInicio and FFinal and (GetQuantidadeQuebrasPrevistas = FQuebras.Count);
end;

function TLRDataFlashQuebraProtocolo.GetValor: string;
var
  I: Integer;
  lResultado: string;
begin
  lResultado := EmptyStr;
  for I := 0 to QuantidadeQuebras - 1 do
  begin
    if I = 0 then
    begin
      lResultado := RemoverMarcadorTotalQuebra(FQuebras[I]);
      lResultado := RemoverMarcadorInicioMensagem(lResultado);
      lResultado := RemoverMarcadorQuebra(lResultado);
    end else
      lResultado := lResultado + RemoverMarcadorQuebra(FQuebras[I]);
  end;
  Result := lResultado;

  if not Valida then
    raise Exception.Create('Mensagem incompleta');

end;

class function TLRDataFlashQuebraProtocolo.GuidValor(const pValor: string): TGuid;
var
  lValorGuid: string;
  lPosicaoFormatador: Integer;
  lPosicaoFinalMarcador: Integer;
  lPosicaoInicialMarcador: Integer;
  lPosicaoFinalFormatador: Integer;
  lVlrInicialMarcador: string;
  lVlrFinalMarcador: string;
  lQuantidade: Integer;
begin
  lValorGuid := EmptyStr;

  lPosicaoFormatador      := Pos(FORMATADOR, MARCADOR_GUID);
  lPosicaoFinalFormatador   := lPosicaoFormatador + Length(FORMATADOR);

  lVlrInicialMarcador := Copy(MARCADOR_GUID, 1, lPosicaoFormatador - 1);

  lPosicaoInicialMarcador := Pos(lVlrInicialMarcador, pValor);

  if lPosicaoInicialMarcador > 0 then
  begin
    lVlrFinalMarcador := Copy(MARCADOR_GUID, lPosicaoFinalFormatador, Length(MARCADOR_GUID));
    lPosicaoFinalMarcador := Pos(lVlrFinalMarcador, pValor);

    lQuantidade := lPosicaoFinalMarcador - (lPosicaoInicialMarcador + length(lVlrInicialMarcador));

    lValorGuid := Copy(pValor, lPosicaoInicialMarcador + length(lVlrInicialMarcador), lQuantidade);

    Result := StringToGUID(lValorGuid);
  end
  else
    Result := GUID_NULL;
end;

function TLRDataFlashQuebraProtocolo.IndexExiste(const pIndex: Integer): Boolean;
begin
  Result :=  QuantidadeQuebras > pIndex;
end;

function TLRDataFlashQuebraProtocolo.IsFinalMensagem(const pValor: string): Boolean;
var
  lPosicaoFinal: Integer;
begin
  lPosicaoFinal := pos(MARCADOR_FINAL, pValor);
  Result := lPosicaoFinal > 0;
end;

function TLRDataFlashQuebraProtocolo.IsInicioMensagem(const pValor: string): Boolean;
var
  lPosicaoFinal: Integer;
begin
  lPosicaoFinal := pos(MARCADOR_INICIO, pValor);
  Result := lPosicaoFinal > 0;
end;

procedure TLRDataFlashQuebraProtocolo.Limpar;
begin
  FFinal := False;
  FQuebras.Clear;
  FIndexAtual := -1;
  FUltimaParteQuebra := 0;
end;

function TLRDataFlashQuebraProtocolo.ValorParteDeQuebra(const pValor: string): Integer;
begin
  Result := ValorMarcador(MARCADOR_QUEBRA, pValor);
end;

function TLRDataFlashQuebraProtocolo.ValorTotalDeQuebra: Integer;
begin
  if QuantidadeQuebras > 0 then
    Result := ValorMarcador(MARCADOR_TOTAL_QUEBRA, FQuebras[0])
  else
    Result := -1;
end;

function TLRDataFlashQuebraProtocolo.QuantidadeQuebrasCalculada(
  const pValor: string): Integer;
begin
  Result := Trunc(Length(pValor) / TAMANHO_MAXIMO);
end;

function TLRDataFlashQuebraProtocolo.Quebrar(const pValor : string) : Integer;
var
  lValorQuebra: string;
  lValorQuebrado: string;
  lGuid: TGUID;
  lQuantidadeTotalCalculada: Integer;
  lCount: Integer;
begin
  lQuantidadeTotalCalculada := QuantidadeQuebrasCalculada(pValor)  * 2;
  lCount := 0;

  FQuebras.Clear;
  CreateGUID(lGuid);
  FGUID := GUIDToString(lGuid);

  lValorQuebra := pValor;
  while Length(lValorQuebra) > TAMANHO_MAXIMO do
  begin
    lValorQuebrado := Copy(lValorQuebra, 1, TAMANHO_MAXIMO);
    lValorQuebrado := AdicionarMarcadorQuebra(lValorQuebrado);
    FQuebras.Add(lValorQuebrado);
    Delete(lValorQuebra, 1, TAMANHO_MAXIMO);

    GerarStatus(stPreparingData, lQuantidadeTotalCalculada, lCount, 'Preparando protocolo de mensagem');

    Inc(lCount);
  end;

  if lValorQuebra <> EmptyStr then
    FQuebras.Add(AdicionarMarcadorQuebra(lValorQuebra))
  else
    if FQuebras.Count = 0 then
      FQuebras.Add(AdicionarMarcadorQuebra( EmptyStr ));

  AdicionarMarcadorInicioMensagem;
  AdicionarMarcadorTotalQuebra;
  AdicionarMarcadorFinalMensagem;

  Result := QuantidadeQuebras;
end;

function TLRDataFlashQuebraProtocolo.RemoverMarcadorFinalMensagem(
  const pValor: string): string;
var
  lValor: string;
  lPosicaoFinal: Integer;
begin
  lValor := pValor;
  lPosicaoFinal := pos(MARCADOR_FINAL, lValor);
  if lPosicaoFinal > 0 then
    Delete(lValor, lPosicaoFinal, Length(pValor) - lPosicaoFinal);
  Result := lValor;
end;

function TLRDataFlashQuebraProtocolo.RemoverMarcadorGuid(const pValor: string): string;
var
  lValorSemQuebra: string;
  lIndexInicioMarcador: Integer;
  lTamanhoTotal: Integer;
  lPosicaoFormatador: Integer;
  lPosicaoFinalMarcador: Integer;
  lValorQuebraFinal: string;
begin
  lValorSemQuebra := pValor;

  lIndexInicioMarcador := Pos(Copy(MARCADOR_GUID, 1, 5), lValorSemQuebra);

  lPosicaoFormatador := Pos(FORMATADOR, MARCADOR_GUID);
  lPosicaoFinalMarcador := lPosicaoFormatador + Length(FORMATADOR);
  lValorQuebraFinal := Copy(MARCADOR_GUID, lPosicaoFinalMarcador, (Length(MARCADOR_GUID) - (lPosicaoFinalMarcador -  1)));

  if lIndexInicioMarcador > 0 then
  begin
    lTamanhoTotal := Length(lValorSemQuebra);
    Delete(lValorSemQuebra, lIndexInicioMarcador, lTamanhoTotal);
  end;
  Result := lValorSemQuebra;
end;

function TLRDataFlashQuebraProtocolo.RemoverMarcadorInicioMensagem(const pValor: string): string;
var
  lValor: string;
  lPosicaoInicio: Integer;
begin
  lValor := pValor;
  lPosicaoInicio := pos(MARCADOR_INICIO, lValor);
  if lPosicaoInicio > 0 then
    Delete(lValor, lPosicaoInicio, Length(MARCADOR_INICIO));
  Result := lValor;
end;

function TLRDataFlashQuebraProtocolo.RemoverMarcadorQuebra(const pValor: string): string;
var
  lValorSemQuebra: string;
  lIndexInicioMarcador: Integer;
  lTamanhoTotal: Integer;
begin
  lValorSemQuebra := pValor;
  lValorSemQuebra := RemoverMarcadorFinalMensagem(lValorSemQuebra);
  lValorSemQuebra := RemoverMarcadorGuid(lValorSemQuebra);

  lIndexInicioMarcador := Pos(Copy(MARCADOR_QUEBRA, 1, 3), lValorSemQuebra);
  if lIndexInicioMarcador > 0 then
  begin
    lTamanhoTotal := Length(lValorSemQuebra);
    Delete(lValorSemQuebra, lIndexInicioMarcador, lTamanhoTotal);
  end;
  Result := lValorSemQuebra;
end;

function TLRDataFlashQuebraProtocolo.RemoverMarcadorTotalQuebra(const pValor: string): string;
var
  lPosicaoFormatador: Integer;
  lPosicaoFinalMarcador: Integer;
  lValorQuebraFinal: string;
  lValorSemTotal: string;
  lIndexInicioMarcador: Integer;
  lTamanhoIdent: Integer;
begin
  lValorSemTotal := pValor;

  lPosicaoFormatador := Pos(FORMATADOR, MARCADOR_TOTAL_QUEBRA);
  lPosicaoFinalMarcador := lPosicaoFormatador + Length(FORMATADOR);
  lValorQuebraFinal := Copy(MARCADOR_TOTAL_QUEBRA, lPosicaoFinalMarcador, (Length(MARCADOR_TOTAL_QUEBRA) - (lPosicaoFinalMarcador -  1)));

  lIndexInicioMarcador := Pos(lValorQuebraFinal, lValorSemTotal);
  if lIndexInicioMarcador > 0 then
  begin
    lTamanhoIdent := Length(lValorQuebraFinal);
    Delete(lValorSemTotal, 1, (lIndexInicioMarcador + lTamanhoIdent) - 1);
  end;
  Result := lValorSemTotal;
end;

function TLRDataFlashQuebraProtocolo.ValorMarcador(const pMarcador, pValor: string): Integer;
var
  lIndexInicioMarcador: Integer;
  lPosicaoFormatador: Integer;
  lIndexFinalMarcador: Integer;
  lPosicaoFormatadorValor : integer;
  lValorQuebraInicial: string;
begin
  Result := -1;
  lIndexInicioMarcador := 0;
  lIndexFinalMarcador := 0;

  lPosicaoFormatador := Pos(FORMATADOR, pMarcador);
  lValorQuebraInicial := Copy(pMarcador, 1, lPosicaoFormatador - 1);
  lPosicaoFormatadorValor := Pos(lValorQuebraInicial, pValor);
  if lPosicaoFormatadorValor > 0 then
  begin
    lIndexInicioMarcador := lPosicaoFormatadorValor + Length(lValorQuebraInicial);
    lIndexFinalMarcador := Pos(Copy(pMarcador, (lPosicaoFormatador + Length(FORMATADOR)), Length(pMarcador)), pValor);
  end;

  if lIndexInicioMarcador > 0 then
  begin
    Result := StrToIntDef(Copy(pValor, lIndexInicioMarcador, (lIndexFinalMarcador - lIndexInicioMarcador)), -1);
  end;
end;

{ TRpListaQuebraProtocolo }

function TRpQuebraProtocoloList.AddQuebra: TLRDataFlashQuebraProtocolo;
begin
  Result := TLRDataFlashQuebraProtocolo.Create;
  Add(Result);
end;

function TRpQuebraProtocoloList.Carregar(
  const AValor: string): TLRDataFlashQuebraProtocolo;
var
  lGuid: TGUID;
begin
  lGuid := TLRDataFlashQuebraProtocolo.GuidValor(AValor);
  Result := Find(GUIDToString(lGuid));
  if Result = nil then
    Result := AddQuebra;
  Result.AdicionarValor(AValor);
end;

procedure TRpQuebraProtocoloList.Delete(const AQuebra: TLRDataFlashQuebraProtocolo);
begin
  Remove(AQuebra);
end;

function TRpQuebraProtocoloList.Find(const AGuid: string): TLRDataFlashQuebraProtocolo;
var
  I : Integer;
begin
  Result := nil;
  // localiza nas quebras já registradas o protocolo recebido
  for I := 0 to Count - 1 do
  begin
    if Item[I].GUID = AGuid then
    begin
      Result := Item[I];
      Break;
    end;
  end;
end;

function TRpQuebraProtocoloList.GetItem(Index: Integer): TLRDataFlashQuebraProtocolo;
begin
  Result := TLRDataFlashQuebraProtocolo(inherited Items[Index]);
end;

function TRpQuebraProtocoloList.Quebrar(const AValor: string): TLRDataFlashQuebraProtocolo;
begin
  Result := AddQuebra;
  Result.AdicionarValor(AValor);
end;

procedure TRpQuebraProtocoloList.SetItem(Index: Integer; const Value: TLRDataFlashQuebraProtocolo);
begin
  inherited Items[Index] := Value;
end;

end.
