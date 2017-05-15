unit uRpDataFlash.Protocol;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  SysUtils, Classes, DBClient, Variants, Contnrs, ActiveX, uRpEncryption,
  uRpDataFlash.XMLController, uRpDataFlash.Types, uRpResourceString;

type
  TRpDataFlashProtocolBreaker = class
  strict private const
    FORMATTER = '%s';
    MARKER_BREAKS = '\::%s::/';
    MARKER_TOTAL_BREAKS = '@#*%s*#@';
    MARKER_GUID = '\$id*%s*di$/';
    MARKER_BEGIN = '\*{BEGIN}*/';
    MARKER_END = '\*{END}*/';
    MAX_SIZE = 1000;
  private
    FIsBegin : Boolean;
    FIsEnd : Boolean;
    FCurrentIndex : Integer;
    FBreaks : TStringList;
    FGUID: string;
    FLastBreakPart : Integer;
    FOnStatus: TRpDataFlashOnStatus;
    function IndexExists(const AIndex : Integer) : Boolean;

    function GetMarkValue(const AMark, AValue : string) : Integer;
    function GetBreakValue(const AValue : string) : Integer;
    function GetTotalBreakValue : Integer;

    function GetMarkComplete(const AMark : string; const AIndex : Integer) : string; overload;
    function GetMarkBreakComplete: string;
    function GetMarkBreakTotalComplete(const AIndex : integer): string;
    function GetMarkGuidComplete: string;
    function GetMarkBeginComplete : string;
    function GetMarkEndComplete: string;

    function AddBreakMark(const AValue : string) : string;
    function RemoveBreakMark(const AValue : string) : string;

    procedure AddMarkBreakTotal;
    function RemoveMarkBreakTotal(const AValue : string) : string;

    function AddGuidMark(const AValue : string) : string;
    function RemoveGuidMark(const AValue : string) : string;

    procedure AddMarkEndMenssage;
    function RemoveMarkEndMenssage(const AValue : string) : string;
    function IsFinalMenssage(const AValue : string) : Boolean;

    procedure AddMarkBeginMessage;
    function RemoveMarkBeginMessage(const AValue : string) : string;
    function IsBeginMessage(const AValue : string) : Boolean;

    function GetBreakCount: Integer;
    function GetBreakMessage(const AIndex: Integer): string;
    function GetNext : Boolean;
    function GetContinue: Boolean;
    function GetValue: string;
    function DoBreak(const AValue : string): Integer;
    function GetBreakCountPreview: Integer;
    function GetCurrent: string;
    function GetValid: Boolean;
    procedure DoStatus(const AStatus : TRpDataFlashStatusType;
      const ATotalProc, ACurrentProc : Integer; const AStatusMessage : string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddValue(const AValue : string);
    function BreaksCalcCount(const AValue : string) : Integer;

    property GUID: string read FGUID;
    property Value : string read GetValue;
    property BreakCount : Integer read GetBreakCount;
    property BreakCountPreview : Integer read GetBreakCountPreview;
    property Valid : Boolean read GetValid;

    property BreakMessage[const AIndex : Integer] : string read GetBreakMessage;
    property First : string index 0 read GetBreakMessage;
    property Continue : Boolean read GetContinue;
    property Next : Boolean read GetNext;
    property Current : string read GetCurrent;
    property OnStatus : TRpDataFlashOnStatus read FOnStatus write FOnStatus;

    class function GuidValue(const AValue : string) : TGuid;
  end;

  TProtocolo = class
  private const
    FORMATTER = '%s';
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
    FTipoMensagem: TRpDataFlashMessageType;
    function GetIdentificadorInicialCompleto: string;
    function GetIdentificadorFinalCompleta: string;
    procedure SetMensagem(const Value: string);
    function MensagemComIdentificadorExistente(const AValue : string): Integer;
    function MensagemCriptografada(const AValue : string): Integer;
    function RemoverEspacosInicio(const AValue : string) : string;
    function RemoverEspacosFinal(const AValue : string) : string;

    function DescobrirIDentificador(const pMensagemCompleta : string) : string;

    function SubstituirInvalidos(const AValue : string) : string;
    function RetornarInvalidos(const AValue : string) : string;
    procedure SetIdentificador(const Value: string);
    procedure SetNomeNodoMsgRetorno(const Value: string);

    function RemoverFlagCriptografia(const Value : string) : string;
  public
    constructor Create(const ATipoCriptografia : TRpDataFlashEncryptionType); reintroduce;
    destructor Destroy; override;
    property Mensagem : string read FMensagem write SetMensagem;

    property Identificador : string read FIdentificador  write SetIdentificador;
    property IdentificadorInicialCompleto : string read GetIdentificadorInicialCompleto;
    property IdentificadorFinalCompleto : string read GetIdentificadorFinalCompleta;
    property TipoMensagem : TRpDataFlashMessageType read FTipoMensagem write FTipoMensagem;

    property NomeNodoMsgRetorno : string read FNomeNodoMsgRetorno write SetNomeNodoMsgRetorno;
    procedure SetTipoCriptografia(const pTipoCriptografia : TRpEncryptionClass);

    function GetMarcaCriptoPadrao : string;
    function GetMarcaCripto(const pValue : string) : string;

    function Criptografar(const pValue : string) : string;
    function Descriptografar(const pValue : string) : string;


    function Montar : string;
    function Desmontar(const pMensagemCompleta : string) : string;

    procedure Clear;

    function ToTexto : string;
    function ToData : Olevariant;
    procedure ToFile(const pFileName : string);

    function ToErro(const pMensagem : string) : string;
    function IsErro : Boolean;

    class function NovoErro(const ATipoCriptografia : TRpDataFlashEncryptionType; const pMensagem : string) : string;
    class function GetErrorTag : string;
  end;

  TRpQuebraProtocoloList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRpDataFlashProtocolBreaker;
    procedure SetItem(Index: Integer; const Value: TRpDataFlashProtocolBreaker);
    function AddQuebra : TRpDataFlashProtocolBreaker;
  public
    function Find(const AGuid : string) : TRpDataFlashProtocolBreaker;

    procedure Delete(const AQuebra : TRpDataFlashProtocolBreaker); overload;

    property Item[Index : Integer]: TRpDataFlashProtocolBreaker read GetItem write SetItem; default;

    function DoBreak(const AValor : string) : TRpDataFlashProtocolBreaker;
    function Carregar(const AValor : string) : TRpDataFlashProtocolBreaker;
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
  if FTipoMensagem = mtText then
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

constructor TProtocolo.Create(const ATipoCriptografia : TRpDataFlashEncryptionType);
begin
  FIdentificador := EmptyStr;
  FMensagem := EmptyStr;
  FNomeNodoMsgRetorno := EmptyStr;
  FTipoMensagem := mtCommand;

  case ATipoCriptografia of
    ecNone : SetTipoCriptografia(nil);
    ecCustom : SetTipoCriptografia(nil);
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
    lPrimeiroCaractereAntesIdentificador := IDENTIFICADOR_INI_FORMAT[Pos(FORMATTER, IDENTIFICADOR_INI_FORMAT) - 1];
    lPrimeiroCaractereDepoisIdentificador := IDENTIFICADOR_INI_FORMAT[Pos(lPrimeiroCaractereAntesIdentificador, IDENTIFICADOR_INI_FORMAT) + Length(FORMATTER) + 1];

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

procedure TProtocolo.Clear;
begin
  FNomeNodoMsgRetorno := EmptyStr;
  FMensagem := EmptyStr;
  FIdentificador := EmptyStr;
end;

function TProtocolo.Montar: string;
var
  lMensagem: string;
begin
  if FTipoMensagem = mtText then
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

class function TProtocolo.NovoErro(const ATipoCriptografia: TRpDataFlashEncryptionType;
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

function TProtocolo.RemoverEspacosFinal(const AValue: string): string;
var
  lMensagem: string;
begin
  lMensagem := AValue;
  while (Length(lMensagem) > 0) and CharInSet(lMensagem[Length(lMensagem)], [#10, #13]) do
  begin
    Delete(lMensagem, Length(lMensagem), 1);
  end;
  Result := lMensagem;
end;

function TProtocolo.RemoverEspacosInicio(const AValue: string): string;
var
  lMensagem: string;
begin
  lMensagem := AValue;
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

function TProtocolo.RetornarInvalidos(const AValue: string): string;
var
  lResultado: string;
begin
  lResultado := AValue;
  lResultado := StringReplace(lResultado, '%CRLF%', #13#10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%LF%', #10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%CR%', #13, [rfReplaceAll]);
  Result := lResultado;
end;

function TProtocolo.SubstituirInvalidos(const AValue: string): string;
var
  lResultado: string;
begin
  lResultado := AValue;
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
  if FTipoMensagem = mtText then
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

function TProtocolo.MensagemComIdentificadorExistente(const AValue : string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(GetIdentificadorInicialCompleto, AValue);
  if lPosicaoInicial <> 0 then
    Result := lPosicaoInicial
  else
    Result := -1;
end;

function TProtocolo.MensagemCriptografada(const AValue: string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(IDENTIFICADOR_CRIPTO, AValue);
  if lPosicaoInicial <> 0 then
    Result := lPosicaoInicial
  else
    Result := -1;
end;

{ TListaRetornos }

procedure TRpDataFlashProtocolBreaker.AddMarkEndMenssage;
begin
  if BreakCount > 0 then
  begin
    FBreaks[FBreaks.Count - 1] := FBreaks[FBreaks.Count - 1] + GetMarkEndComplete;
    FIsEnd := True;
  end;
end;

function TRpDataFlashProtocolBreaker.AddGuidMark(const AValue : string) : string;
begin
  Result := AValue + GetMarkGuidComplete;
end;

procedure TRpDataFlashProtocolBreaker.AddMarkBeginMessage;
begin
  if BreakCount > 0 then
  begin
    FBreaks[0] := GetMarkBeginComplete +  FBreaks[0];
    FIsBegin := True;
  end;
end;

function TRpDataFlashProtocolBreaker.AddBreakMark(const AValue: string): string;
begin
  Result := AValue + GetMarkBreakComplete;
  Result := AddGuidMark(Result);
end;

procedure TRpDataFlashProtocolBreaker.AddMarkBreakTotal;
begin
  if BreakCount > 0 then
  begin
    FBreaks[0] := RemoveMarkBreakTotal(FBreaks[0]);
    FBreaks[0] := GetMarkBreakTotalComplete(BreakCount) + FBreaks[0];
  end;
end;

procedure TRpDataFlashProtocolBreaker.AddValue(const AValue: string);
var
  lBrokenParts: Integer;
  lReceiveGuid: string;
begin
  lReceiveGuid := GuidToString(GuidValue(AValue));
  if FGuid = '' then
    FGuid := lReceiveGuid;
  if FGuid <> lReceiveGuid then
    raise ERpDataFlashProtocolGuid.CreateFmt(R_DATAFLASH_PROT_GUID_ERROR, [FGUID, lReceiveGuid]);

  lBrokenParts := GetBreakValue(AValue);
  if lBrokenParts <> - 1 then
  begin
    if IsBeginMessage(AValue) then
    begin
      FIsBegin := True;
      FIsEnd := False;
    end;

    if lBrokenParts = FLastBreakPart + 1 then
      FLastBreakPart := lBrokenParts
    else
      raise ERpDataFlashProtocolGuid.CreateFmt(R_DATAFLASH_PROT_GUID_ORDER, [FLastBreakPart]);

    FBreaks.Insert(lBrokenParts - 1, AValue);
    if IsFinalMenssage(AValue) then
      FIsEnd := True;
  end
  else
    DoBreak(AValue);
  FCurrentIndex := -1;
end;

constructor TRpDataFlashProtocolBreaker.Create;
begin
  FIsEnd := False;
  FBreaks := TStringList.Create;
  Clear;
end;

destructor TRpDataFlashProtocolBreaker.Destroy;
begin
  FBreaks.Free;
  inherited;
end;

procedure TRpDataFlashProtocolBreaker.DoStatus(const AStatus: TRpDataFlashStatusType;
  const ATotalProc, ACurrentProc: Integer; const AStatusMessage : string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, AStatus, ATotalProc, ACurrentProc, AStatusMessage);
end;

function TRpDataFlashProtocolBreaker.GetCurrent: string;
begin
  Result := GetBreakMessage(FCurrentIndex);
end;

function TRpDataFlashProtocolBreaker.GetContinue: Boolean;
begin
  Result := (FCurrentIndex <> -1)
        and (BreakCount >= FCurrentIndex);
end;

function TRpDataFlashProtocolBreaker.GetMarkComplete(const AMark: string; const AIndex: Integer): string;
begin
  Result := Format(AMark, [IntToStr(AIndex)]);
end;

function TRpDataFlashProtocolBreaker.GetMarkEndComplete: string;
begin
  Result := MARKER_END;
end;

function TRpDataFlashProtocolBreaker.GetMarkGuidComplete: string;
begin
  Result := Format(MARKER_GUID, [FGUID]);
end;

function TRpDataFlashProtocolBreaker.GetMarkBeginComplete: string;
begin
  Result := MARKER_BEGIN;
end;

function TRpDataFlashProtocolBreaker.GetMarkBreakComplete: string;
begin
  Result := GetMarkComplete(MARKER_BREAKS, BreakCount + 1);
end;

function TRpDataFlashProtocolBreaker.GetMarkBreakTotalComplete(const AIndex : integer): string;
begin
  Result := GetMarkComplete(MARKER_TOTAL_BREAKS, AIndex);
end;

function TRpDataFlashProtocolBreaker.GetNext: Boolean;
begin
  Result := IndexExists(FCurrentIndex + 1);
  if Result then
    FCurrentIndex := FCurrentIndex + 1;
end;

function TRpDataFlashProtocolBreaker.GetBreakCount: Integer;
begin
  Result := FBreaks.Count;
end;

function TRpDataFlashProtocolBreaker.GetBreakCountPreview: Integer;
begin
  Result := GetTotalBreakValue;
end;

function TRpDataFlashProtocolBreaker.GetBreakMessage(const AIndex: Integer): string;
begin
  if IndexExists(AIndex) then
    Result := FBreaks[AIndex]
  else
    Result := EmptyStr;
end;

function TRpDataFlashProtocolBreaker.GetValid: Boolean;
begin
  Result := FIsBegin and FIsEnd and (GetBreakCountPreview = FBreaks.Count);
end;

function TRpDataFlashProtocolBreaker.GetValue: string;
var
  I: Integer;
  lResul: string;
begin
  lResul := EmptyStr;
  for I := 0 to BreakCount - 1 do
  begin
    if I = 0 then
    begin
      lResul := RemoveMarkBreakTotal(FBreaks[I]);
      lResul := RemoveMarkBeginMessage(lResul);
      lResul := RemoveBreakMark(lResul);
    end else
      lResul := lResul + RemoveBreakMark(FBreaks[I]);
  end;
  Result := lResul;

  if not Valid then
    raise Exception.Create('Mensagem incompleta');

end;

class function TRpDataFlashProtocolBreaker.GuidValue(const AValue: string): TGuid;
var
  lGuidValue: string;
  lFormPos: Integer;
  lPosEndMark: Integer;
  lPosBeginMark: Integer;
  lPosEndForm: Integer;
  lMarkBeginValue: string;
  lMarkFinalValue: string;
  lQuantity: Integer;
begin
  lGuidValue := EmptyStr;

  lFormPos      := Pos(FORMATTER, MARKER_GUID);
  lPosEndForm   := lFormPos + Length(FORMATTER);

  lMarkBeginValue := Copy(MARKER_GUID, 1, lFormPos - 1);

  lPosBeginMark := Pos(lMarkBeginValue, AValue);

  if lPosBeginMark > 0 then
  begin
    lMarkFinalValue := Copy(MARKER_GUID, lPosEndForm, Length(MARKER_GUID));
    lPosEndMark := Pos(lMarkFinalValue, AValue);

    lQuantity := lPosEndMark - (lPosBeginMark + length(lMarkBeginValue));

    lGuidValue := Copy(AValue, lPosBeginMark + length(lMarkBeginValue), lQuantity);

    Result := StringToGUID(lGuidValue);
  end
  else
    Result := GUID_NULL;
end;

function TRpDataFlashProtocolBreaker.IndexExists(const AIndex: Integer): Boolean;
begin
  Result :=  BreakCount > AIndex;
end;

function TRpDataFlashProtocolBreaker.IsFinalMenssage(const AValue: string): Boolean;
var
  lLastPos: Integer;
begin
  lLastPos := pos(MARKER_END, AValue);
  Result := lLastPos > 0;
end;

function TRpDataFlashProtocolBreaker.IsBeginMessage(const AValue: string): Boolean;
var
  lEndPos: Integer;
begin
  lEndPos := pos(MARKER_BEGIN, AValue);
  Result := lEndPos > 0;
end;

procedure TRpDataFlashProtocolBreaker.Clear;
begin
  FIsEnd := False;
  FBreaks.Clear;
  FCurrentIndex := -1;
  FLastBreakPart := 0;
end;

function TRpDataFlashProtocolBreaker.GetBreakValue(const AValue: string): Integer;
begin
  Result := GetMarkValue(MARKER_BREAKS, AValue);
end;

function TRpDataFlashProtocolBreaker.GetTotalBreakValue: Integer;
begin
  if BreakCount > 0 then
    Result := GetMarkValue(MARKER_TOTAL_BREAKS, FBreaks[0])
  else
    Result := -1;
end;

function TRpDataFlashProtocolBreaker.BreaksCalcCount(const AValue: string): Integer;
begin
  Result := Trunc(Length(AValue) / MAX_SIZE);
end;

function TRpDataFlashProtocolBreaker.DoBreak(const AValue : string) : Integer;
var
  lBreakValue: string;
  lAuxValue: string;
  lGuid: TGUID;
  lTotalCount: Integer;
  lCount: Integer;
begin
  lTotalCount := BreaksCalcCount(AValue)  * 2;
  lCount := 0;

  FBreaks.Clear;
  CreateGUID(lGuid);
  FGUID := GUIDToString(lGuid);

  lBreakValue := AValue;
  while Length(lBreakValue) > MAX_SIZE do
  begin
    lAuxValue := Copy(lBreakValue, 1, MAX_SIZE);
    lAuxValue := AddBreakMark(lAuxValue);
    FBreaks.Add(lAuxValue);
    Delete(lBreakValue, 1, MAX_SIZE);

    DoStatus(stPreparingData, lTotalCount, lCount, R_DATAFLASH_PROT_PREPARING_DATA);

    Inc(lCount);
  end;

  if lBreakValue <> EmptyStr then
    FBreaks.Add(AddBreakMark(lBreakValue))
  else
    if FBreaks.Count = 0 then
      FBreaks.Add(AddBreakMark( EmptyStr ));

  AddMarkBeginMessage;
  AddMarkBreakTotal;
  AddMarkEndMenssage;

  Result := BreakCount;
end;

function TRpDataFlashProtocolBreaker.RemoveMarkEndMenssage(
  const AValue: string): string;
var
  lValue: string;
  lEndPos: Integer;
begin
  lValue := AValue;
  lEndPos := pos(MARKER_END, lValue);
  if lEndPos > 0 then
    Delete(lValue, lEndPos, Length(AValue) - lEndPos);
  Result := lValue;
end;

function TRpDataFlashProtocolBreaker.RemoveGuidMark(const AValue: string): string;
var
  lFullValue: string;
  lIdxMarkBegin: Integer;
  lTotalSize: Integer;
  lFormPos: Integer;
  lPosMarkEnd: Integer;
  lEndValue: string;
begin
  lFullValue := AValue;

  lIdxMarkBegin := Pos(Copy(MARKER_GUID, 1, 5), lFullValue);

  lFormPos := Pos(FORMATTER, MARKER_GUID);
  lPosMarkEnd := lFormPos + Length(FORMATTER);
  lEndValue := Copy(MARKER_GUID, lPosMarkEnd, (Length(MARKER_GUID) - (lPosMarkEnd -  1)));

  if lIdxMarkBegin > 0 then
  begin
    lTotalSize := Length(lFullValue);
    Delete(lFullValue, lIdxMarkBegin, lTotalSize);
  end;
  Result := lFullValue;
end;

function TRpDataFlashProtocolBreaker.RemoveMarkBeginMessage(const AValue: string): string;
var
  lValue: string;
  lEndPos: Integer;
begin
  lValue := AValue;
  lEndPos := pos(MARKER_BEGIN, lValue);
  if lEndPos > 0 then
    Delete(lValue, lEndPos, Length(MARKER_BEGIN));
  Result := lValue;
end;

function TRpDataFlashProtocolBreaker.RemoveBreakMark(const AValue: string): string;
var
  lFullValue: string;
  lIdxBeginMark: Integer;
  lTotalSize: Integer;
begin
  lFullValue := AValue;
  lFullValue := RemoveMarkEndMenssage(lFullValue);
  lFullValue := RemoveGuidMark(lFullValue);

  lIdxBeginMark := Pos(Copy(MARKER_BREAKS, 1, 3), lFullValue);
  if lIdxBeginMark > 0 then
  begin
    lTotalSize := Length(lFullValue);
    Delete(lFullValue, lIdxBeginMark, lTotalSize);
  end;
  Result := lFullValue;
end;

function TRpDataFlashProtocolBreaker.RemoveMarkBreakTotal(const AValue: string): string;
var
  lFormPos: Integer;
  lEndMarkPos: Integer;
  lEndBreakValue: string;
  lValueNoTotal: string;
  lIdxMarkBegin: Integer;
  lIdSize: Integer;
begin
  lValueNoTotal := AValue;

  lFormPos := Pos(FORMATTER, MARKER_TOTAL_BREAKS);
  lEndMarkPos := lFormPos + Length(FORMATTER);
  lEndBreakValue := Copy(MARKER_TOTAL_BREAKS, lEndMarkPos, (Length(MARKER_TOTAL_BREAKS) - (lEndMarkPos -  1)));

  lIdxMarkBegin := Pos(lEndBreakValue, lValueNoTotal);
  if lIdxMarkBegin > 0 then
  begin
    lIdSize := Length(lEndBreakValue);
    Delete(lValueNoTotal, 1, (lIdxMarkBegin + lIdSize) - 1);
  end;
  Result := lValueNoTotal;
end;

function TRpDataFlashProtocolBreaker.GetMarkValue(const AMark, AValue: string): Integer;
var
  lIdxBegin: Integer;
  lFormatPos: Integer;
  lIdxEnd: Integer;
  lPosFormValue : integer;
  lBeginMarkValue: string;
begin
  Result := -1;
  lIdxBegin := 0;
  lIdxEnd := 0;

  lFormatPos := Pos(FORMATTER, AMark);
  lBeginMarkValue := Copy(AMark, 1, lFormatPos - 1);
  lPosFormValue := Pos(lBeginMarkValue, AValue);
  if lPosFormValue > 0 then
  begin
    lIdxBegin := lPosFormValue + Length(lBeginMarkValue);
    lIdxEnd := Pos(Copy(AMark, (lFormatPos + Length(FORMATTER)), Length(AMark)), AValue);
  end;

  if lIdxBegin > 0 then
  begin
    Result := StrToIntDef(Copy(AValue, lIdxBegin, (lIdxEnd - lIdxBegin)), -1);
  end;
end;

{ TRpListaQuebraProtocolo }

function TRpQuebraProtocoloList.AddQuebra: TRpDataFlashProtocolBreaker;
begin
  Result := TRpDataFlashProtocolBreaker.Create;
  Add(Result);
end;

function TRpQuebraProtocoloList.Carregar(const AValor: string): TRpDataFlashProtocolBreaker;
var
  lGuid: TGUID;
begin
  lGuid := TRpDataFlashProtocolBreaker.GuidValue(AValor);
  Result := Find(GUIDToString(lGuid));
  if Result = nil then
    Result := AddQuebra;
  Result.AddValue(AValor);
end;

procedure TRpQuebraProtocoloList.Delete(const AQuebra: TRpDataFlashProtocolBreaker);
begin
  Remove(AQuebra);
end;

function TRpQuebraProtocoloList.Find(const AGuid: string): TRpDataFlashProtocolBreaker;
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

function TRpQuebraProtocoloList.GetItem(Index: Integer): TRpDataFlashProtocolBreaker;
begin
  Result := TRpDataFlashProtocolBreaker(inherited Items[Index]);
end;

function TRpQuebraProtocoloList.DoBreak(const AValor: string): TRpDataFlashProtocolBreaker;
begin
  Result := AddQuebra;
  Result.AddValue(AValor);
end;

procedure TRpQuebraProtocoloList.SetItem(Index: Integer; const Value: TRpDataFlashProtocolBreaker);
begin
  inherited Items[Index] := Value;
end;

end.
