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

  TRpDataFlashProtocol = class
  private const
    FORMATTER = '%s';
    LIMITER = '|';
    IDENTIFIER_BEGIN_FORMAT = '@>%s|';
    IDENTIFIER_END_FORMAT = '|%s<@';
    IDENTIFIER_CRIPTO = '@*CRYPTO';
    NAME_PROTOCOL_ERROR = 'PROT_ERRO';
  private
    FIdentifier : string;
    FMessage : string;
    FReturnMsgNodeName: string;
    FEncryption : TRpEncryption;
    FMessageType: TRpDataFlashMessageType;
    function GetCompleteIdentifierStart: string;
    function GetCompleteIdentifierEnding: string;
    procedure SetMessage(const Value: string);
    function MessageWithIdentifierPresent(const AValue : string): Integer;
    function MessageEncrypted(const AValue : string): Integer;
    function TrimBegin(const AValue : string) : string;
    function TrimEnd(const AValue : string) : string;

    function DiscoverIdentifier(const AMessage : string) : string;

    function ReplaceInvalids(const AValue : string) : string;
    function ReturnInvalids(const AValue : string) : string;
    procedure SetIdentifier(const Value: string);
    procedure SetReturnMsgNodeName(const Value: string);

    function RemoveEncryptionFlag(const Value : string) : string;
  public
  test
    constructor Create(const ATipoCriptografia : TRpDataFlashEncryptionType); reintroduce;
    destructor Destroy; override;
    property Mensagem : string read FMessage write SetMessage;

    property Identificador : string read FIdentifier  write SetIdentifier;
    property IdentificadorInicialCompleto : string read GetCompleteIdentifierStart;
    property IdentificadorFinalCompleto : string read GetCompleteIdentifierEnding;
    property TipoMensagem : TRpDataFlashMessageType read FMessageType write FMessageType;

    property NomeNodoMsgRetorno : string read FReturnMsgNodeName write SetReturnMsgNodeName;
    procedure SetTipoCriptografia(const pTipoCriptografia : TRpEncryptionClass);

    function GetMarcaCriptoPadrao : string;
    function GetMarcaCripto(const pValue : string) : string;

    function Criptografar(const pValue : string) : string;
    function Descriptografar(const pValue : string) : string;


    function Montar : string;
    function Desmontar(const AMessage : string) : string;

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

function TRpDataFlashProtocol.Criptografar(const pValue: string): string;
begin
  Result := pValue;
  if Assigned(FEncryption) then
  begin
    Result := IDENTIFIER_CRIPTO + FEncryption.Encrypt(pValue);
  end;
end;

function TRpDataFlashProtocol.Desmontar(const AMessage : string) : string;
var
  lPosicaoInicial: Integer;
  lPosicaoInicioMensagem: Integer;
  lPosicaoFinalMensagem: Integer;
  lMensagem: string;
begin
  if FMessageType = mtText then
    Result := AMessage
  else
  begin
    lMensagem := AMessage;
    // remove o identificador
    lPosicaoInicial := MessageWithIdentifierPresent(lMensagem);
    if lPosicaoInicial <> -1 then
    begin
      lPosicaoInicioMensagem := lPosicaoInicial + Length(IdentificadorInicialCompleto);
      lPosicaoFinalMensagem := (Length(lMensagem) - Length(GetCompleteIdentifierEnding)) + 1;
      lMensagem := Copy(lMensagem, lPosicaoInicioMensagem, (lPosicaoFinalMensagem - lPosicaoInicioMensagem));
    end;

    lMensagem := Descriptografar(lMensagem);
    Result := ReturnInvalids(lMensagem);
  end;
end;

destructor TRpDataFlashProtocol.Destroy;
begin
  if Assigned(FEncryption) then
  begin
    FEncryption.Free;
  end;
  inherited;
end;

constructor TRpDataFlashProtocol.Create(const ATipoCriptografia : TRpDataFlashEncryptionType);
begin
  FIdentifier := EmptyStr;
  FMessage := EmptyStr;
  FReturnMsgNodeName := EmptyStr;
  FMessageType := mtCommand;

  case ATipoCriptografia of
    ecNone : SetTipoCriptografia(nil);
    ecCustom : SetTipoCriptografia(nil);
    tcBase64 : SetTipoCriptografia(TRpEncryptionBase64);
    tcBase64Compressed : SetTipoCriptografia(TRpEncryptionBase64Compressed);
  end;
end;

function TRpDataFlashProtocol.DiscoverIdentifier(const AMessage: string): string;
var
  lPrimeiroCaractereAntesIdentificador: string;
  lPrimeiroCaractereDepoisIdentificador: string;
  lPosicaoInicialIdentificador: Integer;
  lTamanhoIdentificador: Integer;
  lValido: Boolean;
begin
  Result := EmptyStr;
  lValido := (AMessage > IDENTIFIER_BEGIN_FORMAT);

  if lValido then
  begin
    lPrimeiroCaractereAntesIdentificador := IDENTIFIER_BEGIN_FORMAT[Pos(FORMATTER, IDENTIFIER_BEGIN_FORMAT) - 1];
    lPrimeiroCaractereDepoisIdentificador := IDENTIFIER_BEGIN_FORMAT[Pos(lPrimeiroCaractereAntesIdentificador, IDENTIFIER_BEGIN_FORMAT) + Length(FORMATTER) + 1];

    lPosicaoInicialIdentificador := Pos(lPrimeiroCaractereAntesIdentificador, AMessage) + 1;
    lValido := lPosicaoInicialIdentificador > 0;

    if lValido then
    begin
      lTamanhoIdentificador := (Pos(lPrimeiroCaractereDepoisIdentificador, AMessage) - lPosicaoInicialIdentificador);
      Result := Copy(AMessage, lPosicaoInicialIdentificador, lTamanhoIdentificador);
    end;
  end;
end;

function TRpDataFlashProtocol.Descriptografar(const pValue: string): string;
begin
  Result := pValue;
  if (MessageEncrypted(pValue) <> -1) and (Assigned(FEncryption)) then
  begin
    Result := FEncryption.Decrypt(RemoveEncryptionFlag(pValue));
  end;
end;

class function TRpDataFlashProtocol.GetErrorTag: string;
begin
  Result := Format(IDENTIFIER_BEGIN_FORMAT, [NAME_PROTOCOL_ERROR]);
end;

function TRpDataFlashProtocol.GetCompleteIdentifierEnding: string;
begin
  Result := Format(IDENTIFIER_END_FORMAT, [FIdentifier]);
end;

function TRpDataFlashProtocol.GetCompleteIdentifierStart: string;
begin
  Result := Format(IDENTIFIER_BEGIN_FORMAT, [FIdentifier]);
end;

function TRpDataFlashProtocol.GetMarcaCripto(const pValue: string): string;
var
  lIndice: Integer;
begin
  lIndice := MessageEncrypted(pValue);
  if lIndice <> -1 then
    Result := GetMarcaCriptoPadrao
  else
    Result := EmptyStr;
end;

function TRpDataFlashProtocol.GetMarcaCriptoPadrao: string;
begin
  if Assigned(FEncryption) then
    Result := FEncryption.GetDefaultEncryptionIdentifier
  else
    Result := IDENTIFIER_CRIPTO;
end;

function TRpDataFlashProtocol.IsErro: Boolean;
begin
  Result := (FIdentifier = NAME_PROTOCOL_ERROR);
end;

procedure TRpDataFlashProtocol.Clear;
begin
  FReturnMsgNodeName := EmptyStr;
  FMessage := EmptyStr;
  FIdentifier := EmptyStr;
end;

function TRpDataFlashProtocol.Montar: string;
var
  lMensagem: string;
begin
  if FMessageType = mtText then
    Result := FMessage
  else
  begin
    lMensagem := ReplaceInvalids(FMessage);
    lMensagem := Criptografar(lMensagem);

    // adiciona o identificador da mensagem
    if (MessageWithIdentifierPresent(lMensagem)) = -1 then
      Result := IdentificadorInicialCompleto + lMensagem + IdentificadorFinalCompleto
    else
      Result := lMensagem;
  end;
end;

class function TRpDataFlashProtocol.NovoErro(const ATipoCriptografia: TRpDataFlashEncryptionType;
  const pMensagem: string): string;
var
  lProtocolo: TRpDataFlashProtocol;
begin
  lProtocolo := Self.Create(ATipoCriptografia);
  try
    Result := lProtocolo.ToErro(pMensagem);
  finally
    lProtocolo.Free;
  end;
end;

function TRpDataFlashProtocol.TrimEnd(const AValue: string): string;
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

function TRpDataFlashProtocol.TrimBegin(const AValue: string): string;
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

function TRpDataFlashProtocol.RemoveEncryptionFlag(const Value: string): string;
var
  lIndice: Integer;
  lMensagem: string;
begin
  lIndice := MessageEncrypted(Value);
  lMensagem := Value;
  if lIndice <> -1 then
    Delete(lMensagem, lIndice, Length(IDENTIFIER_CRIPTO));
  Result := lMensagem;
end;

function TRpDataFlashProtocol.ReturnInvalids(const AValue: string): string;
var
  lResultado: string;
begin
  lResultado := AValue;
  lResultado := StringReplace(lResultado, '%CRLF%', #13#10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%LF%', #10, [rfReplaceAll]);
  lResultado := StringReplace(lResultado, '%CR%', #13, [rfReplaceAll]);
  Result := lResultado;
end;

function TRpDataFlashProtocol.ReplaceInvalids(const AValue: string): string;
var
  lResultado: string;
begin
  lResultado := AValue;
  lResultado := StringReplace(lResultado, #13#10, '%CRLF%', [rfReplaceAll]);
  lResultado := StringReplace(lResultado, #10, '%LF%', [rfReplaceAll]);
  lResultado := StringReplace(lResultado, #13, '%CR%', [rfReplaceAll]);
  Result := lResultado;
end;

function TRpDataFlashProtocol.ToData: Olevariant;
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
      lGrupoNodo := lGrupo.Primeiro(FReturnMsgNodeName, lIndex);
      Result := lGrupoNodo.ToDataPacket.ToData;
    finally
      lGrupo.Free;
    end;
  end;
end;

function TRpDataFlashProtocol.ToErro(const pMensagem: string): string;
begin
  Identificador := NAME_PROTOCOL_ERROR;
  Mensagem := pMensagem;
  Result := Montar;
end;

procedure TRpDataFlashProtocol.ToFile(const pFileName: string);
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

function TRpDataFlashProtocol.ToTexto: string;
begin
  Result := Desmontar(Mensagem);
end;

procedure TRpDataFlashProtocol.SetIdentifier(const Value: string);
begin
  FIdentifier := Value;
  FMessage := EmptyStr;
end;

procedure TRpDataFlashProtocol.SetMessage(const Value: string);
var
  lMensagem: string;
begin
  if FMessageType = mtText then
  begin
    FIdentifier := '';
    FMessage := Value;
  end
  else
  begin
    lMensagem := TrimBegin(Value);
    lMensagem := TrimEnd(lMensagem);

    if Trim(FIdentifier) = EmptyStr then
      FIdentifier := DiscoverIdentifier(lMensagem);

    if MessageWithIdentifierPresent(lMensagem) = -1 then
      FMessage := RemoveEncryptionFlag(lMensagem)
    else
    begin
      FMessage := Desmontar(lMensagem);
      if DiscoverIdentifier(FMessage) = NAME_PROTOCOL_ERROR then
      begin
        FIdentifier := NAME_PROTOCOL_ERROR;
        FMessage := Desmontar(FMessage);
      end;
    end;
  end;
end;

procedure TRpDataFlashProtocol.SetReturnMsgNodeName(const Value: string);
begin
  if Trim(Value) = EmptyStr then
    FReturnMsgNodeName := 'Retorno'
  else
    FReturnMsgNodeName := Value;
end;

procedure TRpDataFlashProtocol.SetTipoCriptografia(const pTipoCriptografia: TRpEncryptionClass);
var
  lRecriar: Boolean;
begin
  lRecriar := (FEncryption = nil);

  if (not lRecriar) and (not pTipoCriptografia.ClassNameIs(FEncryption.ClassName)) then
    lRecriar := True;

  if (not lRecriar) then
    lRecriar := pTipoCriptografia <> nil;

  if (pTipoCriptografia = nil) and (FEncryption <> nil) then
    FreeAndNil(FEncryption);

  if lRecriar and (pTipoCriptografia <> nil) then
    FEncryption := pTipoCriptografia.Create;
end;

function TRpDataFlashProtocol.MessageWithIdentifierPresent(const AValue : string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(GetCompleteIdentifierStart, AValue);
  if lPosicaoInicial <> 0 then
    Result := lPosicaoInicial
  else
    Result := -1;
end;

function TRpDataFlashProtocol.MessageEncrypted(const AValue: string): Integer;
var
  lPosicaoInicial: Integer;
begin
  lPosicaoInicial := Pos(IDENTIFIER_CRIPTO, AValue);
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
