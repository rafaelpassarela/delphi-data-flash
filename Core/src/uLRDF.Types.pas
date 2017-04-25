unit uLRDF.Types;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  SysUtils, IdStackWindows, Classes, Contnrs, DB, uRpAlgoritmos, StrUtils,
  DBConsts, Variants, uRpFileHelper;

const
  TAG_RET_ERRO = 'ERRO';
  MSG_ECHO = 'ECCHO';
  IDENTIFICADOR_ITEM_PADRAO = 'ID_IP_GT_IT';
  TAG_RET_INVALID = '\*INVALID*\';
  TAG_COMANDO ='Comando';
  TAG_CALLBACK = 'CALLBACK';
  TAG_GET_CLASSES_PROXY = 'GET_CLASSES_PROXY';
  TAG_PING = '\*PING*\';
  // comandos de controle
  // (ao adicionar um, verificar as funcoes "isComandoDesejado" e "SeparaComando"
  //  da unit uLRDataFlashComponent)
  C_NULL_TCPVALUE = '[::[NULL]::]';
  C_INICIO = 'INICIO';
  C_COMECAR = 'COMECAR';
  C_FIM = 'FIM';
  C_MAIS_DADOS = 'MAIS_DADOS';
  C_SEM_GRUPO_DEFINIDO = 'UNNAMED';
  C_WHERE_DATASET = 'internal_OpenWhere';
  C_TMP_UNIT_CLASS = '{uClassUnit}';
  // parametros de retorno fixos
  C_PARAM_INT_EXCEPTION = 'exec_Exception';
  C_PARAM_INT_STATUS_RET = 'exec_StatusRetorno';
  C_PARAM_INT_STATUS_PROC = 'InternalStatusProcessamento';
  C_PARAM_INT_TIPO_FORMATO = 'internal_FormatType';
  C_GRUPO_INTERNO = '_internal_';
  C_GRUPO_DATASET = '_dataset_provider_';
  C_REST_PARAM = '_rest_prm_http_';
  C_REST_CONTENT_TYPE = 'application/delphi';
  C_FTP_DEFAULT_USER = 'tcpcomftpuser';
  C_FTP_DEFAULT_PWD  = '@ftpusertcpcom#';
  C_FTP_ID_MARK = 'ID';
  C_FTP_SIZE_MARK = 'SIZE';
  C_FTP_DELETE_RECIVE_MARK = 'DEL';
  C_FTP_FILENAME_MARK = 'FILENAME';
  C_COMANDO_NO_DESCRIPTION = 'Function "function DoGetDescricao: string; override;" não foi implementada';

type
  TLRDataFlashStatusType = (stPreparingData, stSendingData, stReceivingData);
  TLRDataFlashTipoCriptografia = (tcSemCriptografia, tcCriptografiaCustomizada, tcBase64, tcBase64Compressed);
  TLRDataFlashTipoParametro = (tpEntrada, tpSaida, tpInterno, tpEntradaSemRecaregar);
  TLRDataFlashTipoValorParametro = (
    tvpInteger, tvpString, tvpBoolean, tvpFloat, tvpBase64, tvpDAO, tvpBase,
    tvpDateTime, tvpFile, tvpJSON, tvpBinaryFile);
  TLRDataFlashTipoProcessamento = (tprLocal, tprPonte, tprSomentePonte);
  TLRDataFlashStatusProcessamento = (tspServidor, tspPonteOnline, tspPonteOffLine, tspLocal, tspNenhum);
  TLRDataFlashTipoCarga = (tcEnvio, tcRetorno);
  TLRDataFlashTipoExecucao = (teExecucao, tePonteInvalida, tePonteBemSucedida, teAntesComunicarPonte);
  TLRDataFlashTipoMensagem = (tmComando, tmTexto);
  TLRDataFlashOrigemValidacao = (cmdRecebimento, cmdEnvio);
  TLRDataFlashTipoLogService = (tlsConexao, tlsDesconexao, tlsEnvio, tlsRecebimento,
    tlsErro, tlsStatus, tlsPonte, tlsComando, tlsRegra, tlsArquivo, tlsSync, tlsSyncXml);
  TLRDataFlashTipoServer = (tsServidor, tsPonte);
  TLRDataFlashTipoComunicacao = (tcTexto, tcStream, tcCompressedStream, tcChar);
  TLRDataFlashLifeCycle = (tlfInstance, tlfSession, tlfServer, tlfInternal);
  TLRDataFlashSerializationFormat = (sfDesconhecido, sfXML, sfJSON);
  TLRDataFlashHelperAction = (haSave, haLoad, haDelete, haExecute);

  TLRDataFlashDoConectarEvent = procedure of object;
  TLRDataFlashExeptionHandler = procedure(const E: Exception) of object;
  TLRDataFlashOnConexaoNoServidor = procedure(Sender : TObject; const AServidor : string; const APorta : Integer) of object;
  TLRDataFlashOnSemServico = procedure(Sender : TObject; var AServidor : string; var APorta : Integer;
    const AException : Exception; var AReconectar : Boolean) of object;
  TLRDataFlashOnExecutarMensagem = function (Sender : TObject) : Boolean;
  TLRDataFlashOnStatus = procedure(Sender : TObject; const ASituacao : TLRDataFlashStatusType;
    const AProcessamentoTotal, AProcessamentoAtual : Integer; const AStatusStr : string) of object;
  TLRDataFlashExecucaoInternaComando = function : Boolean of object;
  TLRDataFlashExecucaoExternaComando = function : string of object;
  TLRDataFlashBusyCallback = procedure (const AStart : Boolean) of object;
  TLRDataFlashOnObjectRequest = procedure(const AOperacao : TLRDataFlashHelperAction; const AObject : TFileCustomObject; out AContinue : Boolean) of object;

  // Exceptions
  ELRDataFlashException = class(Exception);
  ELRDataFlashComunicacao = class(ELRDataFlashException);
  ELRDataFlashRecebimento = class(ELRDataFlashException);
  ELRDataFlashEnvio = class(ELRDataFlashException);
  ELRDataFlashExceptionClass = class of ELRDataFlashException;
  ELRDataFlashFalhaConexao = class(ELRDataFlashException);
  ELRDataFlashMensagemInvalida = class(ELRDataFlashException);
  ELRDataFlashMensagemDivergente = class(ELRDataFlashException);
  ELRDataFlashSemPermissao = class(ELRDataFlashException);
  ELRDataFlashParametroNaoEncontrado = class(ELRDataFlashException);
  ELRDataFlashConexaoInvalida = class(ELRDataFlashException);
  ELRDataFlashExecucao = class(ELRDataFlashException);
  ELRDataFlashFalhaAutenticacao = class(ELRDataFlashException);
  ELRDataFlashUsuarioNaoAutenticado = class(ELRDataFlashException);
  ELRDataFlashBeforeExecuteCommandError = class(ELRDataFlashException);
  ELRDataFlashFileTransferError = class(ELRDataFlashException);
  ELRDataFlashFTPError = class(ELRDataFlashException);

  TLRDataFlashClientInfo = packed record
    DisplayName: string;
    IP: string;
    PeerIP: string;
    GUIDComando: string;
  public
    procedure Initialize;
  end;

  IFileControlConfig = interface
  ['{5C8BB3EE-988F-4F17-8376-23055FADBCA0}']
    function GetServerName : string;
    function GetServerPort: Integer;
    function GetRestPort : Integer;
    function GetFTPPort: Integer;
    function GetModoComunicacao: TLRDataFlashTipoComunicacao;
    function GetModoCriptografia : TLRDataFlashTipoCriptografia;
    function GetLocalHostToIP : Boolean;

    property ServerName : string read GetServerName;
    property ServerPort : Integer read GetServerPort;
    property RestPort : Integer read GetRestPort;
    property FTPPort : Integer read GetFTPPort;
    property ModoComunicacao : TLRDataFlashTipoComunicacao read GetModoComunicacao;
    property ModoCriptografia : TLRDataFlashTipoCriptografia read GetModoCriptografia;
    property LocalHostToIP : Boolean read GetLocalHostToIP;
  end;

  ILRDataFlashFileTransferSupport = interface
    ['{801E5FDB-875A-434C-AF58-DA050D1EBE57}']
    function GetFileTransfer_TempDir : string;
    function GetFileTransfer_Port : Integer;
    function FileTransfer_RegisterFile(const AFileID : string; const AFileName : string) : Boolean;
    procedure OnFileTransferLog(const ALogMessage : string);
  end;

  TFtpFileInfo = packed record
    FileID: string;
    FileName:  string;
    FileSize: Int64;
    FileDelete: Boolean;
  public
    procedure Decode(const AString : string);
  end;

  TLRDataFlashUtils = class
  public
    class function GetNomeComputadorLocal : string;
    class function GetIpComputadorLocal : string;
  end;

  TLRDataFlashValidations = class
  protected
    class function RemoveInvalidChars(const Value : string) : string;
  public
    class function ValidarNome(const Value : string) : string;
  end;

  TLRDataFlashCustomProvider = class(TPersistent)
  private
    FInsertSQL: TStrings;
    FUpdateSQL: TStrings;
    FDeleteSQL: TStrings;
    FSelectSQL: TStrings;
    FCustomCommand: string;
    procedure SetDeleteSQL(const Value: TStrings);
    procedure SetInsertSQL(const Value: TStrings);
    procedure SetSelectSQL(const Value: TStrings);
    procedure SetUpdateSQL(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    function GetAsString : string;
    procedure SetFromString(const Value : string);
    procedure Clear;
  published
    property SelectSQL : TStrings read FSelectSQL write SetSelectSQL;
    property InsertSQL : TStrings read FInsertSQL write SetInsertSQL;
    property UpdateSQL : TStrings read FUpdateSQL write SetUpdateSQL;
    property DeleteSQL : TStrings read FDeleteSQL write SetDeleteSQL;
    property CustomCommand : string read FCustomCommand write FCustomCommand;
  end;

  TStringReplace = class
  public
    class function StringReplaceWholeWord(const text, SearchText, ReplaceText: string;
      ReplaceFlags: TReplaceFlags): String;
  end;

  TDataSetParamItem = class(TParam)
  public
    function Serializar : string;
  end;

  TDataSetParams = class(TParams)
  private
    FAutoCreateParam: Boolean;
  public
    function ParamByName(const Value: string): TParam;
    function GetAsString : string;
    procedure SetFromString(const Value : string);
    property AutoCreateParam : Boolean read FAutoCreateParam write FAutoCreateParam;
  end;

implementation

{ TLRDataFlashClientInfo }

procedure TLRDataFlashClientInfo.Initialize;
begin
  PeerIP := EmptyStr;
  DisplayName := EmptyStr;
  IP := EmptyStr;
  GUIDComando := EmptyStr;
end;

{ TLRDataFlashValidations }

class function TLRDataFlashValidations.RemoveInvalidChars(
  const Value: string): string;
var
  i : Integer;
begin;
  Result := EmptyStr;
  for i := 1 to Length(Value) do
  {$IFDEF UNICODE}
    if CharInSet(Value[i], ['A'..'Z', 'a'..'z', '0'..'9', '_']) then
  {$ELSE}
    if Value[i] in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
  {$ENDIF}
      Result := Result + Value[i];
end;

class function TLRDataFlashValidations.ValidarNome(const Value: string): string;
begin
  Result := RemoveInvalidChars(Value);
end;

{ TLRDataFlashCustomProvider }

procedure TLRDataFlashCustomProvider.Clear;
begin
  FInsertSQL.Clear;
  FUpdateSQL.Clear;
  FDeleteSQL.Clear;
  FSelectSQL.Clear;
  FCustomCommand := EmptyStr;
end;

constructor TLRDataFlashCustomProvider.Create;
begin
  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TLRDataFlashCustomProvider.Destroy;
begin
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FSelectSQL);
  inherited;
end;

function TLRDataFlashCustomProvider.GetAsString: string;
var
  lStr : TStringList;
begin
  lStr := TStringList.Create;
  try
    lStr.Add('CustomCommand=' + Algoritimos.Base64CompressedString( FCustomCommand ) );
    lStr.Add('Insert=' + Algoritimos.Base64CompressedString( FInsertSQL.Text ) );
    lStr.Add('UpdateSQL=' + Algoritimos.Base64CompressedString( FUpdateSQL.Text ) );
    lStr.Add('DeleteSQL=' + Algoritimos.Base64CompressedString( FDeleteSQL.Text ) );
    lStr.Add('SelectSQL=' + Algoritimos.Base64CompressedString( FSelectSQL.Text ) );
    Result := lStr.Text;
  finally
    FreeAndNil(lStr);
  end;
end;

procedure TLRDataFlashCustomProvider.SetDeleteSQL(const Value: TStrings);
begin
  FDeleteSQL.Assign( Value );
end;

procedure TLRDataFlashCustomProvider.SetFromString(const Value: string);
var
  lStr : TStringList;
begin
  lStr := TStringList.Create;
  lStr.Text := Value;
  try
    FCustomCommand := Algoritimos.Base64DecompressedString( lStr.Values['CustomCommand'] );
    FInsertSQL.Text := Algoritimos.Base64DecompressedString( lStr.Values['Insert'] );
    FUpdateSQL.Text := Algoritimos.Base64DecompressedString( lStr.Values['UpdateSQL'] );
    FDeleteSQL.Text := Algoritimos.Base64DecompressedString( lStr.Values['DeleteSQL'] );
    FSelectSQL.Text := Algoritimos.Base64DecompressedString( lStr.Values['SelectSQL'] );
  finally
    FreeAndNil(lStr);
  end;
end;

procedure TLRDataFlashCustomProvider.SetInsertSQL(const Value: TStrings);
begin
  FInsertSQL.Assign( Value );
end;

procedure TLRDataFlashCustomProvider.SetSelectSQL(const Value: TStrings);
begin
  FSelectSQL.Assign( Value );
end;

procedure TLRDataFlashCustomProvider.SetUpdateSQL(const Value: TStrings);
begin
  FUpdateSQL.Assign( Value );
end;

{ TStringReplace }

class function TStringReplace.StringReplaceWholeWord(const text, SearchText,
  ReplaceText: string; ReplaceFlags: TReplaceFlags): String;
const
  separators : set of AnsiChar = [' ', '.', ',', '?', '!',#13, #10, #09, '(', ')'];

var
  StartPos, EndPos : Integer;
  Left, Right : widestring;

  function isWordThere(const text, word: string; ReplaceFlags: TReplaceFlags; var StartPos, EndPos: integer) : Boolean;
  var
     Before, After: boolean;
  begin
    Result:= false;
    StartPos:=0;
    while (not Result) do
    begin
      inc(startPos);
      if (rfIgnoreCase in ReplaceFlags) then
        StartPos := PosEx(lowercase(word), Lowercase(text), StartPos)
      else
        StartPos := PosEx(word, text, StartPos);

      if StartPos = 0 then
        Exit;

      EndPos := StartPos + Length(word) -1;
      {$IFDEF UNICODE}
      if (StartPos = 1) or (CharInSet(Text[StartPos-1], Separators)) then
      {$ELSE}
      if (StartPos = 1) or ((Text[StartPos-1] in Separators)) then
      {$ENDIF}
        Before := true
      else
        Before := false;

      {$IFDEF UNICODE}
      if (EndPos = Length(text)) or CharInSet(Text[EndPos+1], Separators) then
      {$ELSE}
      if (EndPos = Length(text)) or (Text[EndPos+1] in Separators) then
      {$ENDIF}
        After := true
      else
        After := false;

      Result:=Before and After;
    end;
  end;

begin
  Result := Text;
  if not isWordThere(Text, SearchText, ReplaceFlags, StartPos, EndPos) then
    Exit;

  Left := LeftStr(Text, StartPos-1);
  Right := RightStr(Text, Length(Text)-EndPos);

  if rfReplaceAll in ReplaceFlags then
    Right := StringReplaceWholeWord(Right, SearchText, ReplaceText, ReplaceFlags);

  Result := Left + ReplaceText + Right;
end;

{ TDataSetParams }

function TDataSetParams.GetAsString: string;
var
  lLista : TStrings;
  i: Integer;
begin
  lLista := TStringList.Create;
  for i := 0 to Self.Count - 1 do
    lLista.Add( TDataSetParamItem(Self[i]).Serializar );
  Result := Trim( lLista.Text );
  FreeAndNil(lLista);
end;

function TDataSetParams.ParamByName(const Value: string): TParam;
begin
  Result := FindParam(Value);
  if Result = nil then
  begin
    if FAutoCreateParam then
    begin
      Result := TParam(Add);
      Result.Name := Value;
      Result.ParamType := ptInput;
    end
    else
      DatabaseErrorFmt(SParameterNotFound, [Value], GetDataSet);
  end;
end;

procedure TDataSetParams.SetFromString(const Value: string);
var
  lLista : TStrings;
  i: Integer;
  lNome: string;
  lValor: string;
  lTipo: string;
  lOldAutoCreate: Boolean;

  function GetTagValue(const pTag : string) : string;
  var
    lIni: Integer;
  begin
    lIni := Pos('<' + pTag + '>', lLista[i]) + Length(pTag) + 2;
    Result := Copy(lLista[i], lIni, Pos('</' + pTag + '>', lLista[i]) - lIni );
  end;

begin
  lLista := TStringList.Create;
  lLista.Text := Value;

  lOldAutoCreate := Self.AutoCreateParam;
  Self.AutoCreateParam := True;
  
  Self.Clear;
  try
    for i := 0 to lLista.Count - 1 do
    begin
      lNome := GetTagValue('NAME');
      lValor := Algoritimos.Base64DecompressedString( GetTagValue('VALUE') );
      lTipo := GetTagValue('TYPE');

      with ParamByName(lNome) do
      begin
        if Trim(lValor) = C_NULL_TCPVALUE then
          Value := Null
        else
          Value := lValor;
        DataType := TFieldType( StrToInt(lTipo) );
      end;
    end;
  finally
    FreeAndNil(lLista);
  end;

  Self.AutoCreateParam := lOldAutoCreate;
end;

{ TDataSetParamItem }

function TDataSetParamItem.Serializar: string;
var
  lStr : string;
begin
  if (Value = Null) or (Value = Unassigned) then
    lStr := C_NULL_TCPVALUE
  else
    lStr := Algoritimos.Base64CompressedString( VarToStrDef(Value, EmptyStr) );

  Result := Format('<TYPE>%3.3d</TYPE><NAME>%s</NAME><VALUE>%s</VALUE>', [Ord(Self.DataType), Name, lStr] );
end;

{ TLRDataFlashUtils }

class function TLRDataFlashUtils.GetIpComputadorLocal: string;
var
  lStackWin : TIdStackWindows;
begin
  lStackWin := TIdStackWindows.Create;
  try
    Result := lStackWin.LocalAddress;
  finally
    lStackWin.Free;
  end;
end;

class function TLRDataFlashUtils.GetNomeComputadorLocal: string;
var
  lStackWin : TIdStackWindows;
begin
  lStackWin := TIdStackWindows.Create;
  try
    Result := lStackWin.HostByAddress(lStackWin.LocalAddress);
  finally
    lStackWin.Free;
  end;
end;

{ TFtpFileInfo }

procedure TFtpFileInfo.Decode(const AString: string);
var
  lInfo: TStrings;
begin
  lInfo := TStringList.Create;
  try
    lInfo.Text  := StringReplace(WrapText(AString, sLineBreak, ['|'], 1), '|', '', [rfReplaceAll]);
    FileID     := lInfo.Values[C_FTP_ID_MARK];
    FileSize   := StrToInt64Def(lInfo.Values[C_FTP_SIZE_MARK], 0);
    FileDelete := lInfo.Values[C_FTP_DELETE_RECIVE_MARK] = 'T';
    FileName   := lInfo.Values[C_FTP_FILENAME_MARK];
  finally
    FreeAndNil(lInfo);
  end;
end;

end.
