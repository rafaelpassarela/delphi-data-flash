unit uRpDataFlash.Types;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  SysUtils, Classes, Contnrs, DB, uRpAlgorithms, StrUtils,
  DBConsts, Variants, uRpSerialization;

const
  TAG_RET_ERROR = 'ERROR';
  MSG_ECHO = 'ECCHO';
  DEFAULT_ITEM_ID = 'ID_IP_GT_IT';
  TAG_RET_INVALID = '\*INVALID*\';
  TAG_COMMAND ='Command';
  TAG_CALLBACK = 'CALLBACK';
  TAG_GET_CLASSES_PROXY = 'GET_CLASSES_PROXY';
  TAG_PING = '\*PING*\';
  C_NULL_TCPVALUE = '[::[NULL]::]';
  // control commands
  C_CMD_BEGIN = 'CMD_BEGIN';
  C_CMD_START = 'CMD_START';
  C_CMD_END = 'CMD_END';
  C_CMD_MORE = 'CMD_MORE_DATA';

  C_VERIFIED_COMMAND_CONTROLLER : array[0..3] of string = (
    C_CMD_BEGIN,
    C_CMD_START,
    C_CMD_END,
    C_CMD_MORE);

  C_WITHOUT_GROUP = 'UNNAMED';
  C_WHERE_DATASET = 'internal_OpenWhere';
  C_TMP_UNIT_CLASS = '{uClassUnit}';
  // parametros de retorno fixos
  C_PARAM_INT_EXCEPTION = 'exec_Exception';
  C_PARAM_INT_STATUS_RET = 'exec_StatusRetorno';
  C_PARAM_INT_STATUS_PROC = 'InternalStatusProcessamento';
  C_PARAM_INT_FORMAT_TYPE = 'internal_FormatType';
  C_GROUP_INTERNAL = '_internal_';
  C_GROUP_DATASET = '_dataset_provider_';
  C_REST_PARAM = '_rest_prm_http_';
  C_REST_CONTENT_TYPE = 'application/delphi';
  C_FTP_DEFAULT_USER = 'tcpcomftpuser';
  C_FTP_DEFAULT_PWD  = '@ftpusertcpcom#';
  C_FTP_ID_MARK = 'ID';
  C_FTP_SIZE_MARK = 'SIZE';
  C_FTP_DELETE_RECIVE_MARK = 'DEL';
  C_FTP_FILENAME_MARK = 'FILENAME';

type
  TRpDataFlashDataSetOperations = (
    opdsSelect,            //0
    opdsInsert,            //1
    opdsDelete,            //2
    opdsUpdate,            //3
    opdsApplyUpdates,      //4
    opdsStartTrans,        //5
    opdsCommit,            //6
    opdsCommitRetaining,   //7
    opdsRollback,          //8
    opdsRollbackRetaining, //9
    opdsPrepare);          //10

  TRpDataFlashStatusType = (stPreparingData, stSendingData, stReceivingData);
  TRpDataFlashEncryptionType = (ecNone, ecCustom, tcBase64, tcBase64Compressed);
  TRpDataFlashParamType = (tpInput, tpOutput, tpInternal, tpInputNoReload);
  TRpDataFlashParamValueType = (
    tvpInteger, tvpString, tvpBoolean, tvpFloat, tvpBase64, tvpDAO, tvpBase,
    tvpDateTime, tvpFile, tvpJSON, tvpBinaryFile);
  TRpDataFlashProcessType = (prtLocal, prtRemote, prtRemoteOnly);
  TRpDataFlashLifeCycle = (tlfInstance, tlfSession, tlfServer, tlfInternal);
  TRpDataFlashHelperAction = (haSave, haLoad, haDelete, haExecute);
  TRpDataFlashMessageType = (mtCommand, mtText);
  TRpDataFlashValidationOrigin = (voReceiving, voSending);
  TRpDataFlashLoadType = (loSend, loReceive);
  TRpDataFlashProcessingStatus = (psServer, psBridgeOnLine, psBridgeOffLine, psLocal, psNone);
  TRpDataFlashExecutionType = (etExecution, etBridgeInvalid, etBridgeDone, etBeforeExecBridge);
  TRpDataFlashCommunicationType = (ctText, ctStream, ctCompressedStream, ctChar);
//  TRpDataFlashServerType = (stServer, stBridge);
  TRpDataFlashServiceLog = (slOnConnection, slOnDisconnection, slOnSend, slOnReceive,
    slOnError, slOnStatus, slOnBridge, slOnCommand, slOnApplyRule, slOnFile, slOnSync, slOnSyncXml);

  TRpDataFlashOnConnectEvent = procedure of object;
  TRpDataFlashOnExceptionHandler = procedure(const E: Exception) of object;
  TRpDataFlashOnNoService = procedure(Sender : TObject; var AServer: string; var APort : Integer;
    const AException : Exception; var AReConnect : Boolean) of object;
  TRpDataFlashOnConnectOnServer = procedure(Sender : TObject; const AServer: string;
    const APort : Integer) of object;
  TRpDataFlashOnExecuteMessage = function (Sender : TObject) : Boolean;
  TRpDataFlashOnStatus = procedure(Sender : TObject; const AStatus: TRpDataFlashStatusType;
    const AProcTotal, AProcCurrent : Integer; const AStatusStr : string) of object;
  TRpDataFlashOnObjectRequest = procedure(const AAction : TRpDataFlashHelperAction;
    const AObject : TCustomSerializableObject; out AContinue : Boolean) of object;
  TRpDataFlashBusyCallback = procedure (const AStart : Boolean) of object;
//  TRpDataFlashInternalCommandExecution = function : Boolean of object;
//  TRpDataFlashExternalCommandExecution = function : string of object;

  // Exceptions
  ERpDataFlashException = class(Exception);
  ERpDataFlashSending = class(ERpDataFlashException);
  ERpDataFlashConnectionError = class(ERpDataFlashException);
  ERpDataFlashParamNotFound = class(ERpDataFlashException);
  ERpDataFlashInvalidConnection = class(ERpDataFlashException);
  ERpDataFlashAuthError = class(ERpDataFlashException);
  ERpDataFlashUserNotFound = class(ERpDataFlashException);
  ERpDataFlashBeforeExecuteCommandError = class(ERpDataFlashException);
  ERpDataFlashFTPError = class(ERpDataFlashException);
  ERpDataFlashProtocolGuid = class(ERpDataFlashException);
//  ERpDataFlashFileTransferError = class(ERpDataFlashException);
//  ERpDataFlashCommunication = class(ERpDataFlashException);
//  ERpDataFlashReceive = class(ERpDataFlashException);
//  ERpDataFlashInvalidMessage = class(ERpDataFlashException);
//  ERpDataFlashDivergentMessage = class(ERpDataFlashException);
//  ERpDataFlashNotAllowed = class(ERpDataFlashException);
//  ERpDataFlashExecution = class(ERpDataFlashException);

//  ERpDataFlashExceptionClass = class of ERpDataFlashException;

  TRpDataFlashClientInfo = packed record
    DisplayName: string;
    IP: string;
    PeerIP: string;
    GUIDComando: string;
  public
    procedure Initialize;
  end;

  IRpDataFlashConfig = interface
  ['{5C8BB3EE-988F-4F17-8376-23055FADBCA0}']
    function GetServerName : string;
    function GetServerPort: Integer;
    function GetRestPort : Integer;
    function GetFTPPort: Integer;
    function GetCommunicationType: TRpDataFlashCommunicationType;
    function GetEncryptionType: TRpDataFlashEncryptionType;
    function GetLocalHostToIP : Boolean;

    property ServerName : string read GetServerName;
    property ServerPort : Integer read GetServerPort;
    property RestPort : Integer read GetRestPort;
    property FTPPort : Integer read GetFTPPort;
    property CommunicationType : TRpDataFlashCommunicationType read GetCommunicationType;
    property EncryptionType : TRpDataFlashEncryptionType read GetEncryptionType;
    property LocalHostToIP : Boolean read GetLocalHostToIP;
  end;

  IRpDataFlashFileTransferSupport = interface
    ['{801E5FDB-875A-434C-AF58-DA050D1EBE57}']
    function GetFileTransfer_TempDir : string;
    function GetFileTransfer_Port : Integer;
    function FileTransfer_RegisterFile(const AFileID : string; const AFileName : string) : Boolean;
    procedure OnFileTransferLog(const ALogMessage : string);
  end;

  TRpDataFlashFtpFileInfo = packed record
    FileID: string;
    FileName:  string;
    FileSize: Int64;
    FileDelete: Boolean;
  public
    procedure Decode(const AString : string);
  end;

  TRpDataFlashCustomProvider = class(TPersistent)
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

  TRpDataSetParamItem = class(TParam)
  public
    function Serialize : string;
  end;

  TRpDataSetParams = class(TParams)
  private
    FAutoCreateParam: Boolean;
  public
    function ParamByName(const Value: string): TParam;
    function GetAsString : string;
    procedure SetFromString(const Value : string);
    property AutoCreateParam : Boolean read FAutoCreateParam write FAutoCreateParam;
  end;

implementation

{ TRpDataFlashClientInfo }

procedure TRpDataFlashClientInfo.Initialize;
begin
  PeerIP := EmptyStr;
  DisplayName := EmptyStr;
  IP := EmptyStr;
  GUIDComando := EmptyStr;
end;

{ TRpDataFlashCustomProvider }

procedure TRpDataFlashCustomProvider.Clear;
begin
  FInsertSQL.Clear;
  FUpdateSQL.Clear;
  FDeleteSQL.Clear;
  FSelectSQL.Clear;
  FCustomCommand := EmptyStr;
end;

constructor TRpDataFlashCustomProvider.Create;
begin
  FInsertSQL := TStringList.Create;
  FUpdateSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
  FSelectSQL := TStringList.Create;
end;

destructor TRpDataFlashCustomProvider.Destroy;
begin
  FreeAndNil(FInsertSQL);
  FreeAndNil(FUpdateSQL);
  FreeAndNil(FDeleteSQL);
  FreeAndNil(FSelectSQL);
  inherited;
end;

function TRpDataFlashCustomProvider.GetAsString: string;
var
  lStr : TStringList;
begin
  lStr := TStringList.Create;
  try
    lStr.Add('CustomCommand=' + Algorithms.Base64CompressedString( FCustomCommand ) );
    lStr.Add('Insert=' + Algorithms.Base64CompressedString( FInsertSQL.Text ) );
    lStr.Add('UpdateSQL=' + Algorithms.Base64CompressedString( FUpdateSQL.Text ) );
    lStr.Add('DeleteSQL=' + Algorithms.Base64CompressedString( FDeleteSQL.Text ) );
    lStr.Add('SelectSQL=' + Algorithms.Base64CompressedString( FSelectSQL.Text ) );
    Result := lStr.Text;
  finally
    FreeAndNil(lStr);
  end;
end;

procedure TRpDataFlashCustomProvider.SetDeleteSQL(const Value: TStrings);
begin
  FDeleteSQL.Assign( Value );
end;

procedure TRpDataFlashCustomProvider.SetFromString(const Value: string);
var
  lStr : TStringList;
begin
  lStr := TStringList.Create;
  lStr.Text := Value;
  try
    FCustomCommand := Algorithms.Base64DecompressedString( lStr.Values['CustomCommand'] );
    FInsertSQL.Text := Algorithms.Base64DecompressedString( lStr.Values['Insert'] );
    FUpdateSQL.Text := Algorithms.Base64DecompressedString( lStr.Values['UpdateSQL'] );
    FDeleteSQL.Text := Algorithms.Base64DecompressedString( lStr.Values['DeleteSQL'] );
    FSelectSQL.Text := Algorithms.Base64DecompressedString( lStr.Values['SelectSQL'] );
  finally
    FreeAndNil(lStr);
  end;
end;

procedure TRpDataFlashCustomProvider.SetInsertSQL(const Value: TStrings);
begin
  FInsertSQL.Assign( Value );
end;

procedure TRpDataFlashCustomProvider.SetSelectSQL(const Value: TStrings);
begin
  FSelectSQL.Assign( Value );
end;

procedure TRpDataFlashCustomProvider.SetUpdateSQL(const Value: TStrings);
begin
  FUpdateSQL.Assign( Value );
end;

{ TRpDataSetParams }

function TRpDataSetParams.GetAsString: string;
var
  lList : TStrings;
  i: Integer;
begin
  lList := TStringList.Create;
  for i := 0 to Self.Count - 1 do
    lList.Add( TRpDataSetParamItem(Self[i]).Serialize );
  Result := Trim( lList.Text );
  FreeAndNil(lList);
end;

function TRpDataSetParams.ParamByName(const Value: string): TParam;
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

procedure TRpDataSetParams.SetFromString(const Value: string);
var
  lList : TStrings;
  i: Integer;
  lName: string;
  lValue: string;
  lType: string;
  lOldAutoCreate: Boolean;

  function GetTagValue(const pTag : string) : string;
  var
    lIni: Integer;
  begin
    lIni := Pos('<' + pTag + '>', lList[i]) + Length(pTag) + 2;
    Result := Copy(lList[i], lIni, Pos('</' + pTag + '>', lList[i]) - lIni );
  end;

begin
  lList := TStringList.Create;
  lList.Text := Value;

  lOldAutoCreate := Self.AutoCreateParam;
  Self.AutoCreateParam := True;

  Self.Clear;
  try
    for i := 0 to lList.Count - 1 do
    begin
      lName := GetTagValue('NAME');
      lValue := Algorithms.Base64DecompressedString( GetTagValue('VALUE') );
      lType := GetTagValue('TYPE');

      with ParamByName(lName) do
      begin
        if Trim(lValue) = C_NULL_TCPVALUE then
          Value := Null
        else
          Value := lValue;
        DataType := TFieldType( StrToInt(lType) );
      end;
    end;
  finally
    FreeAndNil(lList);
  end;

  Self.AutoCreateParam := lOldAutoCreate;
end;

{ TRpDataSetParamItem }

function TRpDataSetParamItem.Serialize: string;
var
  lStr : string;
begin
  if (Value = Null) or (Value = Unassigned) then
    lStr := C_NULL_TCPVALUE
  else
    lStr := Algorithms.Base64CompressedString( VarToStrDef(Value, EmptyStr) );

  Result := Format('<TYPE>%3.3d</TYPE><NAME>%s</NAME><VALUE>%s</VALUE>', [Ord(Self.DataType), Name, lStr] );
end;

{ TRpDataFlashFtpFileInfo }

procedure TRpDataFlashFtpFileInfo.Decode(const AString: string);
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
