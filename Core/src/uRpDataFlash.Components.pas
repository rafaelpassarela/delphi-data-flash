unit uRpDataFlash.Components;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, IdContext, IdTCPServer, IdIOHandler, ActiveX, Contnrs, IdStackWindows,
  XMLIntf, XMLDoc, Controls, IdSync, SysUtils, Windows, ZLib, DateUtils, IdFTP,
  IdExceptionCore, IdCustomHTTPServer, IdHTTPServer, IdFTPServer, IdComponent,
  IdHTTP, uRpJsonBase, IdTCPClient, IdURI, uRpSerialization, uRpDataFlash.Types,
  uRpDataFlash.Protocol, uRpDataFlash.Command, uRpDataFlash.ThreadConnection,
  uRpDataFlash.Connection, uRpDataFlash.CommandController, uRpDataFlash.ConvertUtils,
  uRpDataFlash.Utils;

type
  TRpDataFlashConnectionItem = class;
  TRpDataFlashCustomConnection = class;
  TRpDataFlashCustomClientConnection = class;
  TRpDataFlashServerConnection = class;
  TRpDataFlashCommandController = class;
  TRpDataFlashCommandControllerList = class;
  TRpDataFlashProviderControllerList = class;

  TRpDataFlashOnGenericReceipt = function(Sender : TObject; const AMenssage : string; AConnectionItem : TRpDataFlashConnectionItem) : string of object;
  TRpDataFlashOnReceiving = function(Sender : TObject; AConnectionItem : TRpDataFlashConnectionItem) : string of object;
  TRpDataFlashOnNewLog = procedure(Sender : TObject; ALogType : TRpDataFlashServiceLog; const ALog : string; const AClientInfo : TRpDataFlashClientInfo) of object;
  TRpDataFlashOnException = procedure(Sender : TObject; AException : Exception; var ARaise : Boolean) of object;
  TRpDataFlashOnBeforeClientConnection = procedure(Sender : TObject; var AAllow : Boolean) of object;
  TRpDataFlashOnClientConnection = procedure(Sender : TObject; const AConnectionItem : TRpDataFlashConnectionItem) of object;
  TRpDataFlashOnSendingError = procedure(Sender : TObject; const AProtocol : TRpDataFlashProtocol; const AException : Exception) of object;
  TRpDataFlashOnManualProcess = procedure(Sender : TObject; const AConnectionItem : TRpDataFlashConnectionItem) of object;

  TRpDataFlashOnCriarExecutor = procedure(Sender : TObject; out AExecutor : IRpPackageCommandExecutor) of object;
  TRpDataFlashOnAutenticarCliente = procedure(Sender : TObject; const AConnectionItem : IAutenticationProvider; out AAutenticado : Boolean; out AErrorMessage : string) of object;
  TRpDataFlashOnTimeOutCheck = procedure(Sender : TObject; const AOrigem : TRpDataFlashValidationOrigin; var AContinue : Boolean) of object;
  TRpDataFlashOnBeforeExecuteCommand = procedure(Sender : TObject; const AConnectionItem : TRpDataFlashConnectionItem; var AContinue : Boolean; out AMessage : string) of object;
  // DataSet
  TRpDataFlashOnExecSQL = function (const ACommand: IRpDataFlashCommandInterfaced; var ASQL: string; var AContinue : Boolean) : Boolean of object;
  TRpDataFlashOnSelect = function (const ACommand: IRpDataFlashCommandInterfaced; var ASelectSQL : string; var AContinue : Boolean): Boolean of object;
  TRpDataFlashOnTransactionEvent = function (const ACommand: IRpDataFlashCommandInterfaced; const ARetaining : Boolean; var AContinue : Boolean) : Boolean of object;
  TRpDataFlashOnStartTransactionEvent = function(const ACommand: IRpDataFlashCommandInterfaced; var AContinue : Boolean) : Boolean of object;

  IRpDataFlashExecutorCallBack = interface
  ['{E1E31424-BC86-4207-AC11-BD7C2FB1549E}']
    function ExecutarCallBack(const AParametrosCallback : TRpDataFlashCommandParameterList) : Boolean;
  end;

  IRpTCPMonitorEventos = interface
  ['{6B2F598C-7BE8-4ABD-A00B-1CD9C145B5C6}']
    procedure MonitorarStatus(Sender : TObject; const ASituacao : TRpDataFlashStatusType;
      const AProcessamentoTotal, AProcessamentoAtual : Integer);
  end;

  IRpDataFlashExecutorComandController = interface
  ['{0A57044F-0EAF-4837-ABA1-A4F951FA8B09}']
    function GetServer : TRpDataFlashServerConnection;
  end;

  IRpDataFlashConfigConexaoView = interface
  ['{2268E0B2-DDC2-49E2-939C-48B080099BD1}']
    function GetPort: Integer;
    function GetServer: string;
    procedure SetPort(const Value: Integer);
    procedure SetServer(const Value: string);

    property Port : Integer read GetPort write SetPort;
    property Server : string read GetServer write SetServer;

    procedure Cancelar;
    function Connect : Boolean;
  end;

  TRpDataFlashComandHelper = class helper for TRpDataFlashCommand
  public
    function GetServer : TRpDataFlashServerConnection;
  end;

  TRpDataFlashExecutorCallBack = class(TInterfacedObject, IRpDataFlashExecutorCallBack)
  protected
    procedure InternalCallback;
    procedure DoAfterCallback; virtual; abstract;
    function DoBeforeCallback(const AParametrosCallback: TRpDataFlashCommandParameterList) : Boolean; virtual; abstract;
    function AsyncMode : Boolean; virtual;
  public
    function ExecutarCallBack(const AParametrosCallback: TRpDataFlashCommandParameterList): Boolean;
  end;

  TRpDataFlashSyncLogEvent = class(TIdNotify)
  private
    FLRDataFlashIP : TRpDataFlashCustomConnection;
    FEvento : TRpDataFlashOnNewLog;
    FTipoLog : TRpDataFlashServiceLog;
    FLog : string;
    FClientInfo : TRpDataFlashClientInfo;
  protected
    procedure DoNotify; override;
  end;

  TRpDataFlashSyncConexaoEvent = class(TIdNotify)
  private
    FLRDataFlashIP : TRpDataFlashCustomConnection;
    FEvento : TRpDataFlashOnClientConnection;
    FConexaoItem : TRpDataFlashConnectionItem;
  protected
    procedure DoNotify; override;
  end;

  TRpDataFlashSyncConexaoEventNew = class(TIdNotify)
  private
    FLRDataFlashIP : TRpDataFlashCustomConnection;
    FEvento : TRpDataFlashOnCriarExecutor;
    FExecutor : IRpPackageCommandExecutor;
  protected
    procedure DoNotify; override;
  end;

  TRpDataFlashConnectionItem = class(TInterfacedPersistent, ISessionInstanceController, IAutenticationProvider)
  private
    FInterfaceList : TInterfaceList;
    FHandler: TIdIOHandler;
    FClientName: string;
    FExecutor: IRpPackageCommandExecutor;
    FOwner: TRpDataFlashCustomConnection;
    FTcpClientPonte: TRpDataFlashCustomClientConnection;
    FQuebras : TRpDataFlashProtocolBreakerList;
    FOnCallBack: TRpDataFlashCallBackEvent;
    FClientID: string;
    FUsername: string;
    FPassword: string;
    FAutenticado: Boolean;
    function GetAuthenticated: Boolean;
    function GetPassword: string;
    function GetUserName: string;
    procedure SetUsername(const Value : string);
    procedure SetPassword(const Value : string);
    procedure SetAuthenticated(const Value : Boolean);
  public
    constructor Create(AOwner : TRpDataFlashCustomConnection; pHandler : TIdIOHandler);
    destructor Destroy; override;
    function FindInstance(const ACommand: string): IRpDataFlashCommandInterfaced;
    procedure AddInstance(const AInstance: IRpDataFlashCommandInterfaced);
    function Ip: string;

    property ClientName : string read FClientName write FClientName;
    property ClientID : string read FClientID write FClientID;
    property Handler : TIdIOHandler read FHandler write FHandler;
    property Executor : IRpPackageCommandExecutor read FExecutor write FExecutor;
    property TcpClientPonte : TRpDataFlashCustomClientConnection read FTcpClientPonte;
    property Quebras : TRpDataFlashProtocolBreakerList read FQuebras;
    property Username : string read GetUserName write SetUsername;
    property Password : string read GetPassword write SetPassword;
    property Authenticated : Boolean read GetAuthenticated write SetAuthenticated;
    property OnCallBack : TRpDataFlashCallBackEvent read FOnCallBack write FOnCallBack;
  end;

  TRpDataFlashCustomConfigConexao = class(TPersistent)
  private
    FEnabled: Boolean;
    FPort: Integer;
  public
    constructor Create; virtual;
    property Enabled : Boolean read FEnabled write FEnabled;
    property Port : Integer read FPort write FPort;
  end;

  TRpDataFlashConfigConexaoTCPIP = class(TRpDataFlashCustomConfigConexao)
  public
    constructor Create; override;
  published
    property Enabled default True;
    property Port default 8890;
  end;

  TRpDataFlashConfigConexaoREST = class(TRpDataFlashCustomConfigConexao)
  public
    constructor Create; override;
  published
    property Enabled default False;
    property Port default 9088;
  end;

  TRpDataFlashFileTransfer = class(TPersistent)
  private
    FPort: Integer;
    FTempDir: string;
    FEnabled: Boolean;
    function GetDataPort: Integer;
  public
    constructor Create;
  published
    property Port : Integer read FPort write FPort default 8891;
    property DataPort : Integer read GetDataPort default 8892;
    property TempDir : string read FTempDir;
    property Enabled : Boolean read FEnabled write FEnabled default True;
  end;

  TRpDataFlashCustomConnection = class(TComponent, IRpDataFlashFileTransferSupport)
  private
    FServer : string;
    FOnInternalProcess: TRpDataFlashOnReceiving;
    FOnNewLog: TRpDataFlashOnNewLog;
    FOnException: TRpDataFlashOnException;
    FEncryptionType: TRpDataFlashEncryptionType;
    FIpsReconhecidos: TStringList;
    FQuebrasProtocolosRecebidos: TRpDataFlashProtocolBreakerList;
    FCommunicationType: TRpDataFlashCommunicationType;
    FMessageType: TRpDataFlashMessageType;
    FOnTimeOutCheck: TRpDataFlashOnTimeOutCheck;
    FOnStatus: TRpDataFlashOnStatus;
    FConfigTCPIP: TRpDataFlashConfigConexaoTCPIP;
    FConfigREST: TRpDataFlashConfigConexaoREST;
    FFileTransfer: TRpDataFlashFileTransfer;
    FFileTransferList : TStrings;
    procedure SetEncryptionType(const Value: TRpDataFlashEncryptionType);

    procedure CompressStream(var AEntrada : TMemoryStream); overload;
    procedure DecompressStream(var AEntrada : TMemoryStream); overload;
    procedure SetServidor(const Value: string); virtual;
  protected
    function GetIdentifier: string; virtual;
    function GetConnected: Boolean; virtual;
    function GetLocalComputerName : string;

    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;

    procedure NewException(const AException : Exception; out AExpandir : Boolean); overload;
    procedure NewException(const AException : Exception); overload;

    function CreateException(const AException : Exception; const AResultType : TSerializationFormat = sfJSON) : string;
    procedure TentaGerarException(const AProtocol : TRpDataFlashProtocol); overload;
    function TentaGerarException(const AMenssage : string; out AException : string) : Boolean; overload;

    procedure Inicializar; virtual;
    procedure Finalizar; virtual;
    procedure DoSaveConfig(const ANode : IXMLNode); virtual;
    procedure DoLoadConfig(const ANode : IXMLNode); virtual;

    procedure NewLog(const ALogType : TRpDataFlashServiceLog; const ALog : string;
      const AContext : TIdContext); overload;
    procedure NewLog(const ALogType : TRpDataFlashServiceLog; const ALog : string;
      const AIP : string); overload;

    procedure Clear; virtual;

    function isComandoDesejado(const pComando, pMensagem : string) : Boolean;
    procedure SeparaComando(const pComandoCompleto : string; out ACommand, AGuid : string);

    procedure DoInternalEnviar(const AHandler : TIdIOHandler; ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo; const AValor : string; const AContext : TIdContext;
      const AArquivo : string);
    procedure InternalEnviar(const AHandler : TIdIOHandler; const AValor : string); virtual;
    function InternalReceber(const AHandler : TIdIOHandler) : string; overload;
    function InternalReceber(ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo) : string; overload;

    procedure GerarStatus(const ASituacao : TRpDataFlashStatusType;
      const AProcessamentoTotal, AProcessamentoAtual : Integer;
      const AStatusMens : string);

    function FileTransfer_RegisterFile(const AFileID: string; const AFileName : string): Boolean;
    function GetFileTransfer_Port: Integer;
    function GetFileTransfer_TempDir: string;
    procedure OnFileTransferLog(const ALogMessage: string);
    procedure RemoverArquivosTemporarios;

    property ConfigTCPIP : TRpDataFlashConfigConexaoTCPIP read FConfigTCPIP write FConfigTCPIP;
    property ConfigREST : TRpDataFlashConfigConexaoREST read FConfigREST write FConfigREST;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SaveConfig(const ANode : IXMLNode); overload;
    procedure SaveConfig(const AArquivo : string); overload;
    procedure LoadConfig(const ANode : IXMLNode); overload;
    procedure LoadConfig(const AArquivo : string); overload;

    procedure Connect;
    procedure Disconnect;

    function GetFile(const AFileID : string; AArquivo : IRpFileProxy) : Boolean; virtual;
    function PutFile(const AArquivo : IRpFileProxy) : Boolean; virtual;

    property OnInternalProcess : TRpDataFlashOnReceiving read FOnInternalProcess write FOnInternalProcess;
    property Server : string read FServer write SetServidor stored True;
    property Connected : Boolean read GetConnected;
    property EncryptionType : TRpDataFlashEncryptionType read FEncryptionType write SetEncryptionType stored True default tcBase64Compressed;
    property Identifier : string read GetIdentifier;
    property CommunicationType : TRpDataFlashCommunicationType read FCommunicationType write FCommunicationType stored True default ctText;
    property MessageType : TRpDataFlashMessageType read FMessageType write FMessageType stored True default mtCommand;
    property FileTransfer : TRpDataFlashFileTransfer read FFileTransfer write FFileTransfer;

    property OnNewLog : TRpDataFlashOnNewLog read FOnNewLog write FOnNewLog;
    property OnException : TRpDataFlashOnException read FOnException write FOnException;
    property OnStatus : TRpDataFlashOnStatus read FOnStatus write FOnStatus;
    property OnTimeOutCheck : TRpDataFlashOnTimeOutCheck read FOnTimeOutCheck write FOnTimeOutCheck;
  end;

  TRpDataFlashServerConnection = class(TRpDataFlashCustomConnection, IServerInstanceController)
  private
    FConectorTCP : TIdTCPServer;
    FConectorREST : TIdHTTPServer;
    FConectorFTP : TIdFTPServer;

    FOnBeforeClientConnection: TRpDataFlashOnBeforeClientConnection;
    FOnClientConnection: TRpDataFlashOnClientConnection;
    FOnClientDisconnect: TRpDataFlashOnClientConnection;
    FDesconectandoServer : Boolean;
    FBridge: TRpDataFlashCustomClientConnection;
    FControllers: TRpDataFlashCommandControllerList;
    FUtilizarControllers: Boolean;
    FProviders: TRpDataFlashProviderControllerList;
    FOnReceivingText: TRpDataFlashOnGenericReceipt;
    FInterfaceList : TInterfaceList;
    FOnManualProcessing: TRpDataFlashOnManualProcess;
    FOnAuthenticateClient: TRpDataFlashOnAutenticarCliente;
    FOnBeforeExecuteCommand: TRpDataFlashOnBeforeExecuteCommand;
    FOnBeforeDataSetStartTransaction: TRpDataFlashOnStartTransactionEvent;
    FOnBeforeDataSetCommitTransaction: TRpDataFlashOnTransactionEvent;
    FOnBeforeDataSetRollbackTransaction: TRpDataFlashOnTransactionEvent;
    FWithoutAuthCommands: string;
    FBaseCommandPrefix: string;
    FNumeroConectados: Integer;
    FOnObjectRequest: TRpDataFlashOnObjectRequest;
    FAuthEnable: Boolean;
    function EnviarCallBack(const AContext: TIdContext; const AMenssage : string) : Boolean;
    procedure NotificarConexaoCliente(const AConnectionItem : TRpDataFlashConnectionItem;
      const AEvento : TRpDataFlashOnClientConnection);

    function Conector : TIdTCPServer;

    procedure AoExecutarNoServidorRest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure AoExecutarNoServidor(AContext: TIdContext);
    procedure AoConectarCliente(AContext: TIdContext);
    procedure AoDesconectarCliente(AContext: TIdContext);
//    procedure DoFreeExecutor(AExecutor : IExecutorComandoPacote);

    function ItemConexao(AContext: TIdContext) : TRpDataFlashConnectionItem;

    function ExecutarComando(const AItem : TRpDataFlashConnectionItem; AContext: TIdContext;
      const AProtocol : TRpDataFlashProtocol; out ASaida : string; out AArquivo : string;
      const ARequestInfo: TIdHTTPRequestInfo = nil; AResponseInfo: TIdHTTPResponseInfo = nil) : Boolean;

    procedure Receber(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo = nil;
      AResponseInfo: TIdHTTPResponseInfo = nil);

    function GetServidor: string;
    procedure SetBridge(const Value: TRpDataFlashCustomClientConnection);
    function CarregarConexaoCliente(const AConnectionItem : TRpDataFlashConnectionItem; out AConexao: TRpDataFlashCustomClientConnection) : Boolean;
    function CarregarStatusProcessamento(const AConnectionItem : TRpDataFlashConnectionItem;
      out AConexao: TRpDataFlashCustomClientConnection) : TRpDataFlashProcessingStatus;
    function GetConnectedClientsCount: Integer;
    // controllers e providers
    function CarregarComandoViaControllers(const ACommand : string; out AObjComando : IRpDataFlashCommandInterfaced;
      out AParametros : TRpDataFlashCommandParameterList) : Boolean;
    function CarregarComandoViaProviders(const ACommand : string; out AObjComando : IRpDataFlashCommandInterfaced;
      out AParametros : TRpDataFlashCommandParameterList) : Boolean;
    function GetControllersCount: Integer;
    function GetProvidersCount: Integer;
    function GetUseControllers : Boolean;
    procedure SetUseControllers(const Value : Boolean);
    function GetControllers: TRpDataFlashCommandControllerList;
    function GetProviders: TRpDataFlashProviderControllerList;
    // ftp
    procedure OnFTPServerAfterUserLogin(ASender: TIdFTPServerContext);
    procedure OnFTPServerUserLogin(ASender: TIdFTPServerContext;
      const AUsername, APassword: string; var AAuthenticated: Boolean);
    procedure OnFTPServerRetrieveFile(ASender: TIdFTPServerContext;
      const AFileName: string; var VStream: TStream);
    procedure OnFTPServerException(AContext: TIdContext; AException: Exception);
    procedure OnFTPServerStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure OnFTPServerDeleteFile(ASender: TIdFTPServerContext; const APathName: string);
    procedure OnFTPServerStoreFile(ASender: TIdFTPServerContext; const AFileName: string;
      AAppend: Boolean; var VStream: TStream);
  protected
    procedure Inicializar; override;
    procedure Finalizar; override;
    function GetConnected: Boolean; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure Clear; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ComandoRequerAutenticacao(const AClasseComando : string;
      const AParametros : TRpDataFlashCommandParameterList) : Boolean;
    function IsPonte : Boolean;
  public
    procedure AfterConstruction; override;
    procedure AddInstance(const AInstance: IRpDataFlashCommandInterfaced);
    function FindInstance(const ACommand: string): IRpDataFlashCommandInterfaced;
    property Controllers : TRpDataFlashCommandControllerList read GetControllers;
    property Providers : TRpDataFlashProviderControllerList read GetProviders;
  published
    property ConfigTCPIP;
    property ConfigREST;
    property Identifier;
    property CommunicationType;
    property MessageType;
    property FileTransfer;
    property ConnectedClientsCount : Integer read GetConnectedClientsCount;
    property Connected;
    property EncryptionType;

    property OnNewLog;
    property OnException;
    property OnStatus;
    property OnTimeOutCheck;
    property OnInternalProcess;

    property OnBeforeClientConnection : TRpDataFlashOnBeforeClientConnection read FOnBeforeClientConnection write FOnBeforeClientConnection;
    property OnClientConnection : TRpDataFlashOnClientConnection read FOnClientConnection write FOnClientConnection;
    property OnClientDisconnect : TRpDataFlashOnClientConnection read FOnClientDisconnect write FOnClientDisconnect;
    property OnReceivingText : TRpDataFlashOnGenericReceipt read FOnReceivingText write FOnReceivingText;
    property OnManualProcessing : TRpDataFlashOnManualProcess read FOnManualProcessing write FOnManualProcessing;
    property OnAuthenticateClient : TRpDataFlashOnAutenticarCliente read FOnAuthenticateClient write FOnAuthenticateClient;
    property OnBeforeExecuteCommand : TRpDataFlashOnBeforeExecuteCommand read FOnBeforeExecuteCommand write FOnBeforeExecuteCommand;
    property OnObjectRequest : TRpDataFlashOnObjectRequest read FOnObjectRequest write FOnObjectRequest;

    property Bridge : TRpDataFlashCustomClientConnection read FBridge write SetBridge;
    property Server : string read GetServidor;
    property ControllersCount : Integer read GetControllersCount;
    property ProvidersCount : Integer read GetProvidersCount;
    property UseControllers : Boolean read GetUseControllers write SetUseControllers default True;
    property WithoutAuthCommands : string read FWithoutAuthCommands write FWithoutAuthCommands;
    property BaseCommandPrefix : string read FBaseCommandPrefix write FBaseCommandPrefix;
    property AuthEnable : Boolean read FAuthEnable write FAuthEnable default False;

    property OnBeforeDataSetStartTransaction : TRpDataFlashOnStartTransactionEvent read FOnBeforeDataSetStartTransaction write FOnBeforeDataSetStartTransaction;
    property OnBeforeDataSetCommitTransaction : TRpDataFlashOnTransactionEvent read FOnBeforeDataSetCommitTransaction write FOnBeforeDataSetCommitTransaction;
    property OnBeforeDataSetRollbackTransaction : TRpDataFlashOnTransactionEvent read FOnBeforeDataSetRollbackTransaction write FOnBeforeDataSetRollbackTransaction;
  end;

  TRpDataFlashCustomClientConnection = class(TRpDataFlashCustomConnection)
  private
    FThreadConexao: TRpDataFlashThreadConnection;
    FConnectionHelper: TRpDataFlashConnectionHelperCustom;
    FOnDisconnect: TRpDataFlashOnConnectOnServer;
    FOnConnect: TRpDataFlashOnConnectOnServer;
    FOnNoService: TRpDataFlashOnNoService;
    FLoginPrompt: Boolean;
    FOnSendingError: TRpDataFlashOnSendingError;
    FReadTimeOut: Integer;
    FConnectionTimeOut: Integer;
    FReconnectionTimeOut: Integer;
    FUltimaConexao : TDateTime;
    FLazyConnection: Boolean;
    FUserName: string;
    FPassword: string;

    FExecutorCallBackClass: string;
    FExecutorCallBackInterfaced: IRpDataFlashExecutorCallBack;

    FOnAfterConnect: TRpDataFlashOnConnectOnServer;
    FOnBeforeConnect: TRpDataFlashOnConnectOnServer;
    FConvertLocalHostToIP: Boolean;
    FSerializationFormat: TSerializationFormat;
    function Enviar(const AIdentificador, AMenssage, ANomeComando : string) : string; virtual;
    function PodeConectar : Boolean;
    procedure DoNovaExcecao(const E:Exception);
    procedure TentaExecutarCallBack(const AProtocol : TRpDataFlashProtocol);
    function GetPort: Integer; virtual;
    procedure SetPort(const Value: Integer); virtual;
    procedure SetServidor(const Value: string); override;
    procedure OnFTPClientStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
  protected
    property ExecutorCallBackClass : string read FExecutorCallBackClass write FExecutorCallBackClass;
    property ExecutorCallBackInterfaced : IRpDataFlashExecutorCallBack read FExecutorCallBackInterfaced write FExecutorCallBackInterfaced;
    function GetIdentifier: string; override;
    function GetConnected: Boolean; override;
    function GetListaItensServidor : string;
    function InternalConectar : Boolean;
    function DoComunicarComThred(const ACommand: TRpDataFlashCommand; const ACallBackClassName: string;
      const AExecutorCallBack : IRpDataFlashExecutorCallBack) : string; virtual; abstract;
    function GetConnectionHelperClass : TRpDataFlashConnectionHelperCustomClass; virtual; abstract;

    procedure Inicializar; override;
    procedure Finalizar; override;

    procedure DoConnect; override;
    procedure DoDisconnect; override;
    procedure DoSaveConfig(const ANode : IXMLNode); override;
    procedure DoLoadConfig(const ANode : IXMLNode); override;
    procedure NovoErroEnvio(const AException : Exception; const AProtocol : TRpDataFlashProtocol);
    procedure Clear; override;
  public
    constructor Create(AOwner : TComponent); override;

    function Comunicar(const AIdentificador : string; const AMenssage : string = ''; const ANomeComando : string = '') : string; overload;
    function Comunicar(const ACommand : TRpDataFlashCommand) : string; overload;
    function Comunicar(const ACommand : IRpDataFlashCommandInterfaced; const AParametros : TRpDataFlashCommandParameterList) : string; overload;
    // chamadas com callback
    function Comunicar(const ACommand: TRpDataFlashCommand; const ACallBackClassName: string): string; overload;
    function Comunicar(const ACommand: TRpDataFlashCommand; const AExecutorCallBack: IRpDataFlashExecutorCallBack): string; overload;
    // autenticações de usuário
    function Autenticar(out AErrorMessage : string) : Boolean; overload;
    function Autenticar(const AUsername, APassword : string; out AErrorMessage : string) : Boolean; overload;

    function ServerOnline : Boolean;
    function ConectarFtp(out ACliente : TIdFTP) : Boolean;
    function GetFile(const AFileID : string; AArquivo : IRpFileProxy) : Boolean; override;
    function PutFile(const AArquivo : IRpFileProxy) : Boolean; override;
    function ConfirmFileReceipt(const AFileID : string; const ADeleteTemp : Boolean;
      const ADeleteOriginal : Boolean; AFtpClient : TIdFTP) : Boolean;

    procedure ClonarDe(const AOther: TRpDataFlashCustomClientConnection; const AClonarEnventos : Boolean = False);
    { .:: --> Ao adicionar uma nova propriedade, verificar funcao "ClonarDe" <-- ::. }
    property Port : Integer read GetPort write SetPort default 8890;
    property Server;
    property Identifier;
    property CommunicationType;
    property LoginPrompt : Boolean read FLoginPrompt write FLoginPrompt default False;
    property ConnectionTimeOut : Integer read FConnectionTimeOut write FConnectionTimeOut stored True default -1;
    property ReadTimeOut : Integer read FReadTimeOut write FReadTimeOut stored True default 0;
    property ReconnectionTimeOut : Integer read FReconnectionTimeOut write FReconnectionTimeOut stored True default 0;
    property LazyConnection : Boolean read FLazyConnection write FLazyConnection stored True default False;
    property UserName : string read FUserName write FUserName;
    property Password : string read FPassword write FPassword;
    property ConvertLocalHostToIP : Boolean read FConvertLocalHostToIP write FConvertLocalHostToIP default False;
    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat default sfAuto;

    property OnBeforeConnect : TRpDataFlashOnConnectOnServer read FOnBeforeConnect write FOnBeforeConnect;
    property OnConnect : TRpDataFlashOnConnectOnServer read FOnConnect write FOnConnect;
    property OnAfterConnect : TRpDataFlashOnConnectOnServer read FOnAfterConnect write FOnAfterConnect;

    property OnDisconnect : TRpDataFlashOnConnectOnServer read FOnDisconnect write FOnDisconnect;
    property OnNoService : TRpDataFlashOnNoService read FOnNoService write FOnNoService;
    property OnSendingError : TRpDataFlashOnSendingError read FOnSendingError write FOnSendingError;
  end;

  TRpDataFlashClientConnection = class(TRpDataFlashCustomClientConnection)
  protected
    function DoComunicarComThred(const ACommand: TRpDataFlashCommand; const ACallBackClassName: string;
      const AExecutorCallBack : IRpDataFlashExecutorCallBack) : string; override;
    function GetConnectionHelperClass : TRpDataFlashConnectionHelperCustomClass; override;

    procedure Inicializar; override;
    procedure Finalizar; override;
    procedure DoConnect; override;
    procedure SetServidor(const Value: string); override;
  published
    property Port;
    property Server;
    property Identifier;
    property CommunicationType;
    property LoginPrompt;
    property ConnectionTimeOut;
    property ReadTimeOut;
    property ReconnectionTimeOut;
    property LazyConnection;
    property UserName;
    property Password;
    property EncryptionType;
    property FileTransfer;
    property ConvertLocalHostToIP;
    property SerializationFormat;

    property OnBeforeConnect;
    property OnConnect;
    property OnAfterConnect;
    property OnDisconnect;
    property OnNoService;
    property OnSendingError;
    property OnException;
    property OnNewLog;
    property OnStatus;
  end;

  TRpDataFlashRESTClient = class(TRpDataFlashCustomClientConnection)
  protected
    function DoComunicarComThred(const ACommand: TRpDataFlashCommand; const ACallBackClassName: string;
      const AExecutorCallBack : IRpDataFlashExecutorCallBack) : string; override;
    procedure InternalEnviar(const AHandler : TIdHTTP; const AValor, ANomeComando : string;
      out AResponse : TStringStream); reintroduce;
  private
    function Enviar(const AIdentificador, AMenssage, ANomeComando : string) : string; override;
    function GetPort: Integer; override;
    procedure SetPort(const Value: Integer); override;
    procedure SetServidor(const Value: string); override;
  public
    constructor Create(AOwner : TComponent); override;
    function GetConnectionHelperClass : TRpDataFlashConnectionHelperCustomClass; override;
  published
    property Port default 9088;
    property Server;
    property Identifier;
    property LoginPrompt;
    property UserName;
    property Password;
    property EncryptionType;
    property FileTransfer;
    property ConvertLocalHostToIP;
    property SerializationFormat;

    property OnBeforeConnect;
    property OnConnect;
    property OnAfterConnect;
    property OnDisconnect;
    property OnNoService;
    property OnSendingError;
    property OnException;
    property OnNewLog;
    property OnStatus;
  end;

  TRpDataFlashThreadInternalAdapter = class(TRpDataFlashCustomClientConnection)
  public
    function DoInternalConectar : Boolean;
  end;

  TRpDataFlashCommandController = class(TComponent)
  private
    FServer: TRpDataFlashServerConnection;
    FComandos: TRpDataFlashCommandList;
    FGrupo: string;
    procedure SetServer(const Value: TRpDataFlashServerConnection);
    procedure SetGroupName(const Value: string);
    function GetGroupName: string;

    function GetCommands: TRpDataFlashCommandList;
    procedure SetCommands(const Value: TRpDataFlashCommandList);
  protected
    function GetComandoClass : TRpDataFlashCommandListClass; virtual;
    function GetComandoItemClass : TRpDataFlashCommandItemClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EditarComandos;
  published
    property Server   : TRpDataFlashServerConnection read FServer write SetServer;
    property Commands : TRpDataFlashCommandList read GetCommands write SetCommands;
    property GroupName : string read GetGroupName write SetGroupName;
  end;

  TRpDataFlashCommandControllerList = class(TComponentList)
  public
    function Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
  end;

  TRpDataFlashProviderControllerList = class(TComponentList)
  public
    function Localizar(const ANome : string; out AObjComando : IRpDataFlashCommandInterfaced) : Boolean;
  end;

  TThreadCallback = class(TThread)
  private
    FConexaoClient: TRpDataFlashCustomClientConnection;
    FResult : string;
    FComando: TRpDataFlashCommand;
    FCallbackClassName: string;
    FExecutorCallBack: IRpDataFlashExecutorCallBack;
    function GetIsTerminated: Boolean;
    procedure SetCallbackClassName(const Value: string);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(const AClient : TRpDataFlashCustomClientConnection; const ACommand: TRpDataFlashCommand;
      const ACallBackClassName: string; const AExecutorCallBack: IRpDataFlashExecutorCallBack);

    property Comando : TRpDataFlashCommand read FComando write FComando;
    property CallbackClassName : string read FCallbackClassName write SetCallbackClassName;
    property ExecutorCallBack : IRpDataFlashExecutorCallBack read FExecutorCallBack write FExecutorCallBack;
    property IsTerminated : Boolean read GetIsTerminated;

    function GetResult : string;
  end;

  TCustomProxyClient = class
  private
    FSerializationFormat: TSerializationFormat;
  protected
    FClient: TRpDataFlashCustomClientConnection;
    FLastError: string;
    FStatusProcessamento: TRpDataFlashProcessingStatus;
    FEventoStatus : TRpDataFlashOnStatus;
    FSharedClient : Boolean;
    FBusyCallback : TRpDataFlashBusyCallback;
    procedure ProcessaErroComunicacao(const pMessage : string);
    procedure DoAoProcessarErroComunicacao; virtual;
    procedure DoComunicar(const ACommand : TRpDataFlashSendCommand);
    function DoEnviar(const ANomeComando: string; const AParams: TRpDataFlashCommandParameterList) : Boolean;
  public
    constructor Create(ATcpClient: TRpDataFlashCustomClientConnection;
      ABusyCallback : TRpDataFlashBusyCallback; const ASharedClient : Boolean = False);
    procedure SetEvents(const AEventoStatus : TRpDataFlashOnStatus);
    function GetLastError : string;
    function GetStatusProcessamento : TRpDataFlashProcessingStatus;
    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat;
  end;

implementation

uses
  uRpDataFlash.Ping,
  uRpDataFlash.AuthCommand,
  uRpDataFlash.ProxyGenerator,
  uRpDataFlash.DataSetProvider,
  uRpDataFlash.ComandosControllerView,
  Types,
  WinSock,
  Forms,
  StrUtils;

{ TRpDataFlashCustomConnection }

procedure TRpDataFlashCustomConnection.LoadConfig(const ANode: IXMLNode);
begin
  FServer := ANode['Servidor'];
  FConfigTCPIP.Port := ANode['Porta'];
  FConfigREST.Port := ANode['PortaRest'];

  DoLoadConfig(ANode);
end;

procedure TRpDataFlashCustomConnection.LoadConfig(const AArquivo: string);
var
  lArquivo: TXMLDocument;
  lNodeConfig: IXMLNode;
begin
  if FileExists(AArquivo) then
  begin
    lArquivo := TXMLDocument.Create(AArquivo);
    lNodeConfig := lArquivo.DocumentElement.ChildNodes.FindNode(Self.ClassName);
    LoadConfig(lNodeConfig);
  end;
end;

procedure TRpDataFlashCustomConnection.CompressStream(var AEntrada: TMemoryStream);
var
  SourceStream : TCompressionStream;
  DestStream : TMemoryStream;
begin
  DestStream := TMemoryStream.Create;
  try
    SourceStream := TCompressionStream.Create(clDefault, DestStream);
    try
      AEntrada.Position := 0;
      AEntrada.SaveToStream(SourceStream);
    finally
      FreeAndNil(SourceStream);
    end;

    DestStream.Position := 0;

    FreeAndNil(AEntrada);
    AEntrada := TMemoryStream.Create;

    AEntrada.CopyFrom(DestStream, DestStream.Size);
    AEntrada.Position := 0;
  finally
    FreeAndNil(DestStream);
  end;
end;

procedure TRpDataFlashCustomConnection.Connect;
begin
  DoConnect;
end;

constructor TRpDataFlashCustomConnection.Create(AOwner: TComponent);
begin
  inherited;
  FFileTransferList := TStringList.Create;

  FConfigTCPIP := TRpDataFlashConfigConexaoTCPIP.Create;
  FConfigREST  := TRpDataFlashConfigConexaoREST.Create;

  FQuebrasProtocolosRecebidos := TRpDataFlashProtocolBreakerList.Create;
  FIpsReconhecidos := TStringList.Create;

  FFileTransfer := TRpDataFlashFileTransfer.Create;

  Inicializar;
end;

function TRpDataFlashCustomConnection.CreateException(const AException: Exception;
  const AResultType : TSerializationFormat): string;
var
  lXmlException: IXMLDocument;
  lRoot: IXMLNode;
  lExceptionNode: IXMLNode;
begin
  if AResultType = sfJSON then
  begin
    Result := Format('{"Exception": {"Classe": "%s","Message": "%s"}}', [
      AException.ClassName,
      AException.Message ]);
  end
  else
  begin
    lXmlException := TXMLDocument.Create(nil);
    lXmlException.Active := True;
    {$IFDEF UNICODE}
    lXmlException.Encoding := 'UTF-16';
    {$ELSE}
    lXmlException.Encoding := 'iso-8859-1';
    {$ENDIF}
    lXmlException.Version := '1.0';
    lXmlException.StandAlone := 'yes';
    lRoot := lXmlException.AddChild('root');
    lRoot.Attributes['Type'] := 'Exception';
    lExceptionNode := lRoot.AddChild('Exception');

    lExceptionNode['Classe'] := AException.ClassName;
    lExceptionNode['Message'] := AException.Message;

    Result := lXmlException.XML.Text;
    {$IFDEF UNICODE}
    Result := StringReplace(Result, 'UTF-16', 'iso-8859-1', []);
    {$ENDIF}
  end;
end;

procedure TRpDataFlashCustomConnection.DecompressStream(var AEntrada: TMemoryStream);
const
  BufferSize = 4096;
var
  OriginalStream : TDecompressionStream;
  DestStream : TMemoryStream;
  Buffer: array[0..BufferSize-1] of Byte;
  Size : Int64;
begin
  AEntrada.Position := 0;
  DestStream := TMemoryStream.Create;
  try
    OriginalStream := TDecompressionStream.Create(AEntrada);
    try
      Size := 1;
      while Size <> 0 do
      begin
        Size := OriginalStream.Read(Buffer, BufferSize);
        DestStream.Write(Buffer, Size);
      end;
    finally
      FreeAndNil(OriginalStream);
    end;
    FreeAndNil(AEntrada);
    AEntrada := TMemoryStream.Create;
    AEntrada.CopyFrom(DestStream, 0);
    AEntrada.Position := 0;
  finally
    FreeAndNil(DestStream);
  end;
end;

procedure TRpDataFlashCustomConnection.Disconnect;
begin
  DoDisconnect;
  RemoverArquivosTemporarios;
end;

destructor TRpDataFlashCustomConnection.Destroy;
begin
//  Desconectar;
  Finalizar;
  FreeAndNil(FIpsReconhecidos);
  FreeAndNil(FQuebrasProtocolosRecebidos);
  FreeAndNil(FConfigTCPIP);
  FreeAndNil(FConfigREST);
  FreeAndNil(FFileTransfer);
  FreeAndNil(FFileTransferList);
  inherited;
end;

procedure TRpDataFlashCustomConnection.DoLoadConfig(const ANode: IXMLNode);
begin

end;

procedure TRpDataFlashCustomConnection.DoInternalEnviar(const AHandler: TIdIOHandler;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo; const AValor: string;
  const AContext : TIdContext; const AArquivo : string);
var
  lProtocolo: TRpDataFlashProtocol;
  lResposta: string;
  lIsXML: Boolean;

  function IsError : Boolean;
  const
    C_ERROR_LOAD_JSON = '{"Exception": {"Classe": ""';
    C_ERROR_EXEC_JSON = '"exec_Exception":{"Tipo":2,"TipoValor":1,"BaseClass":"","Valor":""}';
    C_ERROR_EXEC_APP_JSON = '"exec_Exception":""';
    C_ERROR_LOAD_XML = '<Exception><Classe></Classe>';
    C_ERROR_EXEC_XML = '<exec_Exception Tipo="2" TipoValor="1"></exec_Exception>';
  begin
    // 2 error types, before = onLoadCmd (Exception) and after = OnExecCmd (exec_Exception)
    if Pos(C_PARAM_INT_EXCEPTION, lResposta) > 0 then
    begin
      if lIsXML then
        Result := Pos(C_ERROR_EXEC_XML, lResposta) = 0
      else
        Result := (Pos(C_ERROR_EXEC_JSON, lResposta) = 0)
              and (Pos(C_ERROR_EXEC_APP_JSON, lResposta) = 0)
    end else
    begin
      if lIsXML then
        Result := Pos(C_ERROR_LOAD_XML, lResposta) = 0
      else
        Result := Pos(C_ERROR_LOAD_JSON, lResposta) = 0
    end;
  end;

begin
  if ARequestInfo = nil then
    InternalEnviar(AHandler, AValor)
  else
  begin
    if AArquivo <> '' then
    begin
      if FileExists(AArquivo) then
      begin
        AResponseInfo.ContentType := 'application/octet-stream';
        AResponseInfo.ServeFile(AContext, AArquivo);
        AResponseInfo.ResponseNo := 200;
      end else
      begin
        AResponseInfo.ResponseText := 'Arquivo ' + ExtractFileName(AArquivo) + ' não encontrado !';
        AResponseInfo.ContentText := AResponseInfo.ResponseText;
        AResponseInfo.ResponseNo := 400;
      end;
    end else
    begin
      if ARequestInfo.ContentType = C_REST_CONTENT_TYPE_DELPHI then
        AResponseInfo.ContentText := AValor
      else
      begin
        // neste caso, deve mandar sem a criptografia
        lProtocolo := TRpDataFlashProtocol.Create(EncryptionType);
        try
          lProtocolo.Message := AValor;
          lResposta := lProtocolo.Dismount(AValor);

          if lResposta = EmptyStr then
            lResposta := ' ';

          if AResponseInfo.ContentEncoding = EmptyStr then
            AResponseInfo.ContentEncoding := 'iso-8859-1';

          lIsXML := (lResposta[1] = '<')
                and (ARequestInfo.ContentType <> C_REST_CONTENT_TYPE_JSON);

          if lIsXML then
          begin
            if Pos('<?xml ', lResposta) <= 0 then
            begin
              lResposta := '<?xml version="1.0" encoding="iso-8859-1"?>'
                         + lResposta;
            end;
          end
          else
          begin
            AResponseInfo.ContentType := 'application/json';
            AResponseInfo.CustomHeaders.Add('Access-Control-Allow-Origin: *');
          end;

          // configura o código de retorno
          if IsError then
            AResponseInfo.ResponseNo := 400
          else
            AResponseInfo.ResponseNo := 200;
          AResponseInfo.ContentText := lResposta;
        finally
          FreeAndNil( lProtocolo );
        end;
      end;
    end;
  end;
end;

procedure TRpDataFlashCustomConnection.DoSaveConfig(const ANode: IXMLNode);
begin

end;

function TRpDataFlashCustomConnection.FileTransfer_RegisterFile(const AFileID, AFileName: string): Boolean;
begin
  FFileTransferList.Add(AFileID + '=' + AFileName);
  RemoverArquivosTemporarios;
  Result := True;
end;

procedure TRpDataFlashCustomConnection.Finalizar;
begin
  RemoverArquivosTemporarios;
end;

function TRpDataFlashCustomConnection.TentaGerarException(const AMenssage: string; out AException : string) : Boolean;
var
  lException: IXMLDocument;
  lRoot: IXMLNode;
  lExceptionNode: IXMLNode;
  lStream: TStringStream;
  lClasse: string;
  lMessage: string;
  lJson: TJSONObject;
  lPair: TJSONPair;
begin
  AException := EmptyStr;

  Result := Pos('Exception', AMenssage) > 0;

  if Result then
  begin
    // xml
    if AMenssage[1] = '<' then
    begin
      lMessage := AMenssage;
      if Pos('<?xml version="', lMessage) <= 0 then
        lMessage := '<?xml version="1.0" encoding="iso-8859-1"?>' + sLineBreak
                  + lMessage;

      lStream := TStringStream.Create(lMessage);
      try
        lException := TXMLDocument.Create(nil);
        lException.LoadFromStream(lStream);
        lRoot := lException.ChildNodes.FindNode('root');
        Result := lRoot <> nil;

        if Result then
        begin
          lExceptionNode := lRoot.ChildNodes.FindNode('Exception');

          Result := lExceptionNode <> nil;
          if Result then
          begin
            lClasse := lExceptionNode['Classe'];
            lMessage := lExceptionNode['Message'];

            AException := 'ServerError: ' + lMessage + sLineBreak +
              'Exception : ' + lClasse;
          end;
        end;
      finally
        lStream.Position := 0;
        FreeAndNil(lStream);
      end;
    end
    else // json
    begin
      lJson := TJSONObject.Create;
      try
        lJson.Parse(BytesOf(AMenssage), 0);
        lPair := lJson.Get('Exception');
        Result := Assigned(lPair);
        if Result then
        begin
          lClasse  := StringReplace(TJSONObject(lPair.JsonValue).Get('Classe').FieldValue, '"', '', [rfReplaceAll]);
          lMessage := StringReplace(TJSONObject(lPair.JsonValue).Get('Message').FieldValue, '"', '', [rfReplaceAll]);

          AException := lMessage + sLineBreak + 'Exception: ' + lClasse;
        end;
      finally
        FreeAndNil(lJson);
      end;
    end;
  end;
end;

procedure TRpDataFlashCustomConnection.GerarStatus(const ASituacao: TRpDataFlashStatusType;
  const AProcessamentoTotal, AProcessamentoAtual: Integer; const AStatusMens : string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(Self, ASituacao, AProcessamentoTotal, AProcessamentoAtual, AStatusMens);
end;

function TRpDataFlashCustomConnection.GetConnected: Boolean;
begin
  Result := False;
end;

function TRpDataFlashCustomConnection.GetFile(const AFileID: string;
  AArquivo: IRpFileProxy): Boolean;
begin
  Result := False;
end;

function TRpDataFlashCustomConnection.GetFileTransfer_Port: Integer;
begin
  Result := FFileTransfer.Port;
end;

function TRpDataFlashCustomConnection.GetFileTransfer_TempDir: string;
begin
  Result := FFileTransfer.TempDir;
end;

function TRpDataFlashCustomConnection.GetIdentifier: string;
begin
  Result := EmptyStr;
end;

function TRpDataFlashCustomConnection.GetLocalComputerName: string;
begin
  Result := TRpDataFlashUtils.GetLocalComputerName;
end;

procedure TRpDataFlashCustomConnection.Inicializar;
begin
  FCommunicationType := ctText;
  FEncryptionType := tcBase64Compressed;
end;

procedure TRpDataFlashCustomConnection.InternalEnviar(const AHandler: TIdIOHandler;
  const AValor: string);

  procedure EnviarComoTexto;
  var
    lQuebra: TRpDataFlashProtocolBreaker;
    lContinue: Boolean;
    lLinha: string;
    lErro: Boolean;
    lProcessamentoTotal: Integer;
    lProcessamentoAtual: Integer;
  begin
    AHandler.LargeStream := False;
    lQuebra := TRpDataFlashProtocolBreaker.Create;
    try
      lProcessamentoTotal := lQuebra.BreaksCalcCount(AValor);
      lProcessamentoAtual := lProcessamentoTotal;

      lQuebra.OnStatus := FOnStatus;
      lQuebra.AddValue(AValor);

      lContinue := True;
      lErro := False;
      while lQuebra.Next do
      begin
        lLinha := lQuebra.Current;

        lErro := lErro
              or (Pos(TRpDataFlashProtocol.GetErrorTag, lLinha) > 0);

        if Assigned(FOnTimeOutCheck) and (not lErro) then
        begin
          FOnTimeOutCheck(Self, voSending, lContinue);
          if not lContinue then
            raise Exception.Create('Erro de comunicação (E). Tempo limite atingido.');
        end;

        AHandler.WriteLn(lLinha);

        GerarStatus(stSendingData, lProcessamentoTotal * 2, lProcessamentoAtual, 'Enviando informação...');
        Inc(lProcessamentoAtual);
      end;
    finally
      FreeAndNil(lQuebra);
    end;
  end;

  procedure EnviarStream(AStream : TStream);
  begin
//    GerarStatus(0, 100, 1);
    AStream.Position := 0;
    AHandler.LargeStream := True;
    AHandler.Write(AStream, 0, True);
//    GerarStatus(0, 100, 100);
  end;

  procedure EnviarComoStringStream;
  var
    lStream: TStringStream;
  begin
    lStream := TStringStream.Create(AValor);
    try
      EnviarStream(lStream);
    finally
      lStream.Free;
    end;
  end;

  procedure EnviarComoCompressedStream;
  var
    lTexto: TStringList;
    lStream: TMemoryStream;
  begin
    lTexto := TStringList.Create;
    lStream := TMemoryStream.Create;
    try
      GerarStatus(stPreparingData, 100, 20, 'Enviando Informação...');
      lTexto.Text := AValor;
      lTexto.SaveToStream(lStream);
      GerarStatus(stPreparingData, 100, 50, 'Enviando Informação...');

      CompressStream(lStream);
      GerarStatus(stSendingData, 100, 80, 'Enviando Informação...');
      EnviarStream(lStream);
      GerarStatus(stSendingData, 100, 100, 'Enviando Informação...');
    finally
      FreeAndNil(lTexto);
      lStream.Free;
    end;
  end;

  procedure EnviarComoChar;
  begin
//    GerarStatus(0, 100, 1);
    AHandler.LargeStream := False;
    AHandler.WriteLn(AValor);
//    GerarStatus(0, 100, 100);
  end;

begin
  NewLog(slOnSend, AValor, EmptyStr);

  case FCommunicationType of
    ctText             : EnviarComoTexto;
    ctStream           : EnviarComoStringStream;
    ctCompressedStream : EnviarComoCompressedStream;
    ctChar             : EnviarComoChar;
  end;
end;

function TRpDataFlashCustomConnection.InternalReceber(
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo): string;
var
  lQuebra: TRpDataFlashProtocolBreaker;
  lLinha: string;
  lNomeClasseComando: string;
  lComando: IRpDataFlashCommandInterfaced;
  lLifeCycle: TRpDataFlashLifeCycle;
  i: Integer;
  lComandos : TStringList;

  function CarregarNomeComando(const ANomeClasseComando : string) : Boolean;
  begin
    Result := TRpDataFlashCommand.LoadCommand(ANomeClasseComando, lComando, lLifeCycle, nil, nil);
  end;

  function TentaCarregarComando : Boolean;
  var
    lInternalComando: string;
    lIndexComando: Integer;
  begin
    Result := False;
    lInternalComando := ARequestInfo.Document;
    if lInternalComando[1] = '/' then
      Delete(lInternalComando, 1, 1);

    lComandos.Clear;
    lComandos.Delimiter := '/';
    lComandos.DelimitedText := lInternalComando;

//    lDecode := TIdURI.URLDecode( ARequestInfo.Document );
//    lComandos.Add(lDecode);
    lInternalComando := '';
    for lIndexComando := 0 to lComandos.Count - 1 do
    begin
      lInternalComando := lInternalComando + lComandos[lIndexComando];
      if lInternalComando[1] = '/' then
        Delete(lInternalComando, 1, 1);
      if CarregarNomeComando(lInternalComando) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;

  procedure LoadPostJsonFromBody;
  var
    lStream: TStringStream;
    lValue : string;
  begin
    lStream := TStringStream.Create;
    try
      if Assigned(ARequestInfo.PostStream) then
      begin
        ARequestInfo.PostStream.Position := 0;
        lStream.LoadFromStream( ARequestInfo.PostStream );
        lValue := Trim(lStream.DataString);
        if lValue <> EmptyStr then
          lComando.GetParams.ParseJSON(lValue);
      end;
    finally
      if Assigned(lStream) then
        FreeAndNil(lStream);
    end;
  end;

begin
// {$MESSAGE 'REST InputReader'}
  lComandos := TStringList.Create;
  lQuebra := TRpDataFlashProtocolBreaker.Create;
  try
    // quando vem do componente ja esta correto
    if ARequestInfo.ContentType = C_REST_CONTENT_TYPE_DELPHI then
      lLinha := ARequestInfo.UnparsedParams
    else
      lLinha := TRpURLConverter.DecodeURL(ARequestInfo.UnparsedParams);

    // testa se é comando vindo de componente ou de outra fonte (browser, php, java...)
    if ARequestInfo.ContentType = C_REST_CONTENT_TYPE_DELPHI then
      Result := lLinha
    else
    begin
      lNomeClasseComando := ARequestInfo.Document;
      // remove a barra inicial
      Delete(lNomeClasseComando, 1, 1);

      if lNomeClasseComando = '' then
        raise ERpDataFlashException.Create('Um comando deve ser passado. Para obter a lista de comandos utilizar /PublicList');

      //if TRpDataFlashComando.CarregarComando(lNomeClasseComando, lComando, lLifeCycle, nil, nil) then
      if TentaCarregarComando then
      begin
        // verifica se algum parametro de entrada passado pelo REST nao existe
        for i := 0 to ARequestInfo.Params.Count - 1 do
          lComando.GetParams.Param[ ARequestInfo.Params.Names[i] ];

        // carrega os parametros do REST para o componente conseguir executar
        for i := 0 to lComando.GetParams.Count - 1 do
        begin
          if (lComando.GetParams.Item[i].ParamType in [tpInput, tpInputNoReload])
          or ((lComando.GetParams.Item[i].ParamType = tpInternal) and (lComando.GetParams.Item[i].Name = C_PARAM_INT_FORMAT_TYPE)) then
            lComando.GetParams.Item[i].AsVariant := ARequestInfo.Params.Values[lComando.GetParams.Item[i].Name];
        end;

        if ARequestInfo.Command = 'POST' then
          LoadPostJsonFromBody;

        if ARequestInfo.ContentType = C_REST_CONTENT_TYPE_JSON then
          lComando.GetParams.SerializationFormat := sfJSON;

        Result := lComando.GetParams.Serialize;
      end
      else
        raise ERpDataFlashException.CreateFmt('Comando não suportado: Comando [%s].', [lNomeClasseComando]);
    end;
  finally
    FreeAndNil(lComandos);
    FreeAndNil(lQuebra);
  end;
end;

function TRpDataFlashCustomConnection.InternalReceber(const AHandler: TIdIOHandler): string;

  procedure ReceberComoTexto;
  var
    lQuebra: TRpDataFlashProtocolBreaker;
    lContinue : Boolean;
    lLinha: string;
    lErro: Boolean;
    lProcessamentoTotal: Integer;
    lProcessamentoAtual: Integer;
  begin
    lQuebra := TRpDataFlashProtocolBreaker.Create;
    try
      lProcessamentoTotal := 0;
      lProcessamentoAtual := 0;

      lContinue := True;
      lErro := False;
      while not lQuebra.Valid do
      begin
        lLinha := AHandler.ReadLn;
        lQuebra.AddValue(lLinha);

        lErro := lErro
              or (Pos(TRpDataFlashProtocol.GetErrorTag, lLinha) > 0);

        if Assigned(FOnTimeOutCheck) and (not lErro) then
        begin
          FOnTimeOutCheck(Self, voReceiving, lContinue);
          if not lContinue then
            raise Exception.Create('Erro de comunicação (R). Tempo limite atingido.');
        end;

        if lProcessamentoTotal = 0 then
          lProcessamentoTotal := lQuebra.BreakCountPreview;

        Inc(lProcessamentoAtual);
        GerarStatus(stReceivingData, lProcessamentoTotal, lProcessamentoAtual, 'Recebendo informações...');
      end;
      Result := lQuebra.Value;
    finally
      FreeAndNil(lQuebra);
    end;
  end;

  procedure ReceberStream(AStream : TStream);
  begin
    AHandler.LargeStream := True;
    AHandler.ReadStream(AStream);
    AStream.Position := 0;
  end;

  procedure ReceberComoStringStream;
  var
    lStream: TStringStream;
  begin
    lStream := TStringStream.Create( EmptyStr );
    try
      ReceberStream(lStream);
      Result :=  lStream.DataString;
    finally
      lStream.Free;
    end;
  end;

  procedure ReceberComoCompressedStream;
  var
    lTexto: TStringList;
    lStream: TMemoryStream;
  begin
    lTexto := TStringList.Create;
    lStream := TMemoryStream.Create;
    try
      ReceberStream(lStream);
      DecompressStream(lStream);
      lTexto.LoadFromStream(lStream);
      Result := lTexto.Text;
    finally
      FreeAndNil(lTexto);
      lStream.Free;
    end;
  end;

  procedure ReceberComoChar;
  begin
    Result := '';
    while not AHandler.InputBufferIsEmpty do
      Result := Result + AHandler.ReadChar;
  end;

begin
// {$MESSAGE 'TCP/IP InputReader'}
  case FCommunicationType of
    ctText              : ReceberComoTexto;
    ctStream            : ReceberComoStringStream;
    ctCompressedStream  : ReceberComoCompressedStream;
    ctChar              : ReceberComoChar;
  end;
  NewLog(slOnReceive, Result, EmptyStr);
end;

function TRpDataFlashCustomConnection.isComandoDesejado(const pComando,
  pMensagem: string): Boolean;

  function DoExpectedCmd : Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i := Low(C_VERIFIED_COMMAND_CONTROLLER) to High(C_VERIFIED_COMMAND_CONTROLLER) do
      if (pComando = C_VERIFIED_COMMAND_CONTROLLER[i]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  if DoExpectedCmd then
    Result := Copy(pMensagem, 1, Length(pComando)) = pComando
  else
    Result := False;
end;

procedure TRpDataFlashCustomConnection.Clear;
begin
end;

procedure TRpDataFlashCustomConnection.NewException(const AException: Exception; out AExpandir : Boolean);
begin
  AExpandir := True;
  if Assigned(FOnException) then
    FOnException(Self, AException, AExpandir);
end;

procedure TRpDataFlashCustomConnection.NewException(const AException: Exception);
var
  lExpandir: Boolean;
begin
  lExpandir := True;
  if Assigned(FOnException) then
    FOnException(Self, AException, lExpandir);
end;

procedure TRpDataFlashCustomConnection.NewLog(const ALogType: TRpDataFlashServiceLog;
  const ALog: string; const AContext: TIdContext);
var
  lIP: string;
begin
  lIP := EmptyStr;
  if AContext <> nil then
    lIP := AContext.Binding.PeerIP;
  NewLog(ALogType, ALog, lIP);
end;

procedure TRpDataFlashCustomConnection.NewLog(const ALogType: TRpDataFlashServiceLog;
  const ALog: string; const AIP : string);
var
  lLog: TRpDataFlashSyncLogEvent;

  function GetNomeHost(const AIP : string) : string;
  var
    SockAddrIn: TSockAddrIn;
    HostEnt: PHostEnt;
    WSAData: TWSAData;
  begin
    WSAStartup($101, WSAData);
    {$IFDEF UNICODE}
    SockAddrIn.sin_addr.s_addr := inet_addr(PAnsiChar( AnsiString(AIP) ));
    {$ELSE}
    SockAddrIn.sin_addr.s_addr := inet_addr(PChar( AIP ));
    {$ENDIF}
    HostEnt := gethostbyaddr(@SockAddrIn.sin_addr.S_addr, 4, AF_INET);
    if HostEnt <> nil then
      {$IFDEF UNICODE}
        Result := String(StrPas(Hostent^.h_name))
      {$ELSE}
        Result := StrPas(Hostent^.h_name)
      {$ENDIF}
    else
      Result := '<' + AIP + '>';
    FIpsReconhecidos.Add(AIP + '=' + Result);
  end;

begin
  if Assigned(FOnNewLog) then
  begin
    lLog := TRpDataFlashSyncLogEvent.Create;
    try
      lLog.FLRDataFlashIP  := Self;
      lLog.FEvento    := FOnNewLog;
      lLog.FTipoLog   := ALogType;
      lLog.FLog       := ALog;
      lLog.FClientInfo.Initialize;
      lLog.FClientInfo.GUIDComando := EmptyStr; //IfThen(pGUID = '', 'GUID DESCONHECIDO', pGUID);
      lLog.FClientInfo.IP := AIP;
      lLog.FClientInfo.PeerIP := AIP;
        //localiza o IP na tabela de conhecidos
      lLog.FClientInfo.DisplayName := FIpsReconhecidos.Values[lLog.FClientInfo.PeerIP];
        // se nao localizou busca na rede
      if lLog.FClientInfo.DisplayName = EmptyStr then
        lLog.FClientInfo.DisplayName := GetNomeHost( lLog.FClientInfo.PeerIP );
      lLog.DoNotify;
    finally
      FreeAndNil(lLog);
    end;
  end;
end;

procedure TRpDataFlashCustomConnection.OnFileTransferLog(const ALogMessage: string);
begin
  NewLog(slOnStatus, ALogMessage, nil);
end;

function TRpDataFlashCustomConnection.PutFile(const AArquivo: IRpFileProxy): Boolean;
begin
  Result := False;
end;

procedure TRpDataFlashCustomConnection.RemoverArquivosTemporarios;
var
  lRec: TSearchRec;
  lFileAge: TDateTime;
  lFileName: string;
begin
  // remove arquivos registrados com mais de 1h
  if FindFirst(FFileTransfer.TempDir + '*.*', faAnyFile, lRec) = 0 then
  begin
    repeat
      lFileName := FileTransfer.TempDir + lRec.Name;
      if FileExists(lFileName) then
      begin
        FileAge(lFileName, lFileAge{$IFDEF UNICODE}, False {$ENDIF});
        if MinutesBetween(Now, lFileAge) > 10 then
          DeleteFile(PChar(lFileName));
      end;
    until FindNext(lRec) <> 0;
  end;
end;

procedure TRpDataFlashCustomConnection.SaveConfig(const ANode: IXMLNode);
begin
  ANode['Servidor'] := FServer;
  ANode['Porta'] := FConfigTCPIP.Port;
  ANode['PortaRest'] := FConfigREST.Port;
  DoSaveConfig(ANode);
end;

procedure TRpDataFlashCustomConnection.SaveConfig(const AArquivo: string);
var
  lArquivo: TXMLDocument;
  lNodeRoot: IXMLNode;
begin
  lArquivo := TXMLDocument.Create(nil);
  lArquivo.Active := True;
  lNodeRoot := lArquivo.AddChild('root');
  SaveConfig(lNodeRoot.AddChild(Self.ClassName));
  lArquivo.SaveToFile(AArquivo);
end;

procedure TRpDataFlashCustomConnection.SeparaComando(const pComandoCompleto: string;
  out ACommand, AGuid: string);
var
  lLen : Integer;
  lFinal: string;
  i: Integer;
begin
  lLen := -1;
  for i := Low(C_VERIFIED_COMMAND_CONTROLLER) to High(C_VERIFIED_COMMAND_CONTROLLER) do
    if isComandoDesejado(C_VERIFIED_COMMAND_CONTROLLER[i], pComandoCompleto) then
    begin
      lLen := Length(C_VERIFIED_COMMAND_CONTROLLER[i]);
      Break;
    end;

  if lLen > -1 then
  begin
    ACommand := Copy(pComandoCompleto, 1, lLen);
    AGuid := Copy(pComandoCompleto, lLen + 1, 38);
  end
  else
  begin
    // verifica o final do comando (mensagens genéricas)
    lFinal := Copy(pComandoCompleto, Length(pComandoCompleto) - 37, 38);
    if (Length(lFinal) = 38) and (lFinal[1] = '{') and (lFinal[38] = '}') then
    begin
      AGuid := lFinal;
      ACommand := Copy(pComandoCompleto, 1, Length(pComandoCompleto) - 38);
    end;
  end;
end;

procedure TRpDataFlashCustomConnection.SetServidor(const Value: string);
begin
  FServer := Value;
//  if Pos(':', FServidor) > 0 then
//    Delete(FServidor, Pos(':', FServidor) - 1, Length(FServidor));
end;

procedure TRpDataFlashCustomConnection.SetEncryptionType(const Value: TRpDataFlashEncryptionType);
begin
  FEncryptionType := Value;
end;

procedure TRpDataFlashCustomConnection.TentaGerarException(const AProtocol : TRpDataFlashProtocol);
var
  lException: string;
begin
  if (AProtocol.IsError) and (TentaGerarException(AProtocol.Message, lException)) then
    raise ERpDataFlashException.Create(lException);
end;

{ TRpDataFlashServerConnection }

function TRpDataFlashServerConnection.CarregarComandoViaControllers(
  const ACommand : string; out AObjComando : IRpDataFlashCommandInterfaced;
  out AParametros : TRpDataFlashCommandParameterList) : Boolean;
begin
  AParametros := TRpDataFlashCommandParameterList.Create(Self);
  AParametros.Load(ACommand);

  AObjComando := nil;

  if FControllers <> nil then
  begin
    if FControllers.Localizar(AParametros.Command, AObjComando) then
      AObjComando.DoLoad(loSend, AParametros);
  end;

  Result := AObjComando <> nil;
end;

function TRpDataFlashServerConnection.CarregarComandoViaProviders(
  const ACommand: string; out AObjComando: IRpDataFlashCommandInterfaced;
  out AParametros: TRpDataFlashCommandParameterList): Boolean;
begin
  AParametros := TRpDataFlashCommandParameterList.Create(Self);
  AParametros.Load(ACommand);

  AObjComando := nil;

  if FProviders <> nil then
  begin
    if FProviders.Localizar(AParametros.Command, AObjComando) then
      AObjComando.DoLoad(loSend, AParametros);
  end;

  Result := AObjComando <> nil;
end;

function TRpDataFlashServerConnection.CarregarConexaoCliente(
  const AConnectionItem : TRpDataFlashConnectionItem;
  out AConexao: TRpDataFlashCustomClientConnection) : Boolean;
begin
  AConexao := nil;
  Result := False;

  if IsPonte then
  begin
    AConexao := AConnectionItem.TcpClientPonte;
    try
      if AConexao <> nil then
      begin
        AConexao.Connect;
        Result := AConexao.Connected;
      end;
    except
      on E:Exception do
      begin
        OutputDebugString(PChar('Erro conectando ponte. ' + E.Message));
      end;
    end;
  end;
end;

function TRpDataFlashServerConnection.CarregarStatusProcessamento(
  const AConnectionItem: TRpDataFlashConnectionItem;
  out AConexao: TRpDataFlashCustomClientConnection): TRpDataFlashProcessingStatus;
begin
  AConexao := nil;
  Result := psServer;
  if IsPonte then
  begin
   if CarregarConexaoCliente(AConnectionItem, AConexao) then
     Result := psBridgeOnline
   else
     Result := psBridgeOffLine;
  end;
end;

function TRpDataFlashServerConnection.ComandoRequerAutenticacao(const AClasseComando: string;
  const AParametros : TRpDataFlashCommandParameterList): Boolean;
var
  lIgnorados: Boolean;

  function ComandoNaLista : Boolean;
  var
    lUpperCmd: string;
  begin
    lUpperCmd := ';' + UpperCase(AClasseComando) + ';';
    Result := Pos(lUpperCmd, UpperCase(FWithoutAuthCommands)) > 0;
  end;

  function ParametroEmDesigning : Boolean;
  begin
    Result := Assigned( AParametros.FindByName('csDesigning', tpInput) )
          and AParametros.Param['csDesigning'].AsBoolean;
  end;

begin
  // commands ignored by auth
  lIgnorados := TRpDataFlashComandoAutenticar.ClassNameIs( AClasseComando )
             or TRpDataFlashPingCommand.ClassNameIs( AClasseComando )
             or TRpDataFlashComandoList.ClassNameIs( AClasseComando )
             or ComandoNaLista
             or ParametroEmDesigning;
//             or TRpDataFlashComandoGetProviderList.ClassNameIs( AClasseComando )

//csDesigning

  Result := not lIgnorados;
end;

function TRpDataFlashServerConnection.Conector: TIdTCPServer;
begin
  Result := FConectorTCP;
end;

procedure TRpDataFlashServerConnection.DoConnect;
begin
  inherited;
  try
    FNumeroConectados := 0;
    if FConectorTCP <> nil then
      FreeAndNil(FConectorTCP);

    if FConectorREST <> nil then
      FreeAndNil(FConectorREST);

    if FConfigTCPIP.Enabled then
    begin
      FConectorTCP := TIdTCPServer.Create(Self);
      if FConfigTCPIP.Port = 0 then
        raise ERpDataFlashConnectionError.Create('Porta TCP não informada!');

      Conector.OnExecute    := AoExecutarNoServidor;
      Conector.OnConnect    := AoConectarCliente;
      Conector.OnDisconnect := AoDesconectarCliente;

      Conector.DefaultPort  := FConfigTCPIP.Port;
      if not Conector.Active then
        Conector.Active := True;

      if not Conector.Active then
        raise ERpDataFlashConnectionError.Create('Não foi possível iniciar a conexão do servidor TCP!');
    end;

    if FConfigREST.Enabled then
    begin
      FConectorREST := TIdHTTPServer.Create(Self);
      if FConfigREST.Port = 0 then
        raise ERpDataFlashConnectionError.Create('Porta REST não informada!');

      FConectorREST.OnConnect := AoConectarCliente;
      FConectorREST.OnDisconnect := AoDesconectarCliente;
      FConectorREST.OnCommandGet := AoExecutarNoServidorRest;
      FConectorREST.OnCommandOther := AoExecutarNoServidorRest;

      FConectorREST.DefaultPort  := FConfigREST.Port;
      if not FConectorREST.Active then
        FConectorREST.Active := True;

      if not FConectorREST.Active then
        raise ERpDataFlashConnectionError.Create('Não foi possível iniciar a conexão do servidor TCP!');
    end;

    if (FFileTransfer.Port > 0) and FFileTransfer.Enabled then
    begin
      if FConectorFTP <> nil then
      begin
        if FConectorFTP.Active then
          FConectorFTP.Active := False;
        FreeAndNil(FConectorFTP);
      end;
      FConectorFTP := TIdFTPServer.Create(Self);
      FConectorFTP.DirFormat := ftpdfOSDependent;
      FConectorFTP.DefaultPort := FFileTransfer.Port;
      FConectorFTP.DefaultDataPort := FFileTransfer.DataPort;
      FConectorFTP.FTPSecurityOptions.RequirePASVFromSameIP := False;
      FConectorFTP.FTPSecurityOptions.RequirePORTFromSameIP := False;

      FConectorFTP.OnAfterUserLogin := OnFTPServerAfterUserLogin;
      FConectorFTP.OnUserLogin := OnFTPServerUserLogin;
      FConectorFTP.OnRetrieveFile := OnFTPServerRetrieveFile;
      FConectorFTP.OnException := OnFTPServerException;
      FConectorFTP.OnStatus := OnFTPServerStatus;
      FConectorFTP.OnDeleteFile := OnFTPServerDeleteFile;
      FConectorFTP.OnStoreFile := OnFTPServerStoreFile;

      FConectorFTP.Active := True;
    end;
  except
    on E:Exception do
    begin
      NewException(E);
      raise;
    end;
  end;
  RemoverArquivosTemporarios;
end;

procedure TRpDataFlashServerConnection.DoDisconnect;
begin
  FDesconectandoServer := True;

  try
    if (FConfigTCPIP.Enabled) and (FConectorTCP <> nil) then
    begin
      if FConectorTCP.Active then
        FConectorTCP.Active := False;
      FreeAndNil(FConectorTCP);
    end;
  except
    on E:Exception do
  end;

  try
    if (FConfigREST.Enabled) and (FConectorREST <> nil) then
    begin
      if FConectorREST.Active then
        FConectorREST.Active := False;
      FreeAndNil(FConectorREST);
    end;
  except
    on E:Exception do
  end;

  try
    if Assigned(FConectorFTP) and FConectorFTP.Active then
      FConectorFTP.Active := False;
    FreeAndNil(FConectorFTP);
  except
    on E:Exception do
  end;

  Clear;
  FDesconectandoServer := False;
end;

//procedure TRpDataFlashServerConnection.DoFreeExecutor(AExecutor: IExecutorComandoPacote);
//begin
//  AExecutor.DisconnectDataComponent;
//  AExecutor := nil;
//end;

function TRpDataFlashServerConnection.EnviarCallBack(const AContext: TIdContext; const AMenssage: string) : Boolean;
var
  lProtocolo: TRpDataFlashProtocol;
  lResposta: string;
begin
  lProtocolo := TRpDataFlashProtocol.Create(EncryptionType);
  try
    lProtocolo.Identifier := TAG_CALLBACK;
    lProtocolo.Message := AMenssage;
    lResposta := lProtocolo.Mount;

    InternalEnviar(AContext.Connection.IOHandler, lResposta);
    Result := True;
  finally
    if lProtocolo <> nil then
      FreeAndNil(lProtocolo);
  end;
end;

function TRpDataFlashServerConnection.ExecutarComando(const AItem : TRpDataFlashConnectionItem;
  AContext: TIdContext; const AProtocol : TRpDataFlashProtocol; out ASaida : string;
  out AArquivo : string; const ARequestInfo: TIdHTTPRequestInfo = nil;
  AResponseInfo: TIdHTTPResponseInfo = nil) : Boolean;
var
  lComando: IRpDataFlashCommandInterfaced;
  lParametros: TRpDataFlashCommandParameterList;
  lTcpClient: TRpDataFlashCustomClientConnection;
  lContinuar: Boolean;
  lStatusProcessamento: TRpDataFlashProcessingStatus;
  lNomeComando: string;
  lCarregado: Boolean;

  procedure PrepararExecucaoComando;
  begin
    lComando.SetCallBackEvent(AContext, AItem.OnCallBack);
    lComando.SetServer( Self );
    lComando.SetConnectionItem( AItem );
  end;

  procedure Executar;
  begin
    try
      PrepararExecucaoComando;
      ASaida := lComando.Execute(lParametros, AItem.Executor);
      lComando.SetConnectionItem( nil );
      NewLog(slOnCommand, 'Tipo = ' + IntToStr(Integer(lComando.ProcessType)) + ' - Comando ' + lComando.Command + ' executado !', AContext);
    except on E:Exception do
      begin
        NewLog(slOnError, 'Tipo = ' + IntToStr(Integer(lComando.ProcessType)) + ' - Comando ' + lComando.Command + ' Erro -> ' + E.Message, AContext);
        AItem.Executor.DisconnectDataComponent;
        raise;
      end;
    end;
  end;

  procedure ExecutarPonteInvalida;
  var
    lMsg: string;
  begin
    lContinuar := False;
    PrepararExecucaoComando;
    ASaida := lComando.ExecuteBridgeError(lParametros, AItem.Executor, lContinuar);
    lComando.SetConnectionItem( nil );
    if not lContinuar then
    begin
      lMsg := Trim(lComando.LastError);
      if lMsg = EmptyStr then
        lMsg := Format('Ponte OffLine - servidor: %s porta: %d', [lTcpClient.Server, lTcpClient.ConfigTCPIP.Port]);

      ASaida := lMsg;

      NewLog(slOnBridge, lMsg, AContext);
    end
    else
      NewLog(slOnBridge, 'Executado como Ponte inválida ', AContext);
  end;

  procedure ExecutarPonteBemSucedida;
  begin
    lContinuar := True;
    PrepararExecucaoComando;
    ASaida := lComando.ExecuteBridgeSuccessfully(lParametros, AItem.Executor, lContinuar);
    lComando.SetConnectionItem( nil );
    if not lContinuar then
    begin
      ASaida := 'Ponte Online com execução bem sucedida, mas execucao local falhou!';
      NewLog(slOnBridge, 'Ponte Online com execução bem sucedida, mas execucao local falhou!', AContext);
    end
    else
      NewLog(slOnBridge, 'Executado como Ponte bem-sucedida ', AContext);
  end;

  function DoValidarAntesComunicar : Boolean;
  begin
    Result := True;
    lComando.ExecuteBeforeBridgeConnection(lComando.GetParams, AItem.Executor, Result);
  end;

  procedure Comunicar;
  var
    lExecutado : Boolean;
  begin
    lExecutado := False;

    if lTcpClient = nil then
      raise ERpDataFlashException.Create('Cliente da Ponte = nil');

    if lStatusProcessamento = psBridgeOnLine then
    begin
      try
        if DoValidarAntesComunicar then
        begin
          ASaida := lTcpClient.Comunicar(lComando, lParametros);
          // se vai comunicar, a ponte deve estar onLine (quando processa no servidor,
          // o status muda para tspServidor, então volta para a ponte com o sinal de OnLine)
          lParametros.ProcessingStatus := psBridgeOnLine;
          lComando.DoLoad(loReceive, lParametros);
          ASaida := lParametros.Serialize;

          NewLog(slOnBridge, Format('Comando %s repassado para a ponte - servidor: %s porta: %d',
            [lComando.Command, lTcpClient.Server, lTcpClient.ConfigTCPIP.Port]), AContext);
          lExecutado := lComando.ReturnStatus;
        end;
      except
      end;
    end;

    if not lExecutado then
    begin
      if (lComando.ProcessType = prtRemote) then
        Executar
      else
        if (lComando.ProcessType = prtRemoteOnly) then
          ExecutarPonteInvalida;
    end
    else
    begin
      if (lComando.ProcessType = prtRemoteOnly) then
        ExecutarPonteBemSucedida;
    end;
  end;

begin
  ASaida := EmptyStr;
  lNomeComando := EmptyStr;

  lParametros := nil;
  lTcpClient := nil;
  try
    if AProtocol.Identifier <> TAG_COMMAND then
      raise ERpDataFlashException.Create('Identificador não é um comando: ' + sLineBreak +
        'Identificador: ' + AProtocol.Identifier);

    lCarregado := False;
    if FUtilizarControllers then
    begin
      // localiza um provider ligado ao servidor
      lCarregado := CarregarComandoViaProviders(AProtocol.Message, lComando, lParametros);
      if not lCarregado then
      begin
        lComando := nil;
        if lParametros <> nil then
          FreeAndNil(lParametros);
        // vai para os comandos registrados no controller
        lCarregado := CarregarComandoViaControllers(AProtocol.Message, lComando, lParametros);
        if not lCarregado then
        begin
          lComando := nil;
          if lParametros <> nil then
            FreeAndNil(lParametros);
        end;
      end;
    end;

    if not lCarregado then
      lCarregado := TRpDataFlashCommand.LoadCommand(AProtocol.Message, lComando, lParametros, Self, AItem);

    if not lCarregado then
      raise ERpDataFlashException.CreateFmt('Comando não suportado: Comando [%s].', [lParametros.Command]);

    lNomeComando := lComando.Command;
    lComando.RequestInfo := ARequestInfo;
    lComando.ResponseInfo := AResponseInfo;

    // se tem rotina de autenticacao e o comando recebido nao é para autenticar, aborta o processo
    if Assigned(FOnAuthenticateClient) and (FAuthEnable) and (not AItem.Authenticated) and ComandoRequerAutenticacao(lNomeComando, lComando.GetParams) then
      raise ERpDataFlashUserNotFound.Create('Este servidor requer autenticação para execução de comandos.');

    lComando.Executor := AItem.Executor;
    if lComando.ProcessType = prtLocal then
      lStatusProcessamento := psLocal //tspServidor
    else
      lStatusProcessamento := CarregarStatusProcessamento(AItem, lTcpClient);

    lParametros.ProcessingStatus := lStatusProcessamento;

    if lStatusProcessamento in [psServer, psLocal] then
      Executar
    else
      Comunicar;

    if ASaida = EmptyStr then
      raise Exception.Create('Comando ' + lNomeComando + ' não executado !');

    AArquivo := lComando.GetResponseFileName;
  finally
    if lParametros <> nil then
      FreeAndNil(lParametros);
  end;

  Result := True;
end;

procedure TRpDataFlashServerConnection.Finalizar;
begin
  inherited;
  Disconnect;
  FreeAndNil(FControllers);
  FreeAndNil(FProviders);
  FreeAndNil(FInterfaceList);
end;

function TRpDataFlashServerConnection.GetConnected: Boolean;
var
  lRestOk: Boolean;
  lTcpOk: Boolean;
begin
//  Result := False;

  if FConfigREST.Enabled then
    lRestOk := Assigned(FConectorREST) and FConectorREST.Active
  else
    lRestOk := True;

  if FConfigTCPIP.Enabled then
    lTcpOk := Assigned(FConectorTCP) and FConectorTCP.Active
  else
    lTcpOk := True;

  // 2 ativos
  if FConfigREST.Enabled and FConfigTCPIP.Enabled then
    Result := lRestOk and lTcpOk
  else // somente Rest
    if FConfigREST.Enabled then
      Result := lRestOk
    else // Somente TCP
      if FConfigTCPIP.Enabled then
        Result := lTcpOk
      else
      begin
        Result := False;
        if not (csDesigning in ComponentState) then
          raise Exception.Create('Não existem conexões habilitadas.');
      end;
end;

function TRpDataFlashServerConnection.GetControllers: TRpDataFlashCommandControllerList;
begin
  Result := FControllers;
end;

function TRpDataFlashServerConnection.GetControllersCount: Integer;
begin
  Result := Controllers.Count;
end;

function TRpDataFlashServerConnection.GetConnectedClientsCount: Integer;
begin
  Result := FNumeroConectados;
end;

function TRpDataFlashServerConnection.GetProviders: TRpDataFlashProviderControllerList;
begin
  Result := FProviders;
end;

function TRpDataFlashServerConnection.GetProvidersCount: Integer;
begin
  Result := Providers.Count;
end;

function TRpDataFlashServerConnection.GetServidor: string;
begin
  Result := FServer;
end;

function TRpDataFlashServerConnection.GetUseControllers: Boolean;
begin
  Result := FUtilizarControllers;
end;

procedure TRpDataFlashServerConnection.Inicializar;
begin
  inherited;
  FInterfaceList := TInterfaceList.Create;
  FServer := GetLocalComputerName;
  FControllers := TRpDataFlashCommandControllerList.Create(False);
  FProviders := TRpDataFlashProviderControllerList.Create(False);
  FUtilizarControllers := True;
  FAuthEnable := False;
  FBaseCommandPrefix := 'TComando';
end;

function TRpDataFlashServerConnection.IsPonte: Boolean;
begin
  Result := (FBridge <> nil);
end;

function TRpDataFlashServerConnection.ItemConexao(AContext: TIdContext): TRpDataFlashConnectionItem;
begin
  Result := TRpDataFlashConnectionItem(AContext.Data);
end;

procedure TRpDataFlashServerConnection.Clear;
begin
  inherited;
  FServer := GetLocalComputerName;
end;

function TRpDataFlashServerConnection.FindInstance(const ACommand: string): IRpDataFlashCommandInterfaced;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FInterfaceList.Count - 1 do
  begin
    if (FInterfaceList[I] as IRpDataFlashCommandInterfaced).Command = ACommand then
    begin
      Result := (FInterfaceList[I] as IRpDataFlashCommandInterfaced);
      Exit;
    end;
  end;
end;

procedure TRpDataFlashServerConnection.NotificarConexaoCliente(const AConnectionItem: TRpDataFlashConnectionItem;
  const AEvento : TRpDataFlashOnClientConnection);
var
  lLog: TRpDataFlashSyncConexaoEvent;
begin
  lLog := TRpDataFlashSyncConexaoEvent.Create;
  try
    lLog.FLRDataFlashIP := Self;
    lLog.FEvento := AEvento;
    lLog.FConexaoItem := AConnectionItem;
    lLog.DoNotify;
  finally
    FreeAndNil(lLog);
  end;
end;

procedure TRpDataFlashServerConnection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FBridge) then
    FBridge := nil;

  if (Operation = opRemove) and (AComponent.InheritsFrom(TRpDataFlashCommandController)) then
    FControllers.Remove(AComponent);

  if (Operation = opRemove) and (AComponent.InheritsFrom(TRpDataFlashCustomDataSetProvider)) then
    FProviders.Remove(AComponent);
end;

procedure TRpDataFlashServerConnection.OnFTPServerAfterUserLogin(
  ASender: TIdFTPServerContext);
begin
  ASender.HomeDir    := FFileTransfer.TempDir;
  ASender.CurrentDir := FFileTransfer.TempDir;
end;

procedure TRpDataFlashServerConnection.OnFTPServerDeleteFile(
  ASender: TIdFTPServerContext; const APathName: string);
var
  lFileID : string;
  lFileName : string;
  lDelTemp: Boolean;
  lDelOriginal: Boolean;
begin
  lFileID := StringReplace(APathName, ASender.CurrentDir, '', [rfIgnoreCase, rfReplaceAll]);
  lFileID := StringReplace(lFileID, '/', '', [rfReplaceAll]);
  // 1º char - delete temp
  lDelTemp := Copy(lFileID, 1, 1) = 'T';
  // 2º char - delete original
  lDelOriginal := Copy(lFileID, 2, 1) = 'T';
  Delete(lFileID, 1, 2);

  if lDelOriginal then
  begin
    lFileName := FFileTransferList.Values[lFileID];
    if FileExists(lFileName) then
    begin
      NewLog(slOnStatus, 'FTP: Remover arquivo ID[' + lFileID + '] nome[' + lFileName + ']', ASender);
      DeleteFile(PChar(lFileName));
    end;
  end;

  if lDelTemp then
  begin
    lFileName := FFileTransfer.TempDir + lFileID + '.tmp';
    if FileExists(lFileName) then
    begin
      NewLog(slOnStatus, 'FTP: Remover arquivo temp ID[' + lFileID + '] nome[' + lFileName + ']', ASender);
      DeleteFile(PChar(lFileName));
    end;
  end;
end;

procedure TRpDataFlashServerConnection.OnFTPServerException(AContext: TIdContext;
  AException: Exception);
begin
  NewLog(slOnError, 'Erro no servidor FTP. ' + AException.Message, AContext);
end;

procedure TRpDataFlashServerConnection.OnFTPServerRetrieveFile(
  ASender: TIdFTPServerContext; const AFileName: string; var VStream: TStream);
var
  lID : string;
  lFile : TRpFileProxy;
  lList : TStringList;
begin
  lID := '';

  lList := TStringList.Create;
  try
    lList.Text := Trim(WrapText(AFileName, sLineBreak, ['/', '\'], 10));
    lID := lList[lList.Count - 1];
    lID := StringReplace(lID, '/', '', [rfReplaceAll]);
  finally
    FreeAndNil(lList);
  end;

  NewLog(slOnStatus, 'FTP: Arquivo solicitado ID[' + lID + ']', ASender);

  lFile := TRpFileProxy.Create;
  try
    lFile.Get(Self, lID);

    NewLog(slOnStatus, 'FTP: Tamanho do arquivo solicitado ID [' + lID + '] ' + lFile.FileSizeFmt, ASender);

    if lFile.FileName = EmptyStr then
      lFile.FileName := FileTransfer.TempDir + lFile.FileID + '.tmp';

    lFile.FileStream.Position := 0;
    if VStream = nil then
      VStream := TMemoryStream.Create;
    VStream.CopyFrom(lFile.FileStream, lFile.FileStream.Size);
//    lFile.Remove;

    NewLog(slOnStatus, 'FTP: Arquivo ID [' + lID + '] ' + lFile.FileName + ' removido ', ASender);

  finally
    if Assigned(VStream) then
      VStream.Position := 0;
    FreeAndNil(lFile);
  end;
end;

procedure TRpDataFlashServerConnection.OnFTPServerStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
var
  lMens : string;
begin
  case AStatus of
    hsResolving:     lMens := 'Resolving';
    hsConnecting:    lMens := 'Connecting';
    hsConnected:     lMens := 'Connected';
    hsDisconnecting: lMens := 'Disconnecting';
    hsDisconnected:  lMens := 'Disconnected';
    hsStatusText:    lMens := 'StatusText';
    ftpTransfer:     lMens := 'Transfer';
    ftpReady:        lMens := 'Ready';
    ftpAborted:      lMens := 'Aborted';
  end;

  NewLog(slOnStatus, 'Status FTP ' + lMens + ' ' + AStatusText, '');
end;

procedure TRpDataFlashServerConnection.OnFTPServerStoreFile(
  ASender: TIdFTPServerContext; const AFileName: string; AAppend: Boolean;
  var VStream: TStream);
var
  lFileName: string;
  lFileID: string;
begin
  lFileID := StringReplace(AFileName, ASender.CurrentDir, '', [rfIgnoreCase]);
  lFileID := StringReplace(lFileID, '/', '', []);

  NewLog(slOnStatus, 'FTP: Receber arquivo ID[' + lFileID + ']', ASender);

  lFileName := FFileTransfer.TempDir + lFileID + '.tmp';

  VStream := TFileStream.Create(lFileName, fmCreate);
  FileTransfer_RegisterFile(lFileID, lFileName);
end;

procedure TRpDataFlashServerConnection.OnFTPServerUserLogin(
  ASender: TIdFTPServerContext; const AUsername, APassword: string;
  var AAuthenticated: Boolean);
begin
  AAuthenticated := (AUsername = C_FTP_DEFAULT_USER)
                and (APassword = C_FTP_DEFAULT_PWD);
end;

procedure TRpDataFlashServerConnection.Receber(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  lPedido: string;
  lResultado: string;
  lItem: TRpDataFlashConnectionItem;
  lResposta: string;
  lProtocolo: TRpDataFlashProtocol;
  lContinue: Boolean;
  lMessage: string;
  lNomeArquivo: string;
begin
  try
    lItem := ItemConexao(AContext);
    if lItem <> nil then
    begin
      lResultado := EmptyStr;
      lResposta := EmptyStr;

      if Assigned(FOnManualProcessing) then
        FOnManualProcessing(Self, lItem)
      else
      begin
        if FMessageType = mtCommand then
        begin
          lProtocolo := TRpDataFlashProtocol.Create(EncryptionType);
          try
            lProtocolo.MessageType := FMessageType;

            if ARequestInfo = nil then
            begin
              lPedido := InternalReceber(AContext.Connection.IOHandler);
              lProtocolo.Message := lPedido;
            end
            else
            begin
              lPedido := InternalReceber(ARequestInfo, AResponseInfo);
              if lProtocolo.Identifier = EmptyStr then
                lProtocolo.Identifier := TAG_COMMAND;
              lProtocolo.Message := lPedido;
            end;

            lContinue := True;
            lMessage := EmptyStr;
            if Assigned(FOnBeforeExecuteCommand) then
              FOnBeforeExecuteCommand(Self, lItem, lContinue, lMessage);

            if not lContinue then
              raise ERpDataFlashBeforeExecuteCommandError.Create(lMessage);

            ExecutarComando(lItem, AContext, lProtocolo, lResultado, lNomeArquivo, ARequestInfo, AResponseInfo);

            lProtocolo.Message := lResultado;
            lResposta := lProtocolo.Mount;

            DoInternalEnviar(
              AContext.Connection.IOHandler,
              ARequestInfo,
              AResponseInfo,
              lResposta,
              AContext,
              lNomeArquivo);

          finally
            if lProtocolo <> nil then
              FreeAndNil(lProtocolo);
          end;
        end
        else
        begin
          lPedido := InternalReceber(AContext.Connection.IOHandler);
          if Assigned(FOnReceivingText) then
            lResposta := FOnReceivingText(Self, lPedido, lItem);
          InternalEnviar(AContext.Connection.IOHandler, lResposta);
        end;
      end;
    end
    else
      raise Exception.Create('Cliente não encontrado !');
  except on E:Exception do
    raise Exception.Create('<<>>' + E.Message);
  end;
end;

procedure TRpDataFlashServerConnection.SetBridge(const Value: TRpDataFlashCustomClientConnection);
begin
  if Assigned(FBridge) then
    FBridge.RemoveFreeNotification(Self);

  FBridge := Value;

  if Assigned(FBridge) then
    FBridge.FreeNotification(Self);
end;

procedure TRpDataFlashServerConnection.SetUseControllers(const Value: Boolean);
begin
  FUtilizarControllers := Value;
end;

procedure TRpDataFlashServerConnection.AddInstance(const AInstance: IRpDataFlashCommandInterfaced);
var
  I : Integer;
  lExiste: Boolean;
begin
  // antes de adicionar, verifica se não esta na lista
  lExiste := False;
  for I := 0 to FInterfaceList.Count - 1 do
  begin
    lExiste := (FInterfaceList[I] as IRpDataFlashCommandInterfaced).Command = AInstance.GetCommand;
    if lExiste then
      Break;
  end;

  if not lExiste then
    FInterfaceList.Add(AInstance);
end;

procedure TRpDataFlashServerConnection.AfterConstruction;
begin
  inherited;
end;

procedure TRpDataFlashServerConnection.AoConectarCliente(AContext: TIdContext);
var
  lConexao: TRpDataFlashConnectionItem;
  lPermitir: Boolean;
begin
  lPermitir := True;
  if Assigned(FOnBeforeClientConnection) then
    FOnBeforeClientConnection(Self, lPermitir);

  if lPermitir then
  begin
    lConexao := TRpDataFlashConnectionItem.Create(Self, AContext.Connection.IOHandler);
    lConexao.ClientID := IntToStr(AContext.Binding.Handle);
    lConexao.ClientName := AContext.Binding.PeerIP;
    lConexao.OnCallBack := EnviarCallBack;
    AContext.Data := lConexao;
    Inc(FNumeroConectados);

    NotificarConexaoCliente(lConexao, FOnClientConnection);

    NewLog(slOnConnection, 'Cliente Conectou: ' + AContext.Binding.PeerIP, AContext);
  end
  else
  begin
    AContext.Connection.Disconnect;
    NewLog(slOnDisconnection, 'Desconectando Cliente: ' + AContext.Binding.PeerIP, AContext);
  end;
end;

procedure TRpDataFlashServerConnection.AoDesconectarCliente(AContext: TIdContext);
var
  lItemConexao: TRpDataFlashConnectionItem;
begin
  if (AContext.Data <> nil) then
  begin
    lItemConexao := TRpDataFlashConnectionItem(AContext.Data);
    NewLog(slOnDisconnection, 'Desconectando Cliente ' + AContext.Binding.PeerIP, AContext);
    Dec(FNumeroConectados);
    if (not FDesconectandoServer) and (Assigned(FOnClientDisconnect)) then
      NotificarConexaoCliente(lItemConexao, FOnClientDisconnect);

    if lItemConexao.Executor <> nil then
      lItemConexao.Executor.DisconnectDataComponent;
  end;
end;

procedure TRpDataFlashServerConnection.AoExecutarNoServidor(AContext: TIdContext);
var
  lException : string;
begin
  CoInitialize(nil);
  try
    try
      Receber(AContext);
    except
      on E:EIdNotConnected do
      begin
        // ocorre quando o servidor é fechado com clientes conectador
        NewLog(slOnError, 'Uma conexão foi encerrada. ' + E.Message, AContext);
      end;
      on E:Exception do
      begin
        lException := TRpDataFlashProtocol.NewError(Self.EncryptionType, CreateException(E));
        InternalEnviar(AContext.Connection.IOHandler, lException);
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

procedure TRpDataFlashServerConnection.AoExecutarNoServidorRest(
  AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo;
  AResponseInfo: TIdHTTPResponseInfo);
var
  lException : string;
  lType: TSerializationFormat;
  lProtocolo: TRpDataFlashProtocol;
  lValue: string;
begin
  CoInitialize(nil);

  // vem do componente
  if ARequestInfo.ContentType = C_REST_CONTENT_TYPE_DELPHI then
  begin
    // reverter a criptografia
    try
      lProtocolo := TRpDataFlashProtocol.Create(EncryptionType);
      lProtocolo.Message := ARequestInfo.UnparsedParams;
      lValue := lProtocolo.Message;
      if lValue[1] = '<' then
        lType := sfXML
      else
        lType := sfJSON;
    finally
      FreeAndNil(lProtocolo);
    end;
  end
  else
  begin
    if (Pos('INTERNAL_FORMATTYPE=2', AnsiUpperCase(ARequestInfo.QueryParams)) > 0)
    or (ARequestInfo.ContentType = C_REST_CONTENT_TYPE_JSON) then
      lType := sfJSON
    else
      lType := sfXML;
  end;

  try
    try
      Receber(AContext, ARequestInfo, AResponseInfo);
    except
      on E:Exception do
      begin
        lException := TRpDataFlashProtocol.NewError(Self.EncryptionType, CreateException(E, lType));

        DoInternalEnviar(
          AContext.Connection.IOHandler,
          ARequestInfo,
          AResponseInfo,
          lException,
          AContext,
          '');
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

{ TRpDataFlashConnectionItem }

procedure TRpDataFlashConnectionItem.AddInstance(
  const AInstance: IRpDataFlashCommandInterfaced);
var
  I : Integer;
  lExiste: Boolean;
begin
  // antes de adicionar, verifica se não esta na lista
  lExiste := False;
  for I := 0 to FInterfaceList.Count - 1 do
  begin
    lExiste := (FInterfaceList[I] as IRpDataFlashCommandInterfaced).Command = AInstance.GetCommand;
    if lExiste then
      Break;
  end;

  if not lExiste then
    FInterfaceList.Add(AInstance);
end;

constructor TRpDataFlashConnectionItem.Create(AOwner : TRpDataFlashCustomConnection; pHandler: TIdIOHandler);
var
  lPonte: TRpDataFlashCustomClientConnection;
begin
  FAutenticado := False;
  FUsername := EmptyStr;
  FPassword := EmptyStr;
  FOwner := AOwner;
  FHandler := pHandler;
  FClientName := EmptyStr;
  FInterfaceList := TInterfaceList.Create;
  FExecutor := nil;

  if (AOwner is TRpDataFlashServerConnection) and (TRpDataFlashServerConnection(AOwner).Bridge <> nil) then
  begin
    FTcpClientPonte := TRpDataFlashClientConnection.Create(nil);
    FTcpClientPonte.Name := 'TcpClientPonte_' + IntToStr(Abs(GetTickCount));
    lPonte := TRpDataFlashServerConnection(AOwner).Bridge;
    FTcpClientPonte.ClonarDe(lPonte);
    FTcpClientPonte.LazyConnection := True;
  end;

  FQuebras := TRpDataFlashProtocolBreakerList.Create;
end;

destructor TRpDataFlashConnectionItem.Destroy;
begin
  if (Assigned(FTcpClientPonte) ) then
  begin
    FTcpClientPonte.Disconnect;
    FreeAndNil(FTcpClientPonte);
  end;

  if FExecutor <> nil then
  begin
    FExecutor.DisconnectDataComponent;
    FExecutor := nil;
  end;

  FreeAndNil(FQuebras);
  FreeAndNil(FInterfaceList);
  inherited;
end;

function TRpDataFlashConnectionItem.GetAuthenticated: Boolean;
begin
  Result := FAutenticado;
end;

function TRpDataFlashConnectionItem.GetPassword: string;
begin
  Result := FPassword;
end;

function TRpDataFlashConnectionItem.GetUserName: string;
begin
  Result := FUsername;
end;

function TRpDataFlashConnectionItem.Ip: string;
begin
  Result := FClientName;
end;

function TRpDataFlashConnectionItem.FindInstance(const ACommand: string): IRpDataFlashCommandInterfaced;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FInterfaceList.Count - 1 do
  begin
    if (FInterfaceList[I] as IRpDataFlashCommandInterfaced).Command = ACommand then
    begin
      Result := (FInterfaceList[I] as IRpDataFlashCommandInterfaced);
      Exit;
    end;
  end;
end;

procedure TRpDataFlashConnectionItem.SetAuthenticated(const Value: Boolean);
begin
  FAutenticado := Value;
end;

procedure TRpDataFlashConnectionItem.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TRpDataFlashConnectionItem.SetUsername(const Value: string);
begin
  FUsername := Value;
end;

{ TRpDataFlashCustomClientConnection }

function TRpDataFlashCustomClientConnection.Comunicar(const AIdentificador,
  AMenssage, ANomeComando: string): string;
begin
  Result := EmptyStr;

  try
    Result := Enviar(AIdentificador, AMenssage, ANomeComando);
  except
    on E:Exception do
    begin
      NewException(E);
      raise;
    end;
  end;
end;

function TRpDataFlashCustomClientConnection.ConectarFtp(
  out ACliente: TIdFTP): Boolean;
begin
  Result := False;
  ACliente := nil;
  if (FFileTransfer.Port > 0) and (FFileTransfer.Enabled) then
  begin
    ACliente := TIdFTP.Create(Self);
    ACliente.Host := Server;
    ACliente.Port := FileTransfer.Port;
    ACliente.DataPort := FileTransfer.DataPort;
    ACliente.OnStatus := OnFTPClientStatus;

    ACliente.Username := C_FTP_DEFAULT_USER;
    ACliente.Password := C_FTP_DEFAULT_PWD;
    try
      ACliente.Connect;
      Result := True;
    except
      // retorna um erro "amigavel"
      on E:Exception do
      begin
        FreeAndNil(ACliente);
        if Pos('Socket Error # 10061', E.Message) > 0 then
          raise ERpDataFlashFTPError.CreateFmt('Não foi possível estabelecer a conexão FTP com %s:%d. %s',
            [ACliente.Host, ACliente.Port, E.Message])
        else
          raise ERpDataFlashFTPError.Create('Erro de FTP. ' + E.Message);
      end;
    end;
  end;
end;

function TRpDataFlashCustomClientConnection.ConfirmFileReceipt(
  const AFileID: string; const ADeleteTemp, ADeleteOriginal: Boolean;
  AFtpClient: TIdFTP): Boolean;
var
  lClienteFtp: TIdFTP;
  lFileID: string;
begin
//  Result := False;

  if AFtpClient = nil then
    ConectarFtp(lClienteFtp)
  else
    lClienteFtp := AFtpClient;

  try
    if not Assigned(lClienteFtp) then
      raise Exception.Create('Protocolo para transferência de arquivo não foi inicializado.');
    lFileID := IfThen(ADeleteTemp,     'T', 'F')
             + IfThen(ADeleteOriginal, 'T', 'F')
             + AFileID;
    lClienteFtp.Delete(lFileID);
    Result := True;
  finally
    if AFtpClient = nil then
      FreeAndNil(lClienteFtp);
  end;
end;

function TRpDataFlashCustomClientConnection.Comunicar(const ACommand: TRpDataFlashCommand): string;
begin
  if ACommand.SerializationFormat = sfAuto then
    ACommand.SerializationFormat := Self.SerializationFormat;

  Result := Comunicar(ACommand, ACommand.Params);
end;

function TRpDataFlashCustomClientConnection.Autenticar(out AErrorMessage: string): Boolean;
begin
  Result := Autenticar(FUserName, FPassword, AErrorMessage);
end;

function TRpDataFlashCustomClientConnection.Autenticar(const AUsername,
  APassword: string; out AErrorMessage: string): Boolean;
begin
  Result := TRpDataFlashComandoAutenticar.Autenticar(Self, AUsername, APassword, AErrorMessage);
end;

procedure TRpDataFlashCustomClientConnection.ClonarDe(const AOther: TRpDataFlashCustomClientConnection;
  const AClonarEnventos : Boolean);
begin
  FServer := AOther.Server;
  FEncryptionType := AOther.EncryptionType;
  FLoginPrompt := AOther.LoginPrompt;
  FCommunicationType := AOther.CommunicationType;
  FConnectionTimeOut := AOther.ConnectionTimeOut;
  FReadTimeOut := AOther.ReadTimeOut;
  FReconnectionTimeOut := AOther.ReconnectionTimeOut;
  FLazyConnection := AOther.LazyConnection;
  FUserName := AOther.UserName;
  FPassword := AOther.Password;
  FConvertLocalHostToIP := AOther.ConvertLocalHostToIP;

  FConfigTCPIP.Port := AOther.ConfigTCPIP.Port;
  FConfigTCPIP.Enabled := AOther.ConfigTCPIP.Enabled;

  FConfigREST.Port := AOther.ConfigREST.Port;
  FConfigREST.Enabled := AOther.ConfigREST.Enabled;

  FFileTransfer.Port := AOther.FileTransfer.Port;
  FFileTransfer.Enabled := AOther.FileTransfer.Enabled;
//  FFileTransfer.DataPort := AOther.FileTransfer.DataPort;
//  FFileTransfer.TempDir  := AOther.FileTransfer.TempDir;

  // eventos
  if AClonarEnventos then
  begin
    FOnConnect := AOther.OnConnect;
    FOnDisconnect := AOther.OnDisconnect;
    FOnSendingError := AOther.OnSendingError;
    FOnNoService := AOther.OnNoService;
    FOnException := AOther.OnException;
    FOnNewLog := AOther.OnNewLog;
  end;
end;

function TRpDataFlashCustomClientConnection.Comunicar(const ACommand: IRpDataFlashCommandInterfaced; const AParametros : TRpDataFlashCommandParameterList): string;
begin
  if ACommand.SerializationFormat = sfAuto then
  begin
    AParametros.SerializationFormat := Self.SerializationFormat;
    ACommand.SerializationFormat := Self.SerializationFormat;
  end;

  AParametros.Command := ACommand.Command;
  ACommand.DoSerialize(AParametros);
  Result := Comunicar(TAG_COMMAND, AParametros.Serialize);
  AParametros.Load(Result);
  ACommand.DoLoad(loReceive, AParametros);
end;

function TRpDataFlashCustomClientConnection.Comunicar(const ACommand: TRpDataFlashCommand; const ACallBackClassName: string): string;
begin
  if ACommand.SerializationFormat = sfAuto then
    ACommand.SerializationFormat := Self.SerializationFormat;

  Result := DoComunicarComThred(ACommand, ACallBackClassName, nil);
end;

constructor TRpDataFlashCustomClientConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLoginPrompt := False;
  FConnectionTimeOut := -1;
  FReadTimeOut := 0;
  FReconnectionTimeOut := 0;
  FLazyConnection := False;
  FExecutorCallBackClass := EmptyStr;
  FExecutorCallBackInterfaced := nil;
  FSerializationFormat := sfAuto;
end;

procedure TRpDataFlashCustomClientConnection.DoLoadConfig(const ANode: IXMLNode);
begin
  inherited;
  ANode['TimeOutLeitura'] := FReadTimeOut;
  ANode['TimeOutConexao'] := FConnectionTimeOut;
  ANode['EsperaReconexao'] := FReconnectionTimeOut;
end;

procedure TRpDataFlashCustomClientConnection.DoConnect;
begin
  inherited;
end;

procedure TRpDataFlashCustomClientConnection.DoDisconnect;
begin
  FConnectionHelper.LiberarConector;

  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, FServer, ConfigTCPIP.Port);
end;

procedure TRpDataFlashCustomClientConnection.DoNovaExcecao(const E: Exception);
begin
  NewException(E);
end;

procedure TRpDataFlashCustomClientConnection.DoSaveConfig(const ANode: IXMLNode);
begin
  inherited;
  FReadTimeOut := ANode['TimeOutLeitura'];
  FConnectionTimeOut := ANode['TimeOutConexao'];
  FReconnectionTimeOut := ANode['EsperaReconexao'];
end;

function TRpDataFlashCustomClientConnection.Enviar(const AIdentificador, AMenssage,
  ANomeComando : string): string;
var
  lResultado : string;
  lIdentificadorEnviado: string;
  lProtocolo: TRpDataFlashProtocol;
begin
  Result := EmptyStr;
  try
    Connect;

    if Connected then
    begin
      lProtocolo := TRpDataFlashProtocol.Create(Self.EncryptionType);
      try
        lProtocolo.Identifier := AIdentificador;
        lProtocolo.Message := AMenssage;

        lIdentificadorEnviado := lProtocolo.Identifier;
        InternalEnviar(FConnectionHelper.Conector.IOHandler, lProtocolo.Mount);

        repeat
          lResultado := InternalReceber(FConnectionHelper.Conector.IOHandler);

          lProtocolo.Clear;
          lProtocolo.Message := lResultado;

          TentaGerarException(lProtocolo);

          TentaExecutarCallBack(lProtocolo);

        until (lProtocolo.Identifier <> TAG_CALLBACK);

        if AIdentificador <> lProtocolo.Identifier then
          raise ERpDataFlashSending.Create('A mensagem recebida não é a esperada' + sLineBreak + lProtocolo.Message);

        Result := lProtocolo.Message;
      finally
        FreeAndNil(lProtocolo);
      end;
    end
    else
      raise Exception.Create('Sem conexão com o servidor !');
  except
    on E:Exception Do
    begin
      if Pos('closed gracefully', LowerCase(E.Message)) > 0 then
        Disconnect;
      raise;
    end;
  end;
end;

procedure TRpDataFlashCustomClientConnection.Finalizar;
begin
  inherited;
//  if FThreadConexao <> nil then
//  FThreadConexao.Terminate;
////
//  while FThreadConexao.Conectando do
//    Sleep(200);
//
//  while not FThreadConexao.IsTerminated do
//    Sleep(200);
//
//  FThreadConexao.Suspend;
//  if FThreadConexao.ThreadID <> MainThreadID then
//    FreeAndNil(FThreadConexao);

//  while FThreadConexao <> nil do
//    Sleep(200);

  FreeAndNil( FConnectionHelper );
end;

function TRpDataFlashCustomClientConnection.GetConnected: Boolean;
begin
  try
    if Assigned(FThreadConexao) then
    begin
      Result := (not FThreadConexao.Conectando)
            and (FConnectionHelper.IsConectado);
    end
    else
      Result := (FConnectionHelper.IsConectado);
  except
    Result := False;
  end;
end;

function TRpDataFlashCustomClientConnection.GetFile(const AFileID: string;
  AArquivo: IRpFileProxy): Boolean;
var
  lClienteFtp: TIdFTP;
  lFileInfo: TRpDataFlashFtpFileInfo;
begin
  Result := False;
  lFileInfo.Decode(AFileID);

  if (lFileInfo.FileID = '-1') or (lFileInfo.FileSize <= 0) then
    Exit;

  if ConectarFtp(lClienteFtp) then
  begin
    try
      inherited GetFile(lFileInfo.FileID, AArquivo);
      AArquivo.FileID := lFileInfo.FileID;

      if not Assigned(lClienteFtp) then
        raise Exception.Create('Protocolo para transferência de arquivo não foi inicializado.');

      if lClienteFtp.Connected then
      begin
        if AArquivo.FileName = EmptyStr then
        begin
          lFileInfo.FileName := FileTransfer.TempDir + AArquivo.FileID + '.tmp';
          lFileInfo.FileDelete := False;
          FileTransfer_RegisterFile(lFileInfo.FileID, lFileInfo.FileName);
        end
        else
          lFileInfo.FileName := AArquivo.FileName;

        lClienteFtp.ReadTimeout := 5000;
        lClienteFtp.Get(lFileInfo.FileID, lFileInfo.FileName, True, False);
        AArquivo.LoadFromFile(lFileInfo.FileName);

        if (lFileInfo.FileSize > 0) and (lFileInfo.FileSize <> AArquivo.FileSize) then
          raise Exception.CreateFmt('O arquivo recebido é diferente do original (bytes enviados: %s | bytes recebidos: %s).',
            [IntToStr(lFileInfo.FileSize), IntToStr(AArquivo.FileSize)]);

        Result := AArquivo.FileSize > 0;

        if Result and lFileInfo.FileDelete then
          ConfirmFileReceipt(lFileInfo.FileID, False, True, lClienteFtp);

        lClienteFtp.Disconnect;
      end;
    finally
      FreeAndNil(lClienteFtp);
    end;
  end;
//    end
//    else
//      Result := False;
//  finally
//    FreeAndNil(lClienteFtp);
//  end;
end;

function TRpDataFlashCustomClientConnection.GetIdentifier: string;
begin
  Result:= EmptyStr;
end;

function TRpDataFlashCustomClientConnection.GetListaItensServidor: string;
begin
  Result := Enviar(DEFAULT_ITEM_ID, EmptyStr, EmptyStr);
end;

function TRpDataFlashCustomClientConnection.GetPort: Integer;
begin
  Result := FConfigTCPIP.Port;
end;

procedure TRpDataFlashCustomClientConnection.Inicializar;
begin
  inherited;
  FUltimaConexao := 0;
  FReadTimeOut := -1;
  FConnectionTimeOut := 0;
  FReconnectionTimeOut := 0;

  FConnectionHelper := GetConnectionHelperClass.Create(Self);
  FConnectionHelper.DoConnect := DoConnect;
  FConnectionHelper.DoDisconnect := DoDisconnect;
  FConnectionHelper.NewException := DoNovaExcecao;
end;

function TRpDataFlashCustomClientConnection.InternalConectar : Boolean;
begin
  // libera o antigo e configura um conector novo
  FConnectionHelper.ReinicializarConector(
    FServer,
    FLoginPrompt,
    Port,
    FConnectionTimeOut,
    FReadTimeOut,
    FConvertLocalHostToIP);
  // ajusta os eventos de conexão
  FConnectionHelper.OnConnect := FOnConnect;
  FConnectionHelper.OnNoService := FOnNoService;
  // dispara o processo de conexão
  Result := FConnectionHelper.Connect;
end;

procedure TRpDataFlashCustomClientConnection.Clear;
begin
  inherited;
end;

procedure TRpDataFlashCustomClientConnection.NovoErroEnvio(const AException: Exception;
  const AProtocol : TRpDataFlashProtocol);
begin
  if Assigned(OnSendingError) then
    OnSendingError(Self, AProtocol, AException);

  NewLog(slOnError, AException.Message, nil);
end;

procedure TRpDataFlashCustomClientConnection.OnFTPClientStatus(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  NewLog(slOnStatus, IntToStr(Integer(AStatus)) + ' - ' + AStatusText, '');
end;

function TRpDataFlashCustomClientConnection.PodeConectar: Boolean;
begin
  Result := ((FReconnectionTimeOut = 0) or (FUltimaConexao = 0) or (Now >= IncMilliSecond(FUltimaConexao, FReconnectionTimeOut)))
    and (not Connected) and ((FThreadConexao <> nil) and (FThreadConexao.Conectando = False));

  if Result then
    FUltimaConexao := Now;
end;

function TRpDataFlashCustomClientConnection.PutFile(const AArquivo: IRpFileProxy): Boolean;
var
  lClienteFtp: TIdFTP;
begin
  Result := False;
  if ConectarFtp(lClienteFtp) then
  begin
    try
      inherited PutFile(AArquivo);

      if not Assigned(lClienteFtp) then
        raise Exception.Create('Protocolo para transferência de arquivo não foi inicializado.');

      if not FileExists(AArquivo.FileName) then
        raise Exception.CreateFmt('O arquivo informado não existe [%s].', [AArquivo.FileName]);

      if AArquivo.FileSize <= 0 then
        AArquivo.LoadFromFile(AArquivo.FileName);

      if lClienteFtp.Connected then
      begin
        lClienteFtp.Put(AArquivo.Stream, AArquivo.FileID);
        Result := True;
      end
      else
        Result := False;
    finally
      FreeAndNil(lClienteFtp);
    end;
  end;
end;

function TRpDataFlashCustomClientConnection.ServerOnline: Boolean;
var
  lClient: TIdTCPClient;
begin
  lClient := TIdTCPClient.Create(nil);
  try
    lClient.Host := FServer;
    lClient.Port := FConfigTCPIP.Port;
    try
      lClient.Connect;
      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(lClient);
  end;
end;

procedure TRpDataFlashCustomClientConnection.SetPort(const Value: Integer);
begin
  FConfigTCPIP.Port := Value;
end;

procedure TRpDataFlashCustomClientConnection.SetServidor(const Value: string);
begin
  FServer := Value;
  inherited;
end;

procedure TRpDataFlashCustomClientConnection.TentaExecutarCallBack(const AProtocol: TRpDataFlashProtocol);
var
  lCallBack: IRpDataFlashExecutorCallBack;
  lComandoClass: TRpDataFlashAbstractClass;
  lParametros: TRpDataFlashCommandParameterList;
begin
  if AProtocol.Identifier = TAG_CALLBACK then
  begin
    lParametros := nil;

    // prepara os parametros para callback
    try
      if (FExecutorCallBackClass <> EmptyStr) or Assigned(FExecutorCallBackInterfaced) then
      begin
        lParametros := TRpDataFlashCommandParameterList.Create(Self);
        lParametros.Load( AProtocol.Message );
      end;

      if FExecutorCallBackClass <> EmptyStr then
      begin
        lComandoClass := TCPClassRegistrer.GetClass(FExecutorCallBackClass);
        lCallBack := (TRpDataFlashCommand(lComandoClass.Create)) as IRpDataFlashExecutorCallBack;
        if (lComandoClass <> nil) and Supports(lComandoClass, IRpDataFlashExecutorCallBack) then
          lCallBack.ExecutarCallBack( lParametros );
      end
      else
        if Assigned(FExecutorCallBackInterfaced) then
          FExecutorCallBackInterfaced.ExecutarCallBack( lParametros );
    finally
      if Assigned(lParametros) then
        FreeAndNil(lParametros);
    end;
  end;
end;

function TRpDataFlashCustomClientConnection.Comunicar(const ACommand: TRpDataFlashCommand;
  const AExecutorCallBack: IRpDataFlashExecutorCallBack): string;
begin
  if ACommand.SerializationFormat = sfAuto then
    ACommand.SerializationFormat := Self.SerializationFormat;

  Result := DoComunicarComThred(ACommand, EmptyStr, AExecutorCallBack);
end;

{ TRpDataFlashSyncLogEvent }

procedure TRpDataFlashSyncLogEvent.DoNotify;
begin
  inherited;
  if Assigned(FEvento) and (FLRDataFlashIP <> nil) and (FLog <> EmptyStr) then
    FEvento(FLRDataFlashIP, FTipoLog, FLog, FClientInfo);
end;

{ TRpDataFlashSyncConexaoEvent }

procedure TRpDataFlashSyncConexaoEvent.DoNotify;
begin
  inherited;
  if Assigned(FEvento) and (FLRDataFlashIP <> nil) and (FConexaoItem <> nil) then
    FEvento(FLRDataFlashIP, FConexaoItem);
end;

{ TRpDataFlashThreadInternalAdapter }

function TRpDataFlashThreadInternalAdapter.DoInternalConectar : Boolean;
begin
  Result := InternalConectar;
end;

{ TRpDataFlashComandController }

constructor TRpDataFlashCommandController.Create(AOwner: TComponent);
begin
  inherited;
  FComandos := GetComandoClass.Create(Self, GetComandoItemClass);
  FGrupo := C_WITHOUT_GROUP;
end;

destructor TRpDataFlashCommandController.Destroy;
begin
  FreeAndNil(FComandos);
  inherited;
end;

procedure TRpDataFlashCommandController.EditarComandos;
var
  lView: TfrmComandosControllerView;
begin
  lView := TfrmComandosControllerView.Create(nil);
  lView.Commands := Self.Commands;
  lView.Show;
end;

function TRpDataFlashCommandController.GetComandoClass: TRpDataFlashCommandListClass;
begin
  Result := TRpDataFlashCommandList;
end;

function TRpDataFlashCommandController.GetComandoItemClass: TRpDataFlashCommandItemClass;
begin
  Result := TRpDataFlashCommandItem;
end;

function TRpDataFlashCommandController.GetCommands: TRpDataFlashCommandList;
begin
  Result := FComandos;
end;

function TRpDataFlashCommandController.GetGroupName: string;
begin
  Result := FGrupo;
end;

procedure TRpDataFlashCommandController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FServer) and (Operation = opRemove) then
    FServer := nil;
end;

procedure TRpDataFlashCommandController.SetCommands(const Value: TRpDataFlashCommandList);
begin
  FComandos.Assign(Value);
end;

procedure TRpDataFlashCommandController.SetGroupName(const Value: string);
begin
  FGrupo := TRpDataFlashValidations.NameValidation(Value);
  if FGrupo = EmptyStr then
    FGrupo := C_WITHOUT_GROUP;
end;

procedure TRpDataFlashCommandController.SetServer(const Value: TRpDataFlashServerConnection);
begin
  if (FServer <> nil) then
    FServer.Controllers.Remove(Self);

  FServer := Value;

  if (FServer <> nil) and (FServer.Controllers.IndexOf(Self) = -1) then
    FServer.Controllers.Add(Self);
end;

{ TThreadCallback }

constructor TThreadCallback.Create(const AClient : TRpDataFlashCustomClientConnection; const ACommand: TRpDataFlashCommand;
  const ACallBackClassName: string; const AExecutorCallBack: IRpDataFlashExecutorCallBack);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FResult := EmptyStr;

//  if AClient is TRpDataFlashConexaoCliente then
//    FConexaoClient := TRpDataFlashConexaoCliente.Create(nil)
//  else
//    if AClient is TRpDataFlashClienteREST then
//      FConexaoClient := TRpDataFlashClienteREST.Create(nil);

  FConexaoClient := AClient.Create(nil);

  FConexaoClient.ClonarDe( AClient );
  FComando           := ACommand;
  FCallbackClassName := ACallBackClassName;
  FExecutorCallBack  := AExecutorCallBack
end;

procedure TThreadCallback.DoTerminate;
begin
  inherited;
  if Assigned(FConexaoClient) then
  begin
    FConexaoClient.Disconnect;
    FreeAndNil(FConexaoClient);
  end;
  Terminate;
end;

procedure TThreadCallback.Execute;
begin
  inherited;
//  if FExecutorCallBack <> nil then
//    raise Exception.Create('Not implemented... try again later !');
  CoInitialize(nil);

  if FCallbackClassName <> EmptyStr then
    FConexaoClient.ExecutorCallBackClass := FCallbackClassName
  else
    if Assigned(FExecutorCallBack) then
      FConexaoClient.ExecutorCallBackInterfaced := FExecutorCallBack
    else
      raise Exception.Create('Retorno de Callback sem controlador definido.');

  FResult := FConexaoClient.Comunicar(FComando, FComando.Params);
//  while not IsTerminated do
//  begin
//    Sleep( C_THRED_CALLBACK_INTERVAL );
//    Application.ProcessMessages;
//  end;
  DoTerminate;

  CoUninitialize;
end;

function TThreadCallback.GetIsTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

function TThreadCallback.GetResult: string;
begin
  Result := FResult;
end;

procedure TThreadCallback.SetCallbackClassName(const Value: string);
begin
  FCallbackClassName := Trim( Value );
end;

{ TRpDataFlashExecutorCallBack }

function TRpDataFlashExecutorCallBack.AsyncMode: Boolean;
begin
  Result := True;
end;

function TRpDataFlashExecutorCallBack.ExecutarCallBack(const AParametrosCallback: TRpDataFlashCommandParameterList): Boolean;
begin
  Result := DoBeforeCallback(AParametrosCallback);
  if Result then
  begin
    if AsyncMode then
      TThread.Queue(nil, InternalCallback)
    else
      InternalCallback;
  end;
end;

procedure TRpDataFlashExecutorCallBack.InternalCallback;
begin
  DoAfterCallback;
//  if AsyncMode then
//    Application.ProcessMessages;
end;

{ TRpDataFlashComandControllerList }

function TRpDataFlashCommandControllerList.Localizar(const ANome: string;
  out AObjComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lEnum: TListEnumerator;
  lController: TRpDataFlashCommandController;
begin
  lEnum := GetEnumerator;
  try
    AObjComando := nil;
    Result := False;
    while (lEnum.MoveNext) and (not Result) do
    begin
      lController := TRpDataFlashCommandController(lEnum.Current);
      Result := lController.Commands.Localizar(ANome, AObjComando);
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

{ TRpDataFlashComandHelper }

function TRpDataFlashComandHelper.GetServer: TRpDataFlashServerConnection;
begin
  Result := TRpDataFlashServerConnection(Server);
end;

{ TRpDataFlashProviderControllerList }

function TRpDataFlashProviderControllerList.Localizar(const ANome: string;
  out AObjComando: IRpDataFlashCommandInterfaced): Boolean;
var
  lEnum: TListEnumerator;
  lItem: TRpDataFlashCustomDataSetProvider;
begin
  AObjComando := nil;
  Result := False;

  lEnum := GetEnumerator;
  try
    while (lEnum.MoveNext) and (not Result) do
    begin
      lItem := TRpDataFlashCustomDataSetProvider(lEnum.Current);
      if lItem.Name = ANome then
      begin
        AObjComando := TRpDataFlashDataSetCommandProvider.Create(lItem);
        Result := True;
      end;
    end;
  finally
    FreeAndNil(lEnum);
  end;
end;

{ TRpDataFlashSyncConexaoEventNew }

procedure TRpDataFlashSyncConexaoEventNew.DoNotify;
begin
  inherited;
  if Assigned(FEvento) and (FLRDataFlashIP <> nil) then
    FEvento(FLRDataFlashIP, FExecutor);
end;

{ TCustomProxyClient }

constructor TCustomProxyClient.Create(ATcpClient: TRpDataFlashCustomClientConnection;
  ABusyCallback : TRpDataFlashBusyCallback; const ASharedClient : Boolean);
begin
  FSharedClient := ASharedClient;
  FClient := ATcpClient;
  FLastError := EmptyStr;
  FBusyCallback := ABusyCallback;
end;

procedure TCustomProxyClient.DoAoProcessarErroComunicacao;
begin
// dummy
end;

procedure TCustomProxyClient.DoComunicar(const ACommand: TRpDataFlashSendCommand);
begin
  ACommand.SerializationFormat := FSerializationFormat;
  if Assigned(FClient) then
    FClient.Comunicar(ACommand)
  else
    raise Exception.Create('Proxy não possui configuração cliente informada.');
end;

function TCustomProxyClient.DoEnviar(const ANomeComando: string;
  const AParams: TRpDataFlashCommandParameterList): Boolean;
var
  lCmd: TRpDataFlashSendCommand;
begin
  FBusyCallback(True);
  FLastError := EmptyStr;
  FStatusProcessamento := psNone;
  lCmd := TRpDataFlashSendCommand.Create;
  try
    lCmd.SetCommand( ANomeComando );
    lCmd.Params.Assign(AParams);
    DoComunicar(lCmd);
    AParams.Assign(lCmd.Params);
    Result := lCmd.ReturnStatus;
    FStatusProcessamento := lCmd.ProcessingStatus;
    if not Result then
      FLastError := lCmd.LastError;
  except
    on E:Exception do
    begin
      Result := False;
      ProcessaErroComunicacao(E.Message);
    end;
  end;
  FreeAndNil(lCmd);
  SetEvents(nil);
  FBusyCallback(False);
end;

function TCustomProxyClient.GetLastError: string;
begin
  Result := FLastError;
  if Pos('Sem conex', Result) > 0 then
    Result := 'Sem conexão com o serviço.';
  if Trim(Result) <> '' then
    Result := 'Erro de retorno. ' + Result;
end;

function TCustomProxyClient.GetStatusProcessamento: TRpDataFlashProcessingStatus;
begin
  Result := FStatusProcessamento;
end;

procedure TCustomProxyClient.ProcessaErroComunicacao(const pMessage: string);
var
  lTentativas: Integer;
begin
  FLastError := pMessage;
  if (Pos('sem conex', LowerCase(FLastError)) > 0)
  or (Pos('connection closed gracefully', LowerCase(FLastError)) > 0) then
  begin
    lTentativas := 0;
    repeat
      if Assigned(FClient) and (FClient is TRpDataFlashClientConnection) and (not FClient.LazyConnection) then
      begin
        Inc(lTentativas);
        DoAoProcessarErroComunicacao;
        Sleep(2000);
        FClient.Connect;
        if FClient.Connected then
        begin
          lTentativas := 5;
          FLastError := 'A conexão com o serviço teve que ser restabelecida.';
        end;
      end
      else
        lTentativas := 5;
    until lTentativas > 3;
  end;
end;

procedure TCustomProxyClient.SetEvents(const AEventoStatus: TRpDataFlashOnStatus);
begin
  FEventoStatus := AEventoStatus;
  if (FClient <> nil) and (not FSharedClient) then
    FClient.OnStatus := FEventoStatus;
end;

{ TRpDataFlashConexaoCliente }

function TRpDataFlashClientConnection.DoComunicarComThred(
  const ACommand: TRpDataFlashCommand; const ACallBackClassName: string;
  const AExecutorCallBack: IRpDataFlashExecutorCallBack): string;
var
  lClient : TRpDataFlashClientConnection;
begin
  lClient := TRpDataFlashClientConnection.Create(Self);
  lClient.ClonarDe(Self);

  try
    if ACallBackClassName <> EmptyStr then
      lClient.ExecutorCallBackClass := ACallBackClassName
    else
      if Assigned(AExecutorCallBack) then
        lClient.ExecutorCallBackInterfaced := AExecutorCallBack
      else
        raise Exception.Create('Retorno de Callback sem controlador definido.');

    Result := lClient.Comunicar(ACommand, ACommand.Params);
  finally
    if Assigned(lClient) then
    begin
      lClient.Disconnect;
      FreeAndNil(lClient);
    end;
  end;

{$REGION 'THREAD'}
{  // cria um novo client
  lThCallback := TThreadCallback.Create(Self);
  lThCallback.Comando := AComando;
  lThCallback.CallbackClassName := ACallBackClassName;
  lThCallback.ExecutorCallBack := AExecutorCallBack;
  lThCallback.Resume;

//  WaitForSingleObject(lThCallback.Handle, INFINITE);
  while not lThCallback.IsTerminated do
  begin
    Sleep( 100 );
//    Application.ProcessMessages;
  end;

  Result := lThCallback.GetResult;
  FreeAndNil(lThCallback);
}
{$ENDREGION}
end;

procedure TRpDataFlashClientConnection.DoConnect;
begin
  inherited;
  if PodeConectar then
  begin
    if FLazyConnection  then
    begin
      FreeAndNil(FThreadConexao);

      FThreadConexao := TRpDataFlashThreadConnection.Create;
      FThreadConexao.Conexao := Self;
      {$IFDEF UNICODE}
      FThreadConexao.Start;
      {$ELSE}
      FThreadConexao.Resume;
      {$ENDIF}

      if FConnectionTimeOut <= 0 then
        Sleep(1000)
      else
        Sleep(FConnectionTimeOut)
    end
    else
      InternalConectar;
  end;
end;

procedure TRpDataFlashClientConnection.Finalizar;
begin
  inherited;
  FThreadConexao.Terminate;

  while FThreadConexao.Conectando do
    Sleep(200);

  while not FThreadConexao.IsTerminated do
    Sleep(200);

//  FThreadConexao.Suspend;
  FreeAndNil(FThreadConexao);
end;

function TRpDataFlashClientConnection.GetConnectionHelperClass: TRpDataFlashConnectionHelperCustomClass;
begin
  Result := TRpDataFlashConnectionHelperTCP;
end;

procedure TRpDataFlashClientConnection.Inicializar;
begin
  inherited;
  FThreadConexao := TRpDataFlashThreadConnection.Create;
  FThreadConexao.Conexao := Self;
end;

procedure TRpDataFlashClientConnection.SetServidor(const Value: string);
var
  lPos: Integer;
  lPorta: string;
begin
  inherited;
  // se no nome do servidor tiver a porta, separa as propriedades
  // LOCALHOST:8890

  lPos := Pos(':', FServer);

  if lPos > 0 then
  begin
    lPorta := Copy(FServer, lPos + 1, Length(FServer) );
    Delete(FServer, lPos, Length(FServer) );
    FConfigTCPIP.Port := StrToIntDef( lPorta, FConfigTCPIP.Port );
  end;
end;

{ TRpDataFlashConexaoREST }

constructor TRpDataFlashRESTClient.Create(AOwner: TComponent);
begin
  inherited;
  Port := 9088;
  FLoginPrompt := False;
  FCommunicationType := ctText;
end;

function TRpDataFlashRESTClient.DoComunicarComThred(
  const ACommand: TRpDataFlashCommand; const ACallBackClassName: string;
  const AExecutorCallBack: IRpDataFlashExecutorCallBack): string;
var
  lClient : TRpDataFlashRESTClient;
begin
  lClient := TRpDataFlashRESTClient.Create(Self);
  lClient.ClonarDe(Self);

  try
    if ACallBackClassName <> EmptyStr then
      lClient.ExecutorCallBackClass := ACallBackClassName
    else
      if Assigned(AExecutorCallBack) then
        lClient.ExecutorCallBackInterfaced := AExecutorCallBack
      else
        raise Exception.Create('Retorno de Callback sem controlador definido.');

    Result := lClient.Comunicar(ACommand, ACommand.Params);
  finally
    if Assigned(lClient) then
    begin
      lClient.Disconnect;
      FreeAndNil(lClient);
    end;
  end;

{$REGION 'THREAD'}
{  // cria um novo client
  lThCallback := TThreadCallback.Create(Self);
  lThCallback.Comando := AComando;
  lThCallback.CallbackClassName := ACallBackClassName;
  lThCallback.ExecutorCallBack := AExecutorCallBack;
  lThCallback.Resume;

//  WaitForSingleObject(lThCallback.Handle, INFINITE);
  while not lThCallback.IsTerminated do
  begin
    Sleep( 100 );
//    Application.ProcessMessages;
  end;

  Result := lThCallback.GetResult;
  FreeAndNil(lThCallback);
}
{$ENDREGION}
end;

function TRpDataFlashRESTClient.Enviar(const AIdentificador, AMenssage, ANomeComando: string): string;
var
  lResultado : TStringStream;
  lIdentificadorEnviado: string;
  lProtocolo: TRpDataFlashProtocol;
begin
  Result := EmptyStr;
  lResultado := nil;
  try
    if not Assigned(FConnectionHelper.Conector) then
      InternalConectar;

    lProtocolo := TRpDataFlashProtocol.Create(Self.EncryptionType);
    try
      lProtocolo.Identifier := AIdentificador;
      lProtocolo.Message := AMenssage;

      lIdentificadorEnviado := lProtocolo.Identifier;

      InternalEnviar(
        TRpDataFlashConnectionHelperREST(FConnectionHelper).Conector,
        lProtocolo.Mount,
        ANomeComando,
        lResultado);

      lResultado.Position := 0;

      lProtocolo.Clear;
      lProtocolo.Message := lResultado.DataString;

      TentaGerarException(lProtocolo);

      TentaExecutarCallBack(lProtocolo);

      if AIdentificador <> lProtocolo.Identifier then
        raise ERpDataFlashSending.Create('A mensagem recebida não é a esperada' + sLineBreak + lProtocolo.Message);

      Result := lProtocolo.Message;
    finally
      FreeAndNil(lResultado);
      FreeAndNil(lProtocolo);
    end;
  except
    on E:Exception Do
    begin
      raise Exception.Create('Erro enviando solicitação REST: ' + E.Message);
    end;
  end;
end;

function TRpDataFlashRESTClient.GetConnectionHelperClass: TRpDataFlashConnectionHelperCustomClass;
begin
  Result := TRpDataFlashConnectionHelperREST;
end;

function TRpDataFlashRESTClient.GetPort: Integer;
begin
  Result := FConfigREST.Port;
end;

procedure TRpDataFlashRESTClient.InternalEnviar(const AHandler: TIdHTTP;
  const AValor, ANomeComando: string; out AResponse: TStringStream);
  procedure EnviarComoTexto;
  var
    lQuebra: TRpDataFlashProtocolBreaker;
    lPutParams : TStringStream;
  begin
    lQuebra := TRpDataFlashProtocolBreaker.Create;
    lPutParams := nil;
    try
      lQuebra.OnStatus := FOnStatus;
      lQuebra.AddValue(AValor);

      if AResponse = nil then
        AResponse := TStringStream.Create('');
      lPutParams := TStringStream.Create(AValor);

      AHandler.Post(FConnectionHelper.URL + '/' + ANomeComando, lPutParams, AResponse);
    finally
      if lPutParams <> nil then
        FreeAndNil(lPutParams);
      FreeAndNil(lQuebra);
    end;
  end;

begin
  NewLog(slOnSend, AValor, '');

  case FCommunicationType of
    ctText,
    ctChar : EnviarComoTexto;
    ctStream,
    ctCompressedStream : raise ERpDataFlashException.Create('Conexões REST não permitem envios de stream.');
  end;
end;

procedure TRpDataFlashRESTClient.SetPort(const Value: Integer);
begin
  FConfigREST.Port := Value;
end;

procedure TRpDataFlashRESTClient.SetServidor(const Value: string);
var
  lPos: Integer;
  lPorta: string;
begin
  inherited;
  // se no nome do servidor tiver a porta, separa as propriedades
  // LOCALHOST:8890

  lPos := Pos(':', FServer);

  if lPos > 0 then
  begin
    lPorta := Copy(FServer, lPos + 1, Length(FServer) );
    Delete(FServer, lPos, Length(FServer) );
    FConfigREST.Port := StrToIntDef( lPorta, FConfigREST.Port );
  end;
end;

{ TRpDataFlashConfigConexaoREST }

constructor TRpDataFlashConfigConexaoREST.Create;
begin
  inherited;
  FEnabled := False;
  FPort := 9088;
end;

{ TRpDataFlashConfigConexaoTCPIP }

constructor TRpDataFlashConfigConexaoTCPIP.Create;
begin
  inherited;
  FPort := 8890;
  FEnabled := True;
end;

{ TRpDataFlashCustomConfigConexao }

constructor TRpDataFlashCustomConfigConexao.Create;
begin
  inherited Create;
end;

{ TRpDataFlashFileTransfer }

constructor TRpDataFlashFileTransfer.Create;

  function GetTempDirectory: String;
  var
    tempFolder: array[0..MAX_PATH] of Char;
  begin
    GetTempPath(MAX_PATH, @tempFolder);
    Result := IncludeTrailingPathDelimiter(StrPas(tempFolder));
  end;

begin
  FPort := 8891;
  FTempDir := GetTempDirectory + 'tcpFileTransf\';
  ForceDirectories(FTempDir);
  FEnabled := True;
end;

function TRpDataFlashFileTransfer.GetDataPort: Integer;
begin
  Result := FPort + 1;
end;

end.

