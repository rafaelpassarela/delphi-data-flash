unit uRpDataFlash.Command;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, SysUtils, Contnrs, XMLIntf, XMLDoc, DB, uRpDataFlash.Types, IdContext,
  Variants, ActiveX, StrUtils, uRpAlgorithms, uRpJsonBase, uRpDataFlash.ConvertUtils,
  uRpSerialization, Windows, IdCustomHttpServer, uRpResourceString,
  uRpDataFlash.DataSet.Params;

type
  TRpDataFlashCommandParameterList = class;
  TRpDataFlashCommand = class;
  TRpDataFlashCommandClass = class of TRpDataFlashCommand;
  TRpDataFlashAbstractClass = class of TObject;
  TRpFileProxy = class;

  IRpPackageCommandExecutor = interface
    ['{2C11C1CC-758E-49CE-AD1B-563535B0835B}']
    function GetName : string;
    function GetDataComponent : TComponent;
    function GetClassName : string;
    function AsObject : TObject;
    procedure ConfigDataComponent(const AUser, APassword : string);
    procedure DisconnectDataComponent;
    // DataSet Events
    function Commit(const ARetaining : Boolean) : Boolean;
    function Rollback(const ARetaining : Boolean) : Boolean;
    function StartTransaction : Boolean;
    function Select(const ASelectSQL: string; out XMLData: string): Boolean; overload;
    function Select(const ADataSetParams: TRpDataFlashDataSetParams; out XMLData: string): Boolean; overload;
    function ExecuteSQL(const ASQL: string): Boolean; overload;
    function ExecuteSQL(const ADataSetParams: TRpDataFlashDataSetParams): Boolean; overload;
  end;

  TRpDataFlashCallBackEvent = function(const AContext: TIdContext; const AMessage : string) : Boolean of object;

  IServerInstanceController = interface;
  ISessionInstanceController = interface;

  IAutenticationProvider = interface
    ['{4548735C-B80B-4A8E-B941-22B0111A40FC}']
    procedure SetUserName(const Value : string);
    function GetUserName : string;
    procedure SetPassword(const Value : string);
    function GetPassword : string;
    procedure SetAuthenticated(const Value : Boolean);
    function GetAuthenticated : Boolean;

    function Ip : string;
    property UserName : string read GetUserName write SetUserName;
    property Password : string read GetPassword write SetPassword;
    property Authenticated : Boolean read GetAuthenticated write SetAuthenticated;
  end;

  IRpDataFlashCommandInterfaced = interface
    ['{4EE3E304-8FAE-42C8-9830-488C67BC3135}']
    function GetProcessType : TRpDataFlashProcessType;
    function GetLifeCycle : TRpDataFlashLifeCycle;
    procedure SetLifeCycle(const Value : TRpDataFlashLifeCycle);
    function GetSessionInstanceController: ISessionInstanceController;
    procedure SetServerInstanceController(const Value: IServerInstanceController);
    function GetServerInstanceController: IServerInstanceController;
    procedure SetSessionInstanceController(const Value: ISessionInstanceController);

    function Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor) : string;
    function GetCommand : string;
    function GetParams: TRpDataFlashCommandParameterList;
    function GetDescription: string;
    procedure DoRegisterParams(const AParams : TRpDataFlashCommandParameterList);
    procedure DoLoad(const ALoadType : TRpDataFlashLoadType; const AParams : TRpDataFlashCommandParameterList);
    procedure DoSerialize(const AParams : TRpDataFlashCommandParameterList);
    procedure DoExecutionError(const AParams : TRpDataFlashCommandParameterList);
    function DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean;
    function SendCallBack : Boolean;

    function ExecuteBridgeError(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteBridgeSuccessfully(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteBeforeBridgeConnection(const AParams: TRpDataFlashCommandParameterList;
      AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;

    property ProcessType : TRpDataFlashProcessType read GetProcessType;
    property LifeCycle : TRpDataFlashLifeCycle read GetLifeCycle write SetLifeCycle;
    property ServerInstanceController : IServerInstanceController read GetServerInstanceController write SetServerInstanceController;
    property SessionInstanceController : ISessionInstanceController read GetSessionInstanceController write SetSessionInstanceController;

    function GetSerializationFormat : TSerializationFormat;
    procedure SetSerializationFormat(const ASerializationFormat : TSerializationFormat);
    property SerializationFormat : TSerializationFormat read GetSerializationFormat write SetSerializationFormat;

    property Command : string read GetCommand;
    function ReturnStatus : Boolean;
    function LastError : string;

    procedure SetCallBackEvent(const AContext: TIdContext; const ACallBackEvent : TRpDataFlashCallBackEvent);
    procedure SetServer(const AServer: TComponent);
    procedure SetConnectionItem(const AConnectionItem: IAutenticationProvider);

    function GetObject : TObject;

    procedure SetExecutor(const AExecutor : IRpPackageCommandExecutor);
    function GetExecutor : IRpPackageCommandExecutor;
    property Executor : IRpPackageCommandExecutor read GetExecutor write SetExecutor;

    function GetLock : Boolean;
    procedure SetLock(const Value : Boolean);
    property Lock : Boolean read GetLock write SetLock;

    function GetRequestInfo : TIdHTTPRequestInfo;
    procedure SetRequestInfo(const AValue : TIdHTTPRequestInfo);
    property RequestInfo  : TIdHTTPRequestInfo read GetRequestInfo write SetRequestInfo;

    function GetResponseInfo : TIdHTTPResponseInfo;
    procedure SetResponseInfo(const AValue : TIdHTTPResponseInfo);
    property ResponseInfo : TIdHTTPResponseInfo read GetResponseInfo write SetResponseInfo;

    function GetResponseFileName : string;
  end;

  IInstanceController = interface
    ['{6E22D354-7991-47BA-8331-8554582DC1C4}']
    function FindInstance(const ACommand : string) : IRpDataFlashCommandInterfaced;
    procedure AddInstance(const AInstance : IRpDataFlashCommandInterfaced);
  end;

  IServerInstanceController = interface(IInstanceController)
    ['{94926847-8656-4CCE-99C7-EA93577EBD49}']
  end;

  ISessionInstanceController = interface(IInstanceController)
    ['{C2D0EF02-C7A9-4C4E-A7A9-8E9D7CD4D1E7}']
  end;

  TRpDataFlashCommandParameter = class(TPersistent)
  private
    FValue: Variant;
    FName: string;
    FOwner: TRpDataFlashCommandParameterList;
    FParamType: TRpDataFlashParamType;
    FValueType: TRpDataFlashParamValueType;
    FFile: TRpFileProxy;
    FBaseClass: string;
    function GetIsEmpty: Boolean;
    function GetAsString: string;
    function GetAsFloat: Double;
    function GetAsBoolean: Boolean;
    function GetAsBase64: string;
    function GetAsInteger: Integer;
    function GetAsVariant: string;
    function GetAsDateTime: TDateTime;
    function GetAsJSONString: string;
    function GetAsBinaryFile: TRpFileProxy;
    function GetAsObject : TBaseSerializableObject;
    procedure SetAsString(const Value: string);
    procedure SetAsFloat(const Value: Double);
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsBase64(const Value: string);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsVariant(const Value: string);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsJSONString(const Value: string);
  public
    constructor Create; overload; virtual;
    constructor Create(const AOwner : TRpDataFlashCommandParameterList;
      const AName : string;
      const AValue : Variant;
      const AParamType : TRpDataFlashParamType;
      const AValueType : TRpDataFlashParamValueType;
      const ABaseClass : string = ''); overload;
    destructor Destroy; override;

    property Owner : TRpDataFlashCommandParameterList read FOwner;
    property Name : string read FName;
    property Value : Variant read FValue;
    property ParamType : TRpDataFlashParamType read FParamType;
    property ValueType : TRpDataFlashParamValueType read FValueType;
    property BaseClass : string read FBaseClass write FBaseClass;

    property AsVariant : string read GetAsVariant write SetAsVariant;
    property AsString : string read GetAsString write SetAsString;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    property AsBase64 : string read GetAsBase64 write SetAsBase64;
    property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    property AsJSONString : string read GetAsJSONString write SetAsJSONString;
    property AsBinaryFile: TRpFileProxy read GetAsBinaryFile;
    property AsObject : TBaseSerializableObject read GetAsObject;
    property IsEmpty : Boolean read GetIsEmpty;

    procedure LoadFromFile(const AArquivo : string);
    procedure SaveToFile(const AArquivo : string);

    procedure ToNode(const ANode : IXMLNode);
    procedure FromNode(const ANode : IXMLNode);
    function ToJson : string;
    function ToApplicationJson : string;

    procedure Assign(const Source: TRpDataFlashCommandParameter); reintroduce;
  end;

  TRpDataFlashCommandParameterSetter = class(TRpDataFlashCommandParameter)
  public
    procedure SetValueType(const AValueType : TRpDataFlashParamValueType);
  end;

  TRpDataFlashParamValue = class(TRpDataFlashCommandParameter)
  end;

  TRpDataFlashParamValueClass = class of TRpDataFlashParamValue;

  TRpDataFlashParamValueInteger = class(TRpDataFlashParamValue)
  private
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  public
    constructor Create; override;
    property Value : Integer read GetValue write SetValue;
  end;

  TRpDataFlashParamValueString = class(TRpDataFlashParamValue)
  private
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    constructor Create; override;
    property Value : String read GetValue write SetValue;
  end;

  TRpDataFlashParamValueBoolean = class(TRpDataFlashParamValue)
  private
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
  public
    constructor Create; override;
    property Value : Boolean read GetValue write SetValue;
  end;

  TRpDataFlashCommandParameterList = class(TPersistent)
  private
    FParamList : TObjectList;
    FCommand: string;
    FSerializationFormat: TSerializationFormat;
    FFileTransferSupport: IRpDataFlashFileTransferSupport;
    FContentType: string;
    function GetParamByName(const AName: string; const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
    function GetParamByIdx(const Index: Integer) : TRpDataFlashCommandParameter;
    function GetProcessingStatus: TRpDataFlashProcessingStatus;
    procedure SetProcessingStatus(const Value: TRpDataFlashProcessingStatus);
    function GetCount : Integer;
  protected
    procedure ToNode(const ANode : IXMLNode); virtual;
    procedure FromNode(const ANode : IXMLNode); virtual;
    procedure FromJson(const AJsonPair : TJSONPair); virtual;
    function ToJson : string;
    function ToApplicationJson : string;
  public
    constructor Create(const AFileTransferSupport : IRpDataFlashFileTransferSupport); virtual;
    destructor Destroy; override;
    procedure Assign(const Source: TRpDataFlashCommandParameterList); reintroduce;

    procedure AddNew(const AParam : TRpDataFlashCommandParameter); overload;
    function AddNew(const AName : string; AValue : Variant; const AParamType : TRpDataFlashParamType;
      const AValueType : TRpDataFlashParamValueType; const ABaseClassName : string = '') : TRpDataFlashCommandParameter; overload;
    function AddNew(const AName: string; const AParamType : TRpDataFlashParamType;
      const AValueType : TRpDataFlashParamValueClass) : TRpDataFlashParamValue; overload;
    function AddNew(const AName: string; const AParamType : TRpDataFlashParamType;
      const AClasseBase : string) : TRpDataFlashCommandParameter; overload;

    procedure AddParam(const AName : string; const AValue : Variant; const AParamType : TRpDataFlashParamValueType);
    procedure AddResult(const AName : string; AValue : Variant; const AParamType : TRpDataFlashParamValueType);

    function FindByName(const AName : string) : TRpDataFlashCommandParameter; overload;
    function FindByName(const AName : string; const AParamType : TRpDataFlashParamType) : TRpDataFlashCommandParameter; overload;

    function Serialize : string;
    procedure Load(const AParams : string);
    procedure ParseJSON(const AJSONString : string);

    property InternalParam[const AName : string] : TRpDataFlashCommandParameter index tpInternal read GetParamByName;
    property Param[const AName : string] : TRpDataFlashCommandParameter index tpInput read GetParamByName;
    property ResultParam[const AName : string] : TRpDataFlashCommandParameter index tpOutput read GetParamByName;
    property Item[const Index : Integer] : TRpDataFlashCommandParameter read GetParamByIdx; default;

    property Command : string read FCommand write FCommand;
    property ProcessingStatus : TRpDataFlashProcessingStatus read GetProcessingStatus write SetProcessingStatus;
    property Count : Integer read GetCount;
    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat;
    property ContentType : string read FContentType write FContentType;
  end;

  TRpDataFlashCommand = class(TInterfacedObject, IRpDataFlashCommandInterfaced)
  private
    FParamList: TRpDataFlashCommandParameterList;
    FContext: TIdContext;
    FOnCallBackEvent: TRpDataFlashCallBackEvent;
    FLifeCycle: TRpDataFlashLifeCycle;
    FServerInstanceController: IServerInstanceController;
    FSessionInstanceController: ISessionInstanceController;
    FLock: Boolean;
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    function GetParamByName(const AName: string; const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
    function GetParams: TRpDataFlashCommandParameterList;
    function GetProcessingStatus: TRpDataFlashProcessingStatus;
    function GetDescription: string;
    procedure SetProcessingStatus(const Value: TRpDataFlashProcessingStatus);
    procedure DoLoad(const ALoadType : TRpDataFlashLoadType; const AParams : TRpDataFlashCommandParameterList); overload; virtual;
    procedure DoSerialize(const AParams : TRpDataFlashCommandParameterList); overload;
    procedure DoExecutionError(const AParams: TRpDataFlashCommandParameterList); overload;
    procedure DoRegisterParams(const AParams: TRpDataFlashCommandParameterList); overload;

    function Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor) : string; overload;
    function Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      const AParamType : TRpDataFlashExecutionType; var AContinue : Boolean) : string; overload; virtual;

    function ExecuteBridgeError(const AParams: TRpDataFlashCommandParameterList; AExecutor: IRpPackageCommandExecutor;
      var AContinue : Boolean): string; overload;
    function ExecuteBridgeSuccessfully(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteBeforeBridgeConnection(const AParams: TRpDataFlashCommandParameterList;
      AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;

    function GetSessionInstanceController: ISessionInstanceController;
    procedure SetServerInstanceController(const Value: IServerInstanceController);
    function GetServerInstanceController: IServerInstanceController;
    procedure SetSessionInstanceController(const Value: ISessionInstanceController);
    function GetLock : Boolean;
    procedure SetLock(const Value : Boolean);
    function GetSerializationFormat: TSerializationFormat;
    procedure SetSerializationFormat(const Value: TSerializationFormat);
    function GetExecutor : IRpPackageCommandExecutor;
    procedure SetExecutor(const AExecutor : IRpPackageCommandExecutor);
  protected
    FExecutor : IRpPackageCommandExecutor;
    FServer: TComponent;
    FConnectionItem: IAutenticationProvider;
    FSerealizationFormat: TSerializationFormat;

    function GetCommand: string; virtual;
    function DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean; virtual;
    function DoExecute : Boolean; virtual; abstract;
    function DoGetDescription: string; virtual;
    function FindParametro(const AName: string): TRpDataFlashCommandParameter;
    function FindResult(const AName: string): TRpDataFlashCommandParameter;
    function GetProcessType : TRpDataFlashProcessType; virtual;
    function InternalLoadCommand(const ACommand : TRpDataFlashCommandClass;
      out ACommandObject : IRpDataFlashCommandInterfaced;
      out AParams : TRpDataFlashCommandParameterList) : Boolean;
    function GetObject: TObject;
    function GetLifeCycle: TRpDataFlashLifeCycle; virtual;
    function GetResponseFileName: string; virtual;
    function DoGetSerializedParams : string; virtual;

    procedure DoSerialize; overload; virtual;
    procedure DoLoad; overload; virtual;
    procedure DoExecutionError(const AErrorMsg : string); overload; virtual;
    procedure DoValidateParams; virtual;
    procedure DoRegisterParams; overload; virtual;
    procedure DoExecuteBridgeError(var AContinue : Boolean); virtual;
    procedure DoExecuteBridgeSuccessfully(var AContinue : Boolean); virtual;
    procedure DoExecuteBeforeBridgeConnection(var AContinue : Boolean); virtual;
    procedure NewParam(const AName : string; const AParamType : TRpDataFlashParamValueType; const ARecarregar : Boolean = True); overload;
    procedure NewParam(const AName : string; const ABaseClass : TBaseSerializableObjectClass; const ARecarregar : Boolean = True); overload;
    procedure NewResult(const AName : string; const AParamType : TRpDataFlashParamValueType); overload;
    procedure NewResult(const AName : string; const ABaseClass : TBaseSerializableObjectClass; const ARecarregar : Boolean = True); overload;
    procedure NewInternalParam(const AName : string; const AParamType : TRpDataFlashParamValueType); overload;
    procedure SetLifeCycle(const ALifeCycle: TRpDataFlashLifeCycle);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetServer(const AServer: TComponent);

    procedure SetConnectionItem(const AConnectionItem: IAutenticationProvider);

    function GetRequestInfo: TIdHTTPRequestInfo;
    procedure SetRequestInfo(const AValue: TIdHTTPRequestInfo);
    function GetResponseInfo: TIdHTTPResponseInfo;
    procedure SetResponseInfo(const AValue: TIdHTTPResponseInfo);

    function SendCallBack: Boolean;
    procedure SetCallBackEvent(const AContext: TIdContext;const ACallBackEvent: TRpDataFlashCallBackEvent);
    function Serialize : string;
    procedure Load(const ACommand : string);

    function ReturnStatus : Boolean;
    function LastError : string;

    property Command: string read GetCommand;
    property Params : TRpDataFlashCommandParameterList read GetParams;
    property Param[const AName : string] : TRpDataFlashCommandParameter index tpInput read GetParamByName;
    property ResultParam[const AName : string] : TRpDataFlashCommandParameter index tpOutput read GetParamByName;
    property ProcessType : TRpDataFlashProcessType read GetProcessType;
    property ProcessingStatus : TRpDataFlashProcessingStatus read GetProcessingStatus write SetProcessingStatus;
    property Executor : IRpPackageCommandExecutor read GetExecutor write SetExecutor;
    property Server : TComponent read FServer;
    property ServerInstanceController : IServerInstanceController read GetServerInstanceController write SetServerInstanceController;
    property SessionInstanceController : ISessionInstanceController read GetSessionInstanceController write SetSessionInstanceController;

    property LifeCycle : TRpDataFlashLifeCycle read GetLifeCycle write SetLifeCycle;
    property Lock : Boolean read GetLock write SetLock;
    property SerializationFormat : TSerializationFormat read GetSerializationFormat write SetSerializationFormat;
    property RequestInfo  : TIdHTTPRequestInfo read GetRequestInfo write SetRequestInfo;
    property ResponseInfo : TIdHTTPResponseInfo read GetResponseInfo write SetResponseInfo;

    class function LoadCommand(
      const ACommand : string;
      out ACommandObject : IRpDataFlashCommandInterfaced;
      out AParams : TRpDataFlashCommandParameterList;
      const AServerInstanceController : IServerInstanceController = nil;
      const ASessionInstanceController :ISessionInstanceController = nil;
      const ALoadParams : Boolean = True) : Boolean; overload;

    class function LoadCommand(
      const ACommandClass : string;
      out ACommandObject : IRpDataFlashCommandInterfaced;
      out ALifeCycle: TRpDataFlashLifeCycle;
      const AServerInstanceController : IServerInstanceController;
      const ASessionInstanceController : ISessionInstanceController;
      const ASerializationFormat : TSerializationFormat = sfAuto) : Boolean; overload;
  end;

  TRpDataFlashSendCommand = class(TRpDataFlashCommand)
  private
    FCommand : string;
  protected
    function DoExecute : Boolean; override;
  public
    function GetCommand: string; override;
    procedure SetCommand(const ACommand : string); overload;
    procedure SetCommand(const ACommand : TRpDataFlashCommandClass); overload;
  end;

  TRpDataFlashBrowserCommand = class(TRpDataFlashCommand)
  protected
    function GetBrowserPage : string; virtual;
    function DoGetSerializedParams : string; override;
  end;

  TRpDataFlashTextCommand = class(TRpDataFlashCommand)
  private
    FOnExecuteMessage: TRpDataFlashOnExecuteMessage;
  protected
    function DoExecute : Boolean; override;
  public
    property OnExecuteMessage : TRpDataFlashOnExecuteMessage read FOnExecuteMessage write FOnExecuteMessage;
  end;

  IRpFileProxy = interface
  ['{1F83A425-C29D-486D-9D39-7FAC3FF83EE6}']
    function Save : string;
    function Load(const AFileStream : string) : string;

    function SaveToFile(const AFileName :string) : Boolean; overload;
    function SaveToFile : Boolean; overload;
    function LoadFromFile(const AFileName : string) : Boolean;

    function Get(const ASupport: IRpDataFlashFileTransferSupport;
      const AFileID : string = ''): Boolean;
    function Put(const ASupport : IRpDataFlashFileTransferSupport) : Boolean;
    function Remove : Boolean;

    function GetFileName : string;
    procedure SetFileName(const AValue : string);
    property FileName : string read GetFileName write SetFileName;

    function GetFileID : string;
    procedure SetFileID(const AValue : string);
    property FileID : string read GetFileID write SetFileID;

    function GetFileSize : Int64;
    property FileSize : Int64 read GetFileSize;
    function FileSizeFmt : string;

    function GetDeleteOnRecive : Boolean;
    procedure SetDeleteOnRecive(const AValue : Boolean);
    property DeleteOnRecive : Boolean read GetDeleteOnRecive write SetDeleteOnRecive;

    function GetStream : TStream;
    property Stream : TStream read GetStream;

    function DecodeInfo(const AInfo : string) : TRpDataFlashFtpFileInfo;
    procedure CopyInfo(const ASource : IRpFileProxy);
  end;

  TRpFileProxy = class(TInterfacedPersistent, IRpFileProxy)
  private
    FFileStream: TStringStream;
    FFileName: string;
    FFileID: string;
    FDeleteOnRecive : Boolean;
    function GetFileName : string;
    procedure SetFileName(const AValue : string);
    function GetFileID : string;
    procedure SetFileID(const AValue : string);
    function GetFileSize : Int64;
    function DoGenerateFileID : string;
    function GetDeleteOnRecive : Boolean;
    procedure SetDeleteOnRecive(const AValue : Boolean);
    function GetStream : TStream;
  public
    constructor Create;
    destructor Destroy; override;

    function Save : string;
    function Load(const AFileStream: string) : string;

    function SaveToFile(const AFileName :string) : Boolean; overload;
    function SaveToFile : Boolean; overload;
    function LoadFromFile(const AFileName : string) : Boolean;

    function Get(const ASupport: IRpDataFlashFileTransferSupport; const AFileID : string = ''): Boolean;
    function Put(const ASupport: IRpDataFlashFileTransferSupport): Boolean;
    function Remove : Boolean;
    function DecodeInfo(const AInfo : string) : TRpDataFlashFtpFileInfo;
    procedure CopyInfo(const ASource : IRpFileProxy);

    property FileName : string read GetFileName write SetFileName;
    property FileID : string read GetFileID write SetFileID;

    property FileStream : TStringStream read FFileStream;
    property FileSize : Int64 read GetFileSize;
    function FileSizeFmt : string;

    property DeleteOnRecive : Boolean read GetDeleteOnRecive write SetDeleteOnRecive;
    property Stream : TStream read GetStream;

    class function GetFileSizeFmt(const ABytes : Int64) : string;
  end;

  TRpDataFlashDataSetProviderCommand = class(TRpDataFlashCommand)
  private
    FProviderClass: string;
    FOperation: TRpDataFlashDataSetOperations;
    FApplyMaxErrors: Cardinal;
    FInfoQuery: Boolean;
    function ApplyUpdates : Boolean;
  protected
    // all the decendents must implement this
    function ExecSQL(const pSQL : string) : Boolean; virtual; abstract;
    function Select(const pSelectSQL : string; out XMLData : string): Boolean; virtual; abstract;
    function DoStartTransaction : Boolean; virtual; abstract;
    function DoCommit(const pRetaining : Boolean) : Boolean; virtual; abstract;
    function DoRollback(const pRetaining : Boolean) : Boolean; virtual; abstract;
    // custom providers don't need this
    function GetSelectSQL : string; virtual; abstract;
    function GetInsertSQL : string; virtual; abstract;
    function GetUpdateSQL : string; virtual; abstract;
    function GetDeleteSQL : string; virtual; abstract;

    function DoPrepareClient : Boolean;
    function DoExecute : Boolean; override; final;
    procedure DoRegisterParams; override; final;
  public
    constructor Create; override;
    destructor Destroy; override;

    property ProviderClass : string read FProviderClass;
    property Operation : TRpDataFlashDataSetOperations read FOperation;
    property ApplyMaxErrors : Cardinal read FApplyMaxErrors write FApplyMaxErrors;
    property InfoQuery : Boolean read FInfoQuery;
  end;

  TTcpClassRegisterItem = class
  private
    FClass : TRpDataFlashAbstractClass;
    FProxyGroup : string;
    FLifeCycle: TRpDataFlashLifeCycle;
    FPublicItem: Boolean;
    FMnemonic: string;
    function GetProxyGroup: string;
  public
    property ProxyClass : TRpDataFlashAbstractClass read FClass write FClass;
    property ProxyGroup : string read GetProxyGroup write FProxyGroup;
    property LifeCycle : TRpDataFlashLifeCycle read FLifeCycle write FLifeCycle;
    property PublicItem : Boolean read FPublicItem write FPublicItem;
    property Mnemonic : string read FMnemonic write FMnemonic;
  end;

  TTcpClassRegister = class(TObjectList)
  protected
    function GetItem(const Index : Integer) : TTcpClassRegisterItem;
  public
    class var TcpClassRegister: TTcpClassRegister;
    procedure Registrate(const AClass : TRpDataFlashAbstractClass; const AGrupoProxy : string;
      const AMnemonico : string = ''; const APublico : Boolean = False;
      const ALifeCycle : TRpDataFlashLifeCycle = tlfInstance);
    function GetClass(const AClassName : string) : TRpDataFlashAbstractClass; overload;
    function GetClass(const AClassName : string; out ALifeCycle : TRpDataFlashLifeCycle) : TRpDataFlashAbstractClass; overload;
    property Items[const Index: Integer]: TTcpClassRegisterItem read GetItem; default;
  end;

  TCPClassRegistrer = class
  public
    class procedure ReleaseRegistrer;
    class procedure Registrate(const AClass : TRpDataFlashAbstractClass; const AGrupoProxy : string;
      const AMnemonico : string = ''; const APublico : Boolean = False;
      const ALifeCycle : TRpDataFlashLifeCycle = tlfInstance);
    class procedure RegistrateDSProvider(const AClass : TRpDataFlashAbstractClass;
      const ALifeCycle : TRpDataFlashLifeCycle);
    class procedure RegisteredList(out ARegistrados : TTcpClassRegister; const ASomentePublicos : Boolean = False);
    class function GetClass(const AClassName : string) : TRpDataFlashAbstractClass; overload;
    class function GetClass(const AClassName : string; out ALifeCycle : TRpDataFlashLifeCycle) : TRpDataFlashAbstractClass; overload;
//    class procedure Instanciar(const AClass : TRpDataFlashAbstractClass; const ANomeInstancia : string);
  end;

implementation

{ TRpDataFlashCommandParameter }

procedure TRpDataFlashCommandParameter.Assign(const Source: TRpDataFlashCommandParameter);
begin
  if not Source.InheritsFrom(Self.ClassType) then
    raise Exception.Create('Classe de origem não permitida');

  FValue := Source.Value;
  FParamType := Source.ParamType;
  FValueType := Source.ValueType;
  FBaseClass := Source.BaseClass;
end;

constructor TRpDataFlashCommandParameter.Create(
  const AOwner: TRpDataFlashCommandParameterList; const AName: string;
  const AValue: Variant; const AParamType : TRpDataFlashParamType;
  const AValueType : TRpDataFlashParamValueType;
  const ABaseClass : string);
begin
  FOwner := AOwner;
  FParamType := AParamType;
  FName := AName;
  FValueType := AValueType;
  FBaseClass := ABaseClass;

  if (FValueType = tvpDateTime) and VarIsType(AValue, varDate) then
    SetAsDateTime( AValue )
  else
    FValue := AValue;
end;

constructor TRpDataFlashCommandParameter.Create;
begin
//dummy
end;

procedure TRpDataFlashCommandParameter.FromNode(const ANode: IXMLNode);
begin
  FValue := ANode[FName];
  FParamType := TRpDataFlashParamType(ANode.ChildNodes.FindNode(FName).Attributes['Tipo']);
end;

destructor TRpDataFlashCommandParameter.Destroy;
begin
  if Assigned(FFile) then
    FreeAndNil(FFile);
  inherited;
end;

function TRpDataFlashCommandParameter.GetAsBase64: string;
begin
  Result := Algorithms.Base64DecompressedString( VarToStrDef(FValue, EmptyStr) );
end;

function TRpDataFlashCommandParameter.GetAsBinaryFile: TRpFileProxy;
begin
  if not Assigned(FFile) then
  begin
    FFile := TRpFileProxy.Create;
    FFile.FileID := '-1';
  end;

  Result := FFile;
end;

function TRpDataFlashCommandParameter.GetAsBoolean: Boolean;
begin
  if (FValue = Null) or (FValue = Unassigned) then
    Result := False
  else
    try
      Result := FValue;
    except
      Result := False;
    end;
end;

function TRpDataFlashCommandParameter.GetAsDateTime: TDateTime;
begin
  Result := TRpDateConverter.Encode( GetAsString );
end;

function TRpDataFlashCommandParameter.GetAsFloat: Double;
begin
  Result := TRpFloatConverter.Encode( GetAsString );
end;

function TRpDataFlashCommandParameter.GetAsInteger: Integer;
begin
  try
    Result := StrToIntDef(VarToStrDef(FValue, '0'), 0);
  except
    Result := 0;
  end;
end;

function TRpDataFlashCommandParameter.GetAsJSONString: string;
begin
  Result := VarToStrDef(FValue, '');
  if Result = EmptyStr then
    Result := '{}';
end;

function TRpDataFlashCommandParameter.GetAsObject: TBaseSerializableObject;
var
  lClasseBase: TBaseSerializableObjectClass;
begin
  Result := nil;
  lClasseBase := SerializationClassRegistrer.GetClass(FBaseClass);
  if lClasseBase <> nil then
    Result := lClasseBase.CreateFromXML(AsBase64, nil);
end;

function TRpDataFlashCommandParameter.GetAsString: string;
begin
  Result := VarToStrDef(FValue, EmptyStr);
end;

function TRpDataFlashCommandParameter.GetAsVariant: string;
begin
  Result := FValue;
end;

function TRpDataFlashCommandParameter.GetIsEmpty: Boolean;
begin
  Result := (FValue = Null)
         or (FValue = Unassigned)
         or (VarToStrDef(FValue, EmptyStr) = EmptyStr);
end;

procedure TRpDataFlashCommandParameter.LoadFromFile(const AArquivo: string);
var
  lRetornoStr: TStringStream;
  lStream: TMemoryStream;
begin
  if FileExists(AArquivo) then
  begin
    lRetornoStr := TStringStream.Create('');
    lStream := TMemoryStream.Create;
    try
      lStream.LoadFromFile(AArquivo);
      lStream.Position := 0;
      lStream.SaveToStream(lRetornoStr);
      AsBase64 := lRetornoStr.DataString;
    finally
      FreeAndNil(lRetornoStr);
      FreeAndNil(lStream);
    end;
  end;
end;

function TRpDataFlashCommandParameter.ToApplicationJson: string;
var
  lValue : string;
  lFmtString : string;
begin
  if VarIsNull(FValue) then
    lValue := 'null'
  else
    lValue := FValue;
//    lValue := StringReplace(FValue, '\', '\\', [rfReplaceAll]);

  lFmtString := '"%s":';

//-    tvpInteger: ;
//-    tvpFloat: ;
//-    tvpBoolean: ;
//-    tvpBase: ;
//-    tvpJSON: ;

//    tvpString: ;
//    tvpBase64: ;
//    tvpDAO: ;
//    tvpDateTime: ;
//    tvpFile: ;
//    tvpBinaryFile: ;

  case FValueType of
    tvpBoolean:
    begin
      lFmtString := lFmtString + '%s';
      // False -> false | True -> true
      lValue := LowerCase(lValue);
    end;
    tvpInteger,
    tvpFloat,
    tvpBase,
    tvpJSON: lFmtString := lFmtString + '%s';
    tvpString,
    tvpBase64,
    tvpDAO,
    tvpDateTime,
    tvpFile,
    tvpBinaryFile: lFmtString := lFmtString + '"%s"';
  end;

  Result := Format(lFmtString, [
    FName,
    lValue ]);
end;

function TRpDataFlashCommandParameter.ToJson: string;
var
  lFormatStr: string;
  lValue : Variant;
begin
//      "InternalStatusProcessamento": {
//         "Tipo": "2",
//         "TipoValor": "0",
//         "Valor": "4"
//      },

  if VarIsNull(FValue) then
    lValue := 'null'
  else
    lValue := StringReplace(FValue, '\', '\\', [rfReplaceAll]);

  lFormatStr := '"%s":{"Tipo":%d,"TipoValor":%d,"BaseClass":"%s","Valor":';
  if ((FValueType = tvpJSON) or (FValueType = tvpBase)) then
    lFormatStr := lFormatStr + '%s}'
  else
    lFormatStr := lFormatStr + '"%s"}';

  Result := Format(lFormatStr, [
    FName,
    Integer(FParamType),
    Integer(FValueType),
    FBaseClass,
    lValue]);
end;

procedure TRpDataFlashCommandParameter.ToNode(const ANode: IXMLNode);
var
  lNodo: IXMLNode;
begin
  if FValueType = tvpBinaryFile then
  begin
    ANode[FName] := C_FTP_ID_MARK + '=' + AsBinaryFile.FileID
                  + '|'
                  + C_FTP_SIZE_MARK + '=' + IntToStr(AsBinaryFile.FileSize)
                  + '|'
                  + C_FTP_FILENAME_MARK + '=' + ExtractFileName(AsBinaryFile.FileName)
                  + '|'
                  + C_FTP_DELETE_RECIVE_MARK + '=' + IfThen(AsBinaryFile.DeleteOnRecive, 'T', 'F');
  end
  else
    ANode[FName] := FValue;

  lNodo := ANode.ChildNodes.FindNode(FName);
  lNodo.Attributes['Tipo'] := Integer(FParamType);
  lNodo.Attributes['TipoValor'] := Integer(FValueType);
  lNodo.Attributes['BaseClass'] := FBaseClass;
end;

procedure TRpDataFlashCommandParameter.SaveToFile(const AArquivo: string);
var
  lRetornoStr: TStringStream;
  lStream: TMemoryStream;
begin
  lRetornoStr := TStringStream.Create(AsBase64);
  lStream := TMemoryStream.Create;
  try
    lRetornoStr.Position := 0;
    lStream.LoadFromStream(lRetornoStr);
    lStream.Position := 0;
    lStream.SaveToFile(AArquivo);
  finally
    FreeAndNil(lRetornoStr);
    FreeAndNil(lStream);
  end;
end;

procedure TRpDataFlashCommandParameter.SetAsBase64(const Value: string);
begin
  FValue := Algorithms.Base64CompressedString(Value);
end;

procedure TRpDataFlashCommandParameter.SetAsBoolean(const Value: Boolean);
begin
  FValue := Value;
end;

procedure TRpDataFlashCommandParameter.SetAsDateTime(const Value: TDateTime);
begin
  FValue := TRpDateConverter.Decode(Value);
end;

procedure TRpDataFlashCommandParameter.SetAsFloat(const Value: Double);
begin
  FValue := TRpFloatConverter.Decode( Value );
end;

procedure TRpDataFlashCommandParameter.SetAsInteger(const Value: Integer);
begin
  FValue := Value;
end;

procedure TRpDataFlashCommandParameter.SetAsJSONString(const Value: string);
begin
  FValue := Value;
end;

procedure TRpDataFlashCommandParameter.SetAsString(const Value: string);
begin
  FValue := Value;
end;

procedure TRpDataFlashCommandParameter.SetAsVariant(const Value: string);
begin
  FValue := Value;
end;

{ TRpDataFlashCommandParameterList }

procedure TRpDataFlashCommandParameterList.AddParam(const AName: string;
  const AValue: Variant; const AParamType: TRpDataFlashParamValueType);
begin
  AddNew(AName, AValue, tpInput, AParamType);
end;

procedure TRpDataFlashCommandParameterList.AddResult(const AName: string;
  AValue: Variant; const AParamType: TRpDataFlashParamValueType);
begin
  AddNew(AName, AValue, tpOutput, AParamType);
end;

procedure TRpDataFlashCommandParameterList.Assign(const Source: TRpDataFlashCommandParameterList);
var
  I: Integer;
  lParam: TRpDataFlashCommandParameter;
  lSourceParam: TRpDataFlashCommandParameter;
begin
  if Source <> Self then
  begin
    for I := 0 to Source.FParamList.Count - 1 do
    begin
      lSourceParam := TRpDataFlashCommandParameter(Source.FParamList[I]);
      lParam       := FindByName(lSourceParam.Name);

      if Assigned(lParam) then
        lParam.Assign(lSourceParam)
      else
      begin
        if (lSourceParam.ValueType = tvpBase) and (lSourceParam.BaseClass <> EmptyStr) then
          AddNew(lSourceParam.Name, lSourceParam.ParamType, lSourceParam.BaseClass).AsVariant := lSourceParam.Value
        else
          AddNew(lSourceParam.Name, lSourceParam.Value, lSourceParam.ParamType, lSourceParam.ValueType);
      end;
    end;
  end;
end;

procedure TRpDataFlashCommandParameterList.Load(const AParams: string);

  procedure InternalLoadXML;
  var
    lXML: IXMLDocument;
    lNodoRoot: IXMLNode;
    lNodoComando: IXMLNode;
    lStream: TStringStream;
    lNodoParametros: IXMLNode;
  begin
    AddNew(C_PARAM_INT_FORMAT_TYPE, Ord(sfXML), tpInternal, tvpInteger);

    lStream := TStringStream.Create(AParams);
    try
      try
        lXML := TXMLDocument.Create(nil);

        lXML.LoadFromStream(lStream);

        lNodoRoot := lXML.ChildNodes.FindNode('root');
        lNodoComando := lNodoRoot.ChildNodes.FindNode('TCPComando');

        FCommand := lNodoComando.Attributes['Comando'];

        if FCommand = '' then
          raise Exception.Create('Carga. Comando não pode ser vazio !');

        lNodoParametros := lNodoComando.ChildNodes.FindNode('Parametros');
        if lNodoParametros <> nil then
          FromNode(lNodoParametros);
      except
        on E:Exception do
          raise Exception.CreateFmt('Erro ao carregar comando: %s. %s', [E.Message, AParams]);
      end;
    finally
      lStream.Free;
    end;
  end;

  procedure InternalLoadJson;
  var
    lJsonObj: TJSONObject;
    lPair: TJSONPair;
  begin
    try
      AddNew(C_PARAM_INT_FORMAT_TYPE, Ord(sfJSON), tpInternal, tvpInteger);

      lJsonObj := TJSONObject.Create;
      try
        lJsonObj.Parse(BytesOf(AParams), 0);
        lPair := lJsonObj.Get('Comando');
        if lPair <> nil then
          FCommand := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll]);

        if FCommand = '' then
          raise Exception.Create('Carga. Comando não pode ser vazio !');

        lPair := lJsonObj.Get('Parametros');
        if lPair <> nil then
          FromJson(lPair);
      finally
        FreeAndNil(lJsonObj);
      end;
    except on E:Exception do
      raise Exception.Create('Erro ao carregar mensagem : ' + sLineBreak +
        '(J) : ' +  AParams);
    end;
  end;

begin
  FCommand := '';
  if AParams <> EmptyStr then
  begin
    if AParams[1] = '<' then
    begin
      InternalLoadXML;
      SerializationFormat := sfXML;
    end else
    begin
      InternalLoadJson;
      SerializationFormat := sfJSON;
    end;
  end
  else
    raise Exception.Create('Nenhum parâmetro foi recebido.');
end;

procedure TRpDataFlashCommandParameterList.ParseJSON(const AJSONString: string);
var
  i: Integer;
  lPair: TJSONPair;
  lJsonObj: TJSONObject;
  lValue: string;
  lParamItem: TRpDataFlashCommandParameter;
begin
  // percorre a lista de parametros, e verifica se existe um Json com o mesmo nome,
  // se existir e este estiver vazio na lista de parametros, sera atualizado com o
  // valor do Json
  try
    lJsonObj := TJSONObject.Create;
    try
      lJsonObj.Parse(BytesOf(AJSONString), 0);

      for i := 0 to Self.Count - 1 do
      begin
        lParamItem := Self[i];
        if (lParamItem.ParamType in [tpInput, tpInputNoReload]) and lParamItem.IsEmpty then
        begin
          lPair := lJsonObj.Get( lParamItem.Name );
          if lPair <> nil then
          begin
            // value = tem valor
            lValue := lPair.JsonValue.Value;
            if lValue = EmptyStr then
            begin
              lValue := lPair.JsonValue.ToString;
              if (lValue <> EmptyStr) and (Copy(lValue, 1, 1) = '{') then
              begin
                lParamItem.AsBase64 := lValue;
                TRpDataFlashCommandParameterSetter(lParamItem).SetValueType(tvpBase64);
              end else
                lParamItem.AsVariant := lValue;
            end else
              lParamItem.AsVariant := lValue;
          end;
        end;
      end;
    finally
      FreeAndNil(lJsonObj);
    end;
  except
    on E:Exception do
      raise Exception.Create('Erro ao carregar mensagem (J): ' + E.Message);
  end;
end;

constructor TRpDataFlashCommandParameterList.Create(const AFileTransferSupport : IRpDataFlashFileTransferSupport);
begin
  FContentType := EmptyStr;
  FParamList := TObjectList.Create;
  FFileTransferSupport := AFileTransferSupport;
  AddNew( C_PARAM_INT_STATUS_PROC , EmptyStr, tpInternal, tvpInteger);
end;

procedure TRpDataFlashCommandParameterList.FromJson(const AJsonPair: TJSONPair);
var
  i: Integer;
  lTipo: TRpDataFlashParamType;
  lTipoValor: TRpDataFlashParamValueType;
  lJson : TJSONObject;
  lValues : TJSONObject;
  lPair: TJSONPair;
  lName: string;
  lValueStr: string;
  lBaseClass: string;

  function ParseStringValue(const AValue : string) : string;
  begin
    Result := AValue;
    if (Result <> '') then
    begin
      if Result[1] = '"' then
        Delete(Result, 1, 1);

      if Result[ Length(Result) ] = '"' then
        Delete(Result, Length(Result), 1);
    end;
  end;

begin
  try
    lJson := AJsonPair.JsonValue as TJSONObject;
    for i := 0 to lJson.Size - 1 do
    begin
      lPair := lJson.Get(i);
      lName := StringReplace(lPair.FieldName, '"', '', [rfReplaceAll]);

      lValues := lPair.JsonValue as TJSONObject;

      lTipo := TRpDataFlashParamType(StrToInt(lValues.Get('Tipo').FieldValue));
      lTipoValor := TRpDataFlashParamValueType(StrToInt(lValues.Get('TipoValor').FieldValue));
      lBaseClass := ParseStringValue(lValues.Get('BaseClass').FieldValue);
      lValueStr := ParseStringValue(lValues.Get('Valor').FieldValue);
// Já retorna sem a segunda bara
//      lValueStr := StringReplace(lValueStr, '\\', '\', [rfReplaceAll]);
      AddNew(lName, lValueStr, lTipo, lTipoValor, lBaseClass);
    end;
  except
    raise Exception.Create('Erro carregando parâmetros JSON.');
  end;
end;

procedure TRpDataFlashCommandParameterList.FromNode(const ANode: IXMLNode);
var
  I: Integer;
  lNodo: IXMLNode;
  lTipo: TRpDataFlashParamType;
  lTipoValor: TRpDataFlashParamValueType;
  lClasse : string;
begin
  for I := 0 to ANode.ChildNodes.Count - 1 do
  begin
    lNodo := ANode.ChildNodes[I];
    lTipo := TRpDataFlashParamType(lNodo.Attributes['Tipo']);
    lTipoValor := TRpDataFlashParamValueType(lNodo.Attributes['TipoValor']);
    lClasse := VarToStrDef(lNodo.Attributes['BaseClass'], EmptyStr);

    if lClasse <> EmptyStr then
      AddNew(lNodo.NodeName, lTipo, lClasse);

    AddNew(lNodo.NodeName, lNodo.NodeValue, lTipo, lTipoValor);
  end;
end;

destructor TRpDataFlashCommandParameterList.Destroy;
begin
  FreeAndNil(FParamList);
  inherited;
end;

function TRpDataFlashCommandParameterList.GetCount: Integer;
begin
  if Assigned(FParamList) then
    Result := FParamList.Count
  else
    Result := -1;
end;

function TRpDataFlashCommandParameterList.GetParamByName(const AName: string;
  const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
var
  lParametro: TRpDataFlashCommandParameter;
begin
  lParametro := FindByName(AName, AParamType);

  if lParametro = nil then
    raise ERpDataFlashParamNotFound.Create('Parâmetro ''' + AName + ''' não encontrado!');

  Result := lParametro;
end;

function TRpDataFlashCommandParameterList.GetParamByIdx(const Index: Integer): TRpDataFlashCommandParameter;
begin
  try
    Result := TRpDataFlashCommandParameter(FParamList[Index]);
  except
    Result := nil;
  end;
end;

function TRpDataFlashCommandParameterList.GetProcessingStatus: TRpDataFlashProcessingStatus;
begin
  Result := TRpDataFlashProcessingStatus(InternalParam[C_PARAM_INT_STATUS_PROC].AsInteger);
end;

function TRpDataFlashCommandParameterList.AddNew(const AName: string;
  const AParamType: TRpDataFlashParamType;
  const AClasseBase: string): TRpDataFlashCommandParameter;
begin
  Result := TRpDataFlashCommandParameter.Create(Self, AName, '', AParamType, tvpBase, AClasseBase);
  AddNew(Result);
end;

procedure TRpDataFlashCommandParameterList.AddNew(const AParam: TRpDataFlashCommandParameter);
begin
  if not Assigned(FindByName(AParam.Name)) then
    FParamList.Add(AParam);
end;

function TRpDataFlashCommandParameterList.AddNew(const AName: string; AValue: Variant; const AParamType : TRpDataFlashParamType;
  const AValueType : TRpDataFlashParamValueType; const ABaseClassName : string) : TRpDataFlashCommandParameter;
begin
  Result := FindByName(AName, AParamType);
  if not Assigned(Result) then
  begin
    Result := TRpDataFlashCommandParameter.Create(Self, AName, AValue, AParamType, AValueType, ABaseClassName);
    AddNew(Result);
  end;

  if (AValue <> Unassigned) and (AValue <> Null) then
  begin
    if (AValueType = tvpDateTime) and VarIsType(AValue, varDate) then
      Result.AsDateTime := AValue
    else
      if (AValueType = tvpFloat) and VarIsType(AValue, varDouble) then
        Result.AsFloat := AValue
      else
        Result.AsVariant := AValue;
  end;
end;

function TRpDataFlashCommandParameterList.ToApplicationJson: string;
var
  i: Integer;
  lParametros: string;
  lItem: TRpDataFlashCommandParameter;
begin
  lParametros := '';

  // se tem erro, retorna somente ele
  lParametros := InternalParam[C_PARAM_INT_EXCEPTION].AsString;
  if lParametros <> EmptyStr then
    lParametros := Format('"%s":"%s"', [C_PARAM_INT_EXCEPTION, lParametros])
  else begin
    for I := 0 to FParamList.Count - 1 do
    begin
      lItem := TRpDataFlashCommandParameter(FParamList[i]);
      if (lItem.ParamType = tpOutput) or (lItem.Name = C_PARAM_INT_EXCEPTION) then
      begin
        if lParametros <> '' then
          lParametros := lParametros + ',';

        lParametros := lParametros + lItem.ToApplicationJson;
      end;
    end;
  end;

  Result := '{' +  lParametros + '}';
end;

function TRpDataFlashCommandParameterList.ToJson: string;
var
  i: Integer;
  lParametros: string;
begin
  lParametros := '';
  for i := 0 to FParamList.Count - 1 do
  begin
    if lParametros <> '' then
      lParametros := lParametros + ',';

    lParametros := lParametros + TRpDataFlashCommandParameter(FParamList[i]).ToJson;
  end;

  Result := '"Parametros": {' + lParametros + '}';
end;

procedure TRpDataFlashCommandParameterList.ToNode(const ANode: IXMLNode);
var
  I: Integer;
begin
  for I := 0 to FParamList.Count - 1 do
    TRpDataFlashCommandParameter(FParamList[I]).ToNode(ANode);
end;

function TRpDataFlashCommandParameterList.FindByName(const AName: string;
  const AParamType: TRpDataFlashParamType): TRpDataFlashCommandParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FParamList.Count - 1 do
  begin
    if ((TRpDataFlashCommandParameter(FParamList[I]).ParamType in [AParamType, tpInternal, tpInputNoReload]))
    and (TRpDataFlashCommandParameter(FParamList[I]).Name = AName) then
    begin
      Result := TRpDataFlashCommandParameter(FParamList[I]);
      Exit;
    end;
  end;
end;

function TRpDataFlashCommandParameterList.Serialize: string;
var
  lXML: IXMLDocument;
  lXmlNode: IXMLNode;
  lStream: TStringStream;
begin
  if FCommand = '' then
    raise Exception.Create('Serialização. Comando não pode ser vazio!');

  if SerializationFormat = sfAuto then
    SerializationFormat := TSerializationFormat(Param[C_PARAM_INT_FORMAT_TYPE].AsInteger);

  // mantem como padrao o JSON
  if SerializationFormat in [sfAuto, sfJSON] then
  begin
//lComando.RequestInfo.ContentType
    if FContentType = C_REST_CONTENT_TYPE_JSON then
      Result := ToApplicationJson
    else
      Result := '{"Comando":"' + FCommand + '",' + ToJson + '}';
  end
  else
  begin
    lStream := TStringStream.Create('<?xml version="1.0"?><root/>');
    try
      lXML := TXMLDocument.Create(nil);
      lXML.LoadFromStream(lStream);
      {$IFDEF UNICODE}
      lXML.Encoding := 'UTF-16';
      {$ELSE}
      lXML.Encoding := 'iso-8859-1';
      {$ENDIF}
      lXML.Version := '1.0';
      lXML.StandAlone := 'yes';

      lXmlNode := lXML.ChildNodes.FindNode('root');
      if lXmlNode <> nil then
      begin
        lXmlNode := lXmlNode.AddChild('TCPComando');

        lXmlNode.Attributes['Comando'] := FCommand;
        lXmlNode := lXmlNode.AddChild('Parametros');

        ToNode(lXmlNode);
        Result := lXML.XML.Text;
        {$IFDEF UNICODE}
        Result := StringReplace(Result, 'UTF-16', 'iso-8859-1', []);
        {$ENDIF}
      end
      else
        raise Exception.Create('Serialização. Não foi possível ler o XML do comando.');
    finally
      FreeAndNil( lStream );
    end;
  end
end;

procedure TRpDataFlashCommandParameterList.SetProcessingStatus(const Value: TRpDataFlashProcessingStatus);
begin
  Param[C_PARAM_INT_STATUS_PROC].AsInteger := Ord(Value);
end;

function TRpDataFlashCommandParameterList.FindByName(const AName: string): TRpDataFlashCommandParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FParamList.Count - 1 do
  begin
    if TRpDataFlashCommandParameter(FParamList[I]).Name = AName then
    begin
      Result := TRpDataFlashCommandParameter(FParamList[I]);
      Exit;
    end;
  end;
end;

{ TRpDataFlashCommand }

procedure TRpDataFlashCommand.Load(const ACommand: string);
begin
  FParamList.Load(ACommand);
  DoLoad;
end;

class function TRpDataFlashCommand.LoadCommand(
  const ACommand : string;
  out ACommandObject : IRpDataFlashCommandInterfaced;
  out AParams : TRpDataFlashCommandParameterList;
  const AServerInstanceController : IServerInstanceController;
  const ASessionInstanceController :ISessionInstanceController;
  const ALoadParams : Boolean): Boolean;
var
  lLifeCycle: TRpDataFlashLifeCycle;
begin
  AParams := TRpDataFlashCommandParameterList.Create(nil);
  AParams.Load(ACommand);

  ACommandObject := nil;

  TRpDataFlashCommand.LoadCommand(
    AParams.Command,
    ACommandObject,
    lLifeCycle,
    AServerInstanceController,
    ASessionInstanceController,
    AParams.SerializationFormat);

  if ALoadParams and Assigned(ACommandObject) then
    ACommandObject.DoLoad(loSend, AParams);

  Result := ACommandObject <> nil;
end;

class function TRpDataFlashCommand.LoadCommand(const ACommandClass: string;
  out ACommandObject: IRpDataFlashCommandInterfaced;
  out ALifeCycle: TRpDataFlashLifeCycle;
  const AServerInstanceController: IServerInstanceController;
  const ASessionInstanceController: ISessionInstanceController;
  const ASerializationFormat : TSerializationFormat): Boolean;
var
  lComandoClass: TRpDataFlashAbstractClass;
begin
  lComandoClass := TCPClassRegistrer.GetClass(ACommandClass, ALifeCycle);
  if (lComandoClass <> nil) then
  begin
    if Supports(lComandoClass, IRpDataFlashCommandInterfaced) then
    begin
      if ALifeCycle = tlfSession then
        ACommandObject := ASessionInstanceController.FindInstance(ACommandClass)
      else if ALifeCycle = tlfServer then
        ACommandObject := AServerInstanceController.FindInstance(ACommandClass);

      if ACommandObject = nil then
      begin
        if lComandoClass.InheritsFrom(TComponent) then
          ACommandObject := (TComponentClass(lComandoClass).Create(nil) as IRpDataFlashCommandInterfaced)
        else
          if lComandoClass.InheritsFrom(TRpDataFlashCommand) then
            ACommandObject := (TRpDataFlashCommandClass(lComandoClass).Create as IRpDataFlashCommandInterfaced)
          else
            ACommandObject  := (TRpDataFlashCommand(lComandoClass.Create) as IRpDataFlashCommandInterfaced);
        ACommandObject.LifeCycle := ALifeCycle;

        ACommandObject.ServerInstanceController := AServerInstanceController;
        ACommandObject.SessionInstanceController := ASessionInstanceController;
        ACommandObject.GetParams.SerializationFormat := ASerializationFormat;
      end;

      if ALifeCycle = tlfSession then
        ASessionInstanceController.AddInstance(ACommandObject)
      else
        if ALifeCycle = tlfServer then
          AServerInstanceController.AddInstance(ACommandObject);
    end;
  end;
  Result := ACommandObject <> nil;
end;

constructor TRpDataFlashCommand.Create;
begin
  // inicializados na primeira execucao (e atualziados a cada execucao)
  FServer := nil;
  FConnectionItem := nil;

  FLock := False;
  FParamList := TRpDataFlashCommandParameterList.Create(nil);
  FParamList.Command := GetCommand;
  FSerealizationFormat := sfJSON;
  //FStatusProcessamento := tspServidor;
  DoRegisterParams;

  FParamList.ProcessingStatus := psNone;
end;

destructor TRpDataFlashCommand.Destroy;
begin
  FreeAndNil(FParamList);
  FConnectionItem := nil;
  inherited;
end;

function TRpDataFlashCommand.DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean;
begin
  Result := False;
end;

procedure TRpDataFlashCommand.DoLoad;
begin
//dummy
end;

procedure TRpDataFlashCommand.DoExecutionError(const AParams: TRpDataFlashCommandParameterList);
begin
  FParamList.Assign(AParams);
  DoExecutionError(EmptyStr);
end;

procedure TRpDataFlashCommand.DoExecutionError(const AErrorMsg : string);
begin
  ResultParam[C_PARAM_INT_EXCEPTION].AsString := AErrorMsg;
end;

procedure TRpDataFlashCommand.DoExecuteBeforeBridgeConnection(var AContinue: Boolean);
begin
//dummy
end;

procedure TRpDataFlashCommand.DoExecuteBridgeSuccessfully(var AContinue: Boolean);
begin
//dummy
end;

procedure TRpDataFlashCommand.DoExecuteBridgeError(var AContinue : Boolean);
begin
//dummy
end;

function TRpDataFlashCommand.DoGetDescription: string;
begin
  Result := R_DATAFLASH_CMD_NO_DESCRIPTION;
end;

function TRpDataFlashCommand.DoGetSerializedParams: string;
begin
  if Assigned(FRequestInfo) then // and (FRequestInfo.ContentType = C_REST_CONTENT_TYPE_JSON) then
    FParamList.ContentType := FRequestInfo.ContentType;

  Result := FParamList.Serialize;
end;

procedure TRpDataFlashCommand.DoRegisterParams;
begin
  NewInternalParam(C_PARAM_INT_FORMAT_TYPE, tvpInteger);
  NewInternalParam(C_PARAM_INT_STATUS_RET, tvpBoolean);
  NewInternalParam(C_PARAM_INT_EXCEPTION, tvpString);
end;

procedure TRpDataFlashCommand.DoRegisterParams(
  const AParams: TRpDataFlashCommandParameterList);
begin
  FParamList.Assign(AParams);
  DoRegisterParams;
end;

procedure TRpDataFlashCommand.DoLoad(const ALoadType : TRpDataFlashLoadType;
  const AParams: TRpDataFlashCommandParameterList);
begin
//  FParametros := AParametros;
  FParamList.Assign(AParams);
  DoLoad;
end;

procedure TRpDataFlashCommand.DoSerialize(const AParams: TRpDataFlashCommandParameterList);
begin
//  FParametros := AParametros;
  FParamList.SerializationFormat := Self.SerializationFormat;
  FParamList.Assign(AParams);
  DoSerialize;
end;

procedure TRpDataFlashCommand.DoSerialize;
begin
//dummy
end;

procedure TRpDataFlashCommand.DoValidateParams;
begin
//dummy
end;

function TRpDataFlashCommand.Execute(const AParams: TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor): string;
var
  lContinuar: Boolean;
begin
  lContinuar := True;
  Result := Execute(AParams, AExecutor, etExecution, lContinuar);
end;

function TRpDataFlashCommand.SendCallBack: Boolean;
var
  lParams: TRpDataFlashCommandParameterList;
begin
  Result := False;

  lParams := TRpDataFlashCommandParameterList.Create(nil);
  try
    lParams.Command := TAG_CALLBACK;
    if DoCallBack(lParams) and Assigned(lParams) and Assigned(FOnCallBackEvent) then
      Result := FOnCallBackEvent(FContext, lParams.Serialize);
  finally
    if Assigned(lParams) then
      FreeAndNil(lParams);
  end;
end;

function TRpDataFlashCommand.Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
  const AParamType : TRpDataFlashExecutionType; var AContinue : Boolean): string;
var
  lRetornoPositivo: Boolean;
begin
  while Lock do
  begin
    Sleep(100);
  end;

  Lock := True;
  try
    FParamList.Assign(AParams);
    FExecutor := AExecutor;
    lRetornoPositivo := False;
    try
      DoValidateParams;
      DoLoad;

      case AParamType of
      etExecution:
        lRetornoPositivo := DoExecute;
      etBridgeInvalid:
        begin
          DoExecuteBridgeError(AContinue);
          lRetornoPositivo := AContinue;
        end;
      etBridgeDone:
        begin
          DoExecuteBridgeSuccessfully(AContinue);
          lRetornoPositivo := AContinue;
        end;
      etBeforeExecBridge:
        begin
          DoExecuteBeforeBridgeConnection(AContinue);
          lRetornoPositivo := AContinue;
        end;
      end;
    except
      on E:Exception do
      begin
        if (Executor <> nil)
        and ((Pos('error writing data to connection', AnsiLowerCase(E.Message)) > 0)
          or (Pos('error reading data to connection', AnsiLowerCase(E.Message)) > 0)) then
        begin
          Executor.DisconnectDataComponent;
        end;

        DoExecutionError(E.Message);
      end;
    end;
    ResultParam[C_PARAM_INT_STATUS_RET].AsBoolean := lRetornoPositivo;
    Result := Serialize;
  finally
    Lock := False;
  end;
end;

function TRpDataFlashCommand.ExecuteBeforeBridgeConnection(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBeforeExecBridge, AContinue);
end;

function TRpDataFlashCommand.ExecuteBridgeSuccessfully(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBridgeDone, AContinue);
end;

function TRpDataFlashCommand.ExecuteBridgeError(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue : Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBridgeInvalid, AContinue);
end;

function TRpDataFlashCommand.FindParametro(const AName: string): TRpDataFlashCommandParameter;
begin
  Result := FParamList.FindByName(AName, tpInput);
end;

function TRpDataFlashCommand.FindResult(const AName: string): TRpDataFlashCommandParameter;
begin
  Result := FParamList.FindByName(AName, tpOutput);
end;

function TRpDataFlashCommand.GetCommand: string;
begin
  Result := Self.ClassName;
end;

function TRpDataFlashCommand.GetDescription: string;
begin
  Result := DoGetDescription;
end;

function TRpDataFlashCommand.GetExecutor: IRpPackageCommandExecutor;
begin
  Result := FExecutor;
end;

function TRpDataFlashCommand.GetSerializationFormat: TSerializationFormat;
begin
  Result := FSerealizationFormat;
end;

function TRpDataFlashCommand.GetServerInstanceController: IServerInstanceController;
begin
  Result := FServerInstanceController;
end;

function TRpDataFlashCommand.GetSessionInstanceController: ISessionInstanceController;
begin
  Result := FSessionInstanceController;
end;

function TRpDataFlashCommand.GetLifeCycle: TRpDataFlashLifeCycle;
begin
  Result := FLifeCycle;
end;

function TRpDataFlashCommand.GetLock: Boolean;
begin
  Result := FLock;
end;

function TRpDataFlashCommand.GetObject: TObject;
begin
  Result := Self;
end;

function TRpDataFlashCommand.GetParamByName(const AName: string;
  const AParamType: TRpDataFlashParamType): TRpDataFlashCommandParameter;
begin
  Result := FParamList.GetParamByName(AName, AParamType);
end;

function TRpDataFlashCommand.GetParams: TRpDataFlashCommandParameterList;
begin
  Result := FParamList;
end;

function TRpDataFlashCommand.GetRequestInfo: TIdHTTPRequestInfo;
begin
  Result := FRequestInfo;
end;

function TRpDataFlashCommand.GetResponseFileName: string;
begin
  Result := '';
end;

function TRpDataFlashCommand.GetResponseInfo: TIdHTTPResponseInfo;
begin
  Result := FResponseInfo;
end;

function TRpDataFlashCommand.GetProcessingStatus: TRpDataFlashProcessingStatus;
begin
//  Result := FStatusProcessamento;
  Result := FParamList.ProcessingStatus;
end;

function TRpDataFlashCommand.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtRemote;
end;

function TRpDataFlashCommand.InternalLoadCommand(
  const ACommand: TRpDataFlashCommandClass;
  out ACommandObject: IRpDataFlashCommandInterfaced;
  out AParams: TRpDataFlashCommandParameterList): Boolean;
var
  lComando: string;
begin
  // ver como fica esta parte, o comando para carregar não é o "nome" do comando
  // mas sim o XML que é recebido pelo server com todos os parametros
  if FSerealizationFormat = sfJSON then
  begin
    lComando := '{"Comando":"' + ACommand.ClassName + '","Parametros":{"InternalStatusProcessamento":{"Tipo":2,"TipoValor":0,"Valor":"4"},'
              + '"internal_FormatType":{"Tipo":2,"TipoValor":0,"Valor":"2"},"exec_StatusRetorno":{"Tipo":2,"TipoValor":2,"Valor":"true"},'
              + '"exec_Exception":{"Tipo":2,"TipoValor":1,"Valor":""}}}';
  end
  else
  begin
    lComando := '<root><TCPComando Comando="' + ACommand.ClassName + '"><Parametros>'
              + '<InternalStatusProcessamento Tipo="2" TipoValor="0">4</InternalStatusProcessamento>'
              + '<exec_StatusRetorno Tipo="2" TipoValor="2"></exec_StatusRetorno>'
              + '<exec_Exception Tipo="2" TipoValor="1"></exec_Exception></Parametros></TCPComando></root>';
  end;

  Result := LoadCommand(lComando, ACommandObject, AParams,
    FServerInstanceController, FSessionInstanceController, False);

  if Assigned(ACommandObject) then
  begin
    while ACommandObject.Lock do
      Sleep(100);
  end;
end;

function TRpDataFlashCommand.LastError: string;
begin
  Result := FParamList.ResultParam[C_PARAM_INT_EXCEPTION].AsString;
end;

procedure TRpDataFlashCommand.NewParam(const AName: string;
  const AParamType: TRpDataFlashParamValueType; const ARecarregar : Boolean);
begin
  if not ARecarregar then
    FParamList.AddNew(AName, EmptyStr, tpInputNoReload, AParamType)
  else
    FParamList.AddNew(AName, EmptyStr, tpInput, AParamType);
end;

procedure TRpDataFlashCommand.NewResult(const AName: string;
  const AParamType: TRpDataFlashParamValueType);
begin
  FParamList.AddNew(AName, EmptyStr, tpOutput, AParamType);
end;

function TRpDataFlashCommand.Serialize: string;
begin
  DoSerialize;
  Result := DoGetSerializedParams;
//  Result := FParametros.Serializar;
end;

procedure TRpDataFlashCommand.SetSerializationFormat(const Value: TSerializationFormat);
begin
  FSerealizationFormat := Value;
end;

procedure TRpDataFlashCommand.SetServer(const AServer: TComponent);
begin
  FServer := AServer;
end;

procedure TRpDataFlashCommand.SetServerInstanceController(
  const Value: IServerInstanceController);
begin
  FServerInstanceController := Value;
end;

procedure TRpDataFlashCommand.SetSessionInstanceController(
  const Value: ISessionInstanceController);
begin
  FSessionInstanceController := Value;
end;

procedure TRpDataFlashCommand.SetConnectionItem(const AConnectionItem: IAutenticationProvider);
begin
  FConnectionItem := AConnectionItem;
end;

procedure TRpDataFlashCommand.SetExecutor(const AExecutor: IRpPackageCommandExecutor);
begin
  FExecutor := AExecutor;
end;

procedure TRpDataFlashCommand.SetLifeCycle(const ALifeCycle: TRpDataFlashLifeCycle);
begin
  FLifeCycle := ALifeCycle;
end;

procedure TRpDataFlashCommand.SetLock(const Value: Boolean);
begin
  FLock := Value;
end;

procedure TRpDataFlashCommand.SetRequestInfo(const AValue: TIdHTTPRequestInfo);
begin
  FRequestInfo := AValue;
end;

procedure TRpDataFlashCommand.SetResponseInfo(const AValue: TIdHTTPResponseInfo);
begin
  FResponseInfo := AValue;
end;

procedure TRpDataFlashCommand.SetCallBackEvent(const AContext: TIdContext;const ACallBackEvent: TRpDataFlashCallBackEvent);
begin
  FOnCallBackEvent := ACallBackEvent;
  FContext := AContext;
end;

procedure TRpDataFlashCommand.SetProcessingStatus(const Value: TRpDataFlashProcessingStatus);
begin
  FParamList.ProcessingStatus := Value;
end;

function TRpDataFlashCommand.ReturnStatus: Boolean;
begin
  Result := FParamList.ResultParam[C_PARAM_INT_STATUS_RET].AsBoolean;
end;

{ TRpDataFlashSendCommand }

function TRpDataFlashSendCommand.DoExecute: Boolean;
begin
  Result := True;
end;

procedure TRpDataFlashSendCommand.SetCommand(const ACommand: string);
begin
  FCommand := ACommand;
  if Trim(FParamList.Command) = EmptyStr then
    FParamList.Command := Trim(FCommand);
//  FParametros.Comando := AComando;
//  FComando := AComando;
end;

function TRpDataFlashSendCommand.GetCommand: string;
begin
  Result := FCommand;
end;

procedure TRpDataFlashSendCommand.SetCommand(const ACommand: TRpDataFlashCommandClass);
begin
  SetCommand(ACommand.ClassName);
end;

procedure TRpDataFlashCommand.NewParam(const AName: string;
  const ABaseClass: TBaseSerializableObjectClass; const ARecarregar: Boolean);
begin
  if not ARecarregar then
    FParamList.AddNew(AName, tpInputNoReload, ABaseClass.ClassName)
  else
    FParamList.AddNew(AName, tpInput, ABaseClass.ClassName);
end;

procedure TRpDataFlashCommand.NewInternalParam(const AName: string;
  const AParamType: TRpDataFlashParamValueType);
begin
  FParamList.AddNew(AName, EmptyStr, tpInternal, AParamType);
end;

procedure TRpDataFlashCommand.NewResult(const AName: string;
  const ABaseClass: TBaseSerializableObjectClass; const ARecarregar: Boolean);
begin
  FParamList.AddNew(AName, tpOutput, ABaseClass.ClassName);
end;

{ TCPClassRegistrer }

class procedure TCPClassRegistrer.ReleaseRegistrer;
begin
  if TTcpClassRegister.TcpClassRegister <> nil then
    FreeAndNil(TTcpClassRegister.TcpClassRegister);
end;

class function TCPClassRegistrer.GetClass(const AClassName: string): TRpDataFlashAbstractClass;
begin
  Result := TTcpClassRegister.TcpClassRegister.GetClass(AClassName);
end;

class function TCPClassRegistrer.GetClass(const AClassName: string;
  out ALifeCycle: TRpDataFlashLifeCycle): TRpDataFlashAbstractClass;
begin
  Result := TTcpClassRegister.TcpClassRegister.GetClass(AClassName, ALifeCycle);
end;

class procedure TCPClassRegistrer.RegisteredList(out ARegistrados: TTcpClassRegister;
  const ASomentePublicos : Boolean);
var
  I: Integer;
  lItem : TTcpClassRegisterItem;
  lAdicionar: Boolean;
begin
  ARegistrados := TTcpClassRegister.Create;

  if TTcpClassRegister.TcpClassRegister <> nil then
    for I := 0 to TTcpClassRegister.TcpClassRegister.Count - 1 do
    begin
      lAdicionar := TTcpClassRegister.TcpClassRegister.Items[I].ProxyGroup <> C_GROUP_INTERNAL;
      if ASomentePublicos then
        lAdicionar := lAdicionar and TTcpClassRegister.TcpClassRegister.Items[I].PublicItem;

      if lAdicionar then
      begin
        lItem := TTcpClassRegisterItem.Create;
        lItem.ProxyClass := TTcpClassRegister.TcpClassRegister.Items[I].ProxyClass;
        lItem.ProxyGroup := TTcpClassRegister.TcpClassRegister.Items[I].ProxyGroup;
        lItem.LifeCycle  := TTcpClassRegister.TcpClassRegister.Items[I].LifeCycle;
        lItem.Mnemonic  := TTcpClassRegister.TcpClassRegister.Items[I].Mnemonic;
        ARegistrados.Add( lItem );
      end;
    end;
end;

class procedure TCPClassRegistrer.Registrate(const AClass : TRpDataFlashAbstractClass;
  const AGrupoProxy : string; const AMnemonico : string; const APublico : Boolean;
  const ALifeCycle : TRpDataFlashLifeCycle);
var
  lGrupoProxy: string;
begin
  if TTcpClassRegister.TcpClassRegister = nil then
    TTcpClassRegister.TcpClassRegister := TTcpClassRegister.Create;

  lGrupoProxy := AGrupoProxy;
  if lGrupoProxy = '' then
    lGrupoProxy := 'Default';

  TTcpClassRegister.TcpClassRegister.Registrate(AClass, lGrupoProxy, AMnemonico, APublico, ALifeCycle);
end;

class procedure TCPClassRegistrer.RegistrateDSProvider(const AClass: TRpDataFlashAbstractClass;
  const ALifeCycle : TRpDataFlashLifeCycle);
begin
  if TTcpClassRegister.TcpClassRegister = nil then
    TTcpClassRegister.TcpClassRegister := TTcpClassRegister.Create;
  TTcpClassRegister.TcpClassRegister.Registrate(AClass, C_GROUP_DATASET, '', False, ALifeCycle);
end;

{ TTcpClassRegister }

function TTcpClassRegister.GetClass(const AClassName: string): TRpDataFlashAbstractClass;
var
  lLifeCycle: TRpDataFlashLifeCycle;
begin
  Result := GetClass(AClassName, lLifeCycle);
end;

function TTcpClassRegister.GetClass(const AClassName: string;
  out ALifeCycle: TRpDataFlashLifeCycle): TRpDataFlashAbstractClass;
var
  I: Integer;
begin
  Result := nil;
  ALifeCycle := tlfInstance;
  for I := 0 to Count - 1 do
    if TRpDataFlashAbstractClass(Items[I].ProxyClass).ClassNameIs(AClassName) or (Items[I].Mnemonic = AClassName) then
    begin
      ALifeCycle := Items[I].LifeCycle;
      Result := TRpDataFlashAbstractClass(Items[I].ProxyClass);
      Exit;
    end;
end;

function TTcpClassRegister.GetItem(const Index : Integer) : TTcpClassRegisterItem;
begin
  Result := TTcpClassRegisterItem(Self.Get(Index));
end;

procedure TTcpClassRegister.Registrate(const AClass : TRpDataFlashAbstractClass;
  const AGrupoProxy : string; const AMnemonico : string; const APublico : Boolean;
  const ALifeCycle : TRpDataFlashLifeCycle);
var
  lItem : TTcpClassRegisterItem;
begin
  lItem := TTcpClassRegisterItem.Create;
  lItem.ProxyClass := AClass;
  lItem.ProxyGroup := AGrupoProxy;
  lItem.LifeCycle  := ALifeCycle;
  lItem.PublicItem := APublico;
  lItem.Mnemonic  := AClass.ClassName;
  if AMnemonico <> '' then
    lItem.Mnemonic  := AMnemonico;
  Add(lItem);
end;

{ TRpDataFlashParamValueInteger }

constructor TRpDataFlashParamValueInteger.Create;
begin
  inherited;
  FValueType := tvpInteger;
end;

function TRpDataFlashParamValueInteger.GetValue: Integer;
begin
  try
    Result := StrToIntDef(VarToStrDef(FValue, '0'), 0);
  except
    Result := 0;
  end;
end;

procedure TRpDataFlashParamValueInteger.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

{ TRpDataFlashParamValueString }

constructor TRpDataFlashParamValueString.Create;
begin
  inherited;
  FValueType := tvpString;
end;

function TRpDataFlashParamValueString.GetValue: String;
begin
  Result := VarToStrDef(FValue, EmptyStr);
end;

procedure TRpDataFlashParamValueString.SetValue(const Value: String);
begin
  FValue := Value;
end;

{ TRpDataFlashParamValueBoolean }

constructor TRpDataFlashParamValueBoolean.Create;
begin
  inherited;
  FValueType := tvpBoolean;
end;

function TRpDataFlashParamValueBoolean.GetValue: Boolean;
begin
  if (FValue = Null) or (FValue = Unassigned) then
    Result := False
  else
    try
      Result := FValue;
    except
      Result := False;
    end;
end;

procedure TRpDataFlashParamValueBoolean.SetValue(const Value: Boolean);
begin
  FValue := Value;
end;

function TRpDataFlashCommandParameterList.AddNew(const AName: string;
  const AParamType : TRpDataFlashParamType;
  const AValueType : TRpDataFlashParamValueClass): TRpDataFlashParamValue;
begin
  Result := AValueType.Create;
  Result.FParamType := AParamType;
  Result.FName := AName;
  AddNew(Result);
end;

{ TTcpClassRegisterItem }

function TTcpClassRegisterItem.GetProxyGroup: string;
begin
  Result := Trim( FProxyGroup );
  if Result = EmptyStr then
    Result := C_WITHOUT_GROUP;
end;

{ TRpDataFlashDataSetProviderCommand }

function TRpDataFlashDataSetProviderCommand.ApplyUpdates: Boolean;
var
  lStatements : TStringList;
  lTotal: Integer;
  i: Integer;
  lSQL: string;
begin
  lStatements := TStringList.Create;
  lStatements.Text := Param['SQLInstruct'].AsBase64;
  lTotal := StrToIntDef(Trim(lStatements[0]), 0);
//  Result := False;
  try
    if lTotal > 0 then
    begin
//      DoStartTransaction; //-> Client must start transaction
      for i := 0 to lTotal - 1 do
      begin
        lSQL := lStatements.Values[ IntToStr(i) ];
        lSQL := Algorithms.Base64DecompressedString( lSQL );
        ExecSQL( lSQL );
      end;
      Result := True;
    end
    else
      raise Exception.Create('Não existem valores para aplicar.');
  finally
    FreeAndNil( lStatements );
//    if Result then
//      DoCommit
//    else
//      DoRollback; //-> Client must finalize transaction
  end;
end;

constructor TRpDataFlashDataSetProviderCommand.Create;
begin
  inherited Create;
  FProviderClass := EmptyStr;
  FInfoQuery := False;
//  FParams := TDataSetParams.Create;
//  FSQLs := TRpDataFlashCustomProvider.Create;
end;

destructor TRpDataFlashDataSetProviderCommand.Destroy;
begin
//  FreeAndNil(FSQLs);
//  FreeAndNil(FParams);
  inherited;
end;

function TRpDataFlashDataSetProviderCommand.DoExecute: Boolean;
var
  lXmlData: string;
begin
  FProviderClass := Param['ProviderClass'].AsString;
  FOperation := TRpDataFlashDataSetOperations( Param['Operacao'].AsInteger );
  FApplyMaxErrors := Param['MaxErrors'].AsInteger;
  FInfoQuery := Param['csDesigning'].AsBoolean;

  // zera os retornos
  ResultParam['ApplyErrCount'].AsInteger := 0;
  ResultParam['XMLData'].AsBase64 := EmptyStr;

  // executa a operacao informada
  if FOperation = opdsSelect then
  begin
    Result := Select(Param['SQLInstruct'].AsBase64, lXmlData);
    if Result then
      ResultParam['XMLData'].AsBase64 := lXmlData;
  end
  else
    case FOperation of
      opdsApplyUpdates:      Result := ApplyUpdates;
      opdsStartTrans:        Result := DoStartTransaction;
      opdsCommit:            Result := DoCommit(False);
      opdsCommitRetaining:   Result := DoCommit(True);
      opdsRollback:          Result := DoRollback(False);
      opdsRollbackRetaining: Result := DoRollback(True);
      opdsPrepare:           Result := DoPrepareClient;
//      opdsSelect: ;
//      opdsInsert: ;
//      opdsDelete: ;
//      opdsUpdate: ;
    else
      Result := False;
    end;
end;

function TRpDataFlashDataSetProviderCommand.DoPrepareClient: Boolean;
var
  lSQL : TRpDataFlashCustomProvider;
begin
  lSQL := TRpDataFlashCustomProvider.Create;

  lSQL.SelectSQL.Text := GetSelectSQL;
  lSQL.InsertSQL.Text := GetInsertSQL;
  lSQL.UpdateSQL.Text := GetUpdateSQL;
  lSQL.DeleteSQL.Text := GetDeleteSQL;

  ResultParam['XMLData'].AsBase64 := lSQL.GetAsString;
  Result := True;
  FreeAndNil(lSQL);
end;

procedure TRpDataFlashDataSetProviderCommand.DoRegisterParams;
begin
  inherited;
  NewParam('Operacao', tvpInteger); // INS / UPD / DEL / SEL
  NewParam('SQLInstruct', tvpBase64, False);
  NewParam('MaxErrors', tvpInteger);
  NewParam('Params', tvpBase64);
  NewParam('csDesigning', tvpBoolean);

  // ou a classe registrada no servidor
  NewParam('ProviderClass', tvpString);

  NewResult('XMLData', tvpBase64);
  NewResult('ApplyErrCount', tvpInteger);
end;

{ TRpDataFlashTextCommand }

function TRpDataFlashTextCommand.DoExecute: Boolean;
begin
  Result := False;
  if Assigned(FOnExecuteMessage) then
    Result := FOnExecuteMessage(Self);
end;

{ TFileProxy }

function TRpFileProxy.LoadFromFile(const AFileName: string): Boolean;
var
  lStream: TMemoryStream;
begin
  if FileExists(AFileName) then
  begin
    FFileName := AFileName;

    lStream := TMemoryStream.Create;
    try
      lStream.LoadFromFile(AFileName);
      lStream.Position := 0;

      FFileStream.CopyFrom(lStream, lStream.Size);
      FFileStream.Position := 0;
    finally
      FreeAndNil(lStream);
    end;

    if (FFileID = '-1') or (FFileID = '') then
      FFileID := DoGenerateFileID;
  end;

  Result := FFileStream.Size > 0;
end;

procedure TRpFileProxy.CopyInfo(const ASource: IRpFileProxy);
begin
  FFileID := ASource.FileID;
  FFileName := ASource.FileName;
  FDeleteOnRecive := ASource.DeleteOnRecive;

  FreeAndNil(FFileStream);
  FFileStream := TStringStream.Create('');
  if Assigned(ASource.Stream) then
  begin
    ASource.Stream.Position := 0;
    FFileStream.CopyFrom(ASource.Stream, ASource.Stream.Size);
  end;
  FFileStream.Position := 0;
end;

constructor TRpFileProxy.Create;
begin
  FFileStream := TStringStream.Create('');
  FDeleteOnRecive := False;
end;

function TRpFileProxy.DecodeInfo(const AInfo: string): TRpDataFlashFtpFileInfo;
begin
  Result.Decode(AInfo);
end;

destructor TRpFileProxy.Destroy;
begin
  if FFileStream <> nil then
    FreeAndNil(FFileStream);
  inherited;
end;

function TRpFileProxy.DoGenerateFileID: string;
begin
  Randomize;
  Result := FormatDateTime('hhmmnn_zzz_ddmmyy_', Now)
          + Format('%6.6d', [Random(999999)]);
end;

function TRpFileProxy.FileSizeFmt: string;
begin
  Result := TRpFileProxy.GetFileSizeFmt(FileSize);
end;

function TRpFileProxy.Get(const ASupport: IRpDataFlashFileTransferSupport;
  const AFileID: string): Boolean;
var
  lFileStream : TFileStream;
  lTmpFile : string;
begin
  if AFileID <> EmptyStr then
    FFileID := AFileID;

  lTmpFile := ASupport.GetFileTransfer_TempDir
            + FFileID + '.tmp';

  ASupport.OnFileTransferLog('FileTransfer Get ID[' + FFileID + ']');

  lFileStream := TFileStream.Create(lTmpFile, fmOpenRead);
  try
    lFileStream.Position := 0;

    FreeAndNil(FFileStream);
    FFileStream := TStringStream.Create('');
    FFileStream.CopyFrom(lFileStream, lFileStream.Size);
    FFileStream.Position := 0;
    Result := True;
  finally
    FreeAndNil(lFileStream);
  end;
end;

function TRpFileProxy.GetDeleteOnRecive: Boolean;
begin
  Result := FDeleteOnRecive;
end;

function TRpFileProxy.GetFileID: string;
begin
  if FFileID = EmptyStr then
    FFileID := DoGenerateFileID;

  Result := FFileID;
end;

function TRpFileProxy.GetFileName: string;
begin
  Result := FFileName;
end;

function TRpFileProxy.GetFileSize: Int64;
begin
  if Assigned(FFileStream) then
    Result := FFileStream.Size
  else
    Result := 0;
end;

class function TRpFileProxy.GetFileSizeFmt(const ABytes: Int64): string;
const
  C_SIZE_DIV = 1024;
var
  lSize : Double;
begin
  lSize := ABytes;
  if lSize > C_SIZE_DIV then
  begin
    lSize := lSize / C_SIZE_DIV;
    if lSize > C_SIZE_DIV then
    begin
      lSize := lSize / C_SIZE_DIV;
      if lSize > C_SIZE_DIV then
      begin
        lSize := lSize / C_SIZE_DIV;
        Result := FormatFloat('#0.## GB', lSize);
      end
      else
        Result := FormatFloat('#0.## MB', lSize);
    end
    else
      Result := FormatFloat('#0.## KB', lSize);
  end
  else
    Result := FormatFloat('#0 bytes', lSize);
end;

function TRpFileProxy.GetStream: TStream;
begin
  Result := FFileStream;
end;

function TRpFileProxy.Put(const ASupport: IRpDataFlashFileTransferSupport): Boolean;
var
  lDestino : TFileStream;
  lNomeDestino : string;
begin
  try
    lNomeDestino := ASupport.GetFileTransfer_TempDir + FileID + '.tmp';
    lDestino := TFileStream.Create(lNomeDestino, fmCreate);
    lDestino.CopyFrom(FFileStream, FFileStream.Size);

    ASupport.FileTransfer_RegisterFile(FileID, FFileName);
    ASupport.OnFileTransferLog('FileTransfer Put ID[' + FFileID + '] FileName[' + FFileName + ']');

    Result := True;
  finally
    if Assigned(lDestino) then
      FreeAndNil(lDestino);
  end;
end;

function TRpFileProxy.Remove: Boolean;
begin
  if (FileName <> '') and FileExists(FileName) then
    Result := DeleteFile(PChar(FFileName))
  else
    Result := True;
end;

function TRpFileProxy.Load(const AFileStream: string) : string;
begin
  FreeAndNil(FFileStream);
  FFileStream := TStringStream.Create(AFileStream);
  FFileStream.Position := 0;
end;

function TRpFileProxy.Save: string;
begin
  Result := FFileStream.DataString;
end;

function TRpFileProxy.SaveToFile: Boolean;
begin
  if FFileName = EmptyStr then
    raise Exception.Create('Nenhum nome de arquivo foi informado.');

  Result := SaveToFile(FFileName);
end;

function TRpFileProxy.SaveToFile(const AFileName: string): Boolean;
var
  lMemoryStream: TMemoryStream;
begin
  FFileName := AFileName;
  lMemoryStream := TMemoryStream.Create;
  try
    lMemoryStream.CopyFrom(FFileStream, FFileStream.Size);
    lMemoryStream.SaveToFile(AFileName);
  finally
    lMemoryStream.Free;
  end;

  Result := FileExists(AFileName);
end;

procedure TRpFileProxy.SetDeleteOnRecive(const AValue: Boolean);
begin
  FDeleteOnRecive := AValue;
end;

procedure TRpFileProxy.SetFileID(const AValue: string);
begin
  FFileID := AValue;
end;

procedure TRpFileProxy.SetFileName(const AValue: string);
begin
  FFileName := AValue;
end;

{ TRpDataFlashBrowserCommand }

function TRpDataFlashBrowserCommand.DoGetSerializedParams: string;
var
  lBrowserPage: string;
begin
  lBrowserPage := GetBrowserPage;
  if lBrowserPage = '' then
    Result := inherited DoGetSerializedParams
  else
    Result := lBrowserPage;
end;

function TRpDataFlashBrowserCommand.GetBrowserPage: string;
begin
  Result := '';
end;

{ TRpDataFlashCommandParameterSetter }

procedure TRpDataFlashCommandParameterSetter.SetValueType(
  const AValueType: TRpDataFlashParamValueType);
begin
  FValueType := AValueType;
end;

initialization

finalization
  TCPClassRegistrer.ReleaseRegistrer;

end.
