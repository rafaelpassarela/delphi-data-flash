unit uRpDataFlash.Command;

//{$I ..\..\Common\src\RpInc.inc}

interface

uses
  Classes, SysUtils, Contnrs, XMLIntf, XMLDoc, DB, uRpDataFlash.Types, IdContext,
  Variants, ActiveX, StrUtils, uRpAlgorithms, uRpJsonBase, uRpDataFlash.ConvertUtils,
  uRpSerialization, Windows, IdCustomHttpServer, uRpResourceString;

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
    function Select(const ASelectSQL: string; out XMLData: string): Boolean;
    function ExecuteSQL(const ASQL: string): Boolean;
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
    procedure SetAutenticado(const Value : Boolean);
    function GetAutenticado : Boolean;

    function Ip : string;
    property UserName : string read GetUserName write SetUserName;
    property Password : string read GetPassword write SetPassword;
    property Authenticated : Boolean read GetAutenticado write SetAutenticado;
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

    function ExecuteRemoteError(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteRemoteSuccessfully(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteBeforeRemoteConnection(const AParams: TRpDataFlashCommandParameterList;
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
    procedure SetConexaoItem(const AConexaoItem: IAutenticationProvider);

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
    function GetAsString: string;
    function GetAsFloat: Double;
    function GetAsBoolean: Boolean;
    function GetAsBase64: string;
    function GetAsInteger: Integer;
    function GetAsVariant: string;
    function GetAsDateTime: TDateTime;
    function GetAsJSONString: string;
    function GetAsBinaryFile: TRpFileProxy;
    function GetAsObject : TCustomSerializableObject;
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
    property AsObject : TCustomSerializableObject read GetAsObject;

    procedure LoadFromFile(const AArquivo : string);
    procedure SaveToFile(const AArquivo : string);

    procedure ToNode(const ANode : IXMLNode);
    procedure FromNode(const ANode : IXMLNode);
    function ToJson : string;

    procedure Assign(const Source: TRpDataFlashCommandParameter); reintroduce;
  end;

  TLRDataFlashValorParametro = class(TRpDataFlashCommandParameter)
  end;

  TLRDataFlashValorParametroClass = class of TLRDataFlashValorParametro;

  TLRDataFlashValorParametroInteger = class(TLRDataFlashValorParametro)
  private
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  public
    constructor Create; override;
    property Value : Integer read GetValue write SetValue;
  end;

  TLRDataFlashValorParametroString = class(TLRDataFlashValorParametro)
  private
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    constructor Create; override;
    property Value : String read GetValue write SetValue;
  end;

  TLRDataFlashValorParametroBoolean = class(TLRDataFlashValorParametro)
  private
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
  public
    constructor Create; override;
    property Value : Boolean read GetValue write SetValue;
  end;

  TRpDataFlashCommandParameterList = class(TPersistent)
  private
    FParametros : TObjectList;
    FComando: string;
    FSerializationFormat: TSerializationFormat;
    FFileTransferSupport: IRpDataFlashFileTransferSupport;
    function GetParametro(const AName: string; const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
    function GetParametroIdx(const Index: Integer) : TRpDataFlashCommandParameter;
    function GetStatusProcessamento: TRpDataFlashProcessingStatus;
    procedure SetStatusProcessamento(const Value: TRpDataFlashProcessingStatus);
    function GetCount : Integer;
  protected
    procedure ToNode(const ANode : IXMLNode); virtual;
    procedure FromNode(const ANode : IXMLNode); virtual;
    function ToJson : string;
    procedure DeJson(const AJsonPair : TJSONPair); virtual;
  public
    constructor Create(const AFileTransferSupport : IRpDataFlashFileTransferSupport); virtual;
    destructor Destroy; override;
    procedure Assign(const Source: TRpDataFlashCommandParameterList); reintroduce;

    procedure Novo(const AParametro : TRpDataFlashCommandParameter); overload;
    function Novo(const AName : string; AValue : Variant; const AParamType : TRpDataFlashParamType;
      const AValueType : TRpDataFlashParamValueType) : TRpDataFlashCommandParameter; overload;
    function Novo(const AName: string; const AParamType : TRpDataFlashParamType;
      const AValueType : TLRDataFlashValorParametroClass) : TLRDataFlashValorParametro; overload;
    function Novo(const AName: string; const AParamType : TRpDataFlashParamType;
      const AClasseBase : string) : TRpDataFlashCommandParameter; overload;

    procedure AddParametro(const AName : string; const AValue : Variant; const AParamType : TRpDataFlashParamValueType);
    procedure AddRetorno(const AName : string; AValue : Variant; const AParamType : TRpDataFlashParamValueType);

    function PorNome(const AName : string) : TRpDataFlashCommandParameter; overload;
    function PorNome(const AName : string; const AParamType : TRpDataFlashParamType) : TRpDataFlashCommandParameter; overload;

    function Serializar : string;
    procedure Carregar(const AParams : string);

    property ParametroInterno[const AName : string] : TRpDataFlashCommandParameter index tpInternal read GetParametro;
    property Parametro[const AName : string] : TRpDataFlashCommandParameter index tpInput read GetParametro;
    property Retorno[const AName : string] : TRpDataFlashCommandParameter index tpOutput read GetParametro;
    property Item[const Index : Integer] : TRpDataFlashCommandParameter read GetParametroIdx; default;

    property Command : string read FComando write FComando;
    property StatusProcessamento : TRpDataFlashProcessingStatus read GetStatusProcessamento write SetStatusProcessamento;
    property Count : Integer read GetCount;
    property SerializationFormat : TSerializationFormat read FSerializationFormat write FSerializationFormat;
  end;

  TRpDataFlashCommand = class(TInterfacedObject, IRpDataFlashCommandInterfaced)
  private
    FParametros: TRpDataFlashCommandParameterList;
    FContext: TIdContext;
    FOnCallBackEvent: TRpDataFlashCallBackEvent;
    FLifeCycle: TRpDataFlashLifeCycle;
    FServerInstanceController: IServerInstanceController;
    FSessionInstanceController: ISessionInstanceController;
    FLock: Boolean;
    FRequestInfo: TIdHTTPRequestInfo;
    FResponseInfo: TIdHTTPResponseInfo;
    function GetParametro(const AName: string; const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
    function GetParams: TRpDataFlashCommandParameterList;
    function GetStatusProcessamento: TRpDataFlashProcessingStatus;
    function GetDescription: string;
    procedure SetStatusProcessamento(const Value: TRpDataFlashProcessingStatus);
    procedure DoLoad(const ALoadType : TRpDataFlashLoadType; const AParams : TRpDataFlashCommandParameterList); overload; virtual;
    procedure DoSerialize(const AParams : TRpDataFlashCommandParameterList); overload;
    procedure DoExecutionError(const AParams: TRpDataFlashCommandParameterList); overload;
    procedure DoRegisterParams(const AParams: TRpDataFlashCommandParameterList); overload;

    function Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor) : string; overload;
    function Execute(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      const AParamType : TRpDataFlashExecutionType; var AContinue : Boolean) : string; overload; virtual;

    function ExecuteRemoteError(const AParams: TRpDataFlashCommandParameterList; AExecutor: IRpPackageCommandExecutor;
      var AContinue : Boolean): string; overload;
    function ExecuteRemoteSuccessfully(const AParams : TRpDataFlashCommandParameterList; AExecutor : IRpPackageCommandExecutor;
      var AContinue : Boolean) : string;
    function ExecuteBeforeRemoteConnection(const AParams: TRpDataFlashCommandParameterList;
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
    FConexaoItem: IAutenticationProvider;
    FSerealizationFormat: TSerializationFormat;

    function GetCommand: string; virtual;
    function DoCallBack(var AParamsCallback : TRpDataFlashCommandParameterList) : Boolean; virtual;
    function DoExecutar : Boolean; virtual; abstract;
    function DoGetDescricao: string; virtual;
    function FindParametro(const AName: string): TRpDataFlashCommandParameter;
    function FindRetorno(const AName: string): TRpDataFlashCommandParameter;
    function GetProcessType : TRpDataFlashProcessType; virtual;
    function InternalCarregarComando(const AComando : TRpDataFlashCommandClass;
      out AObjComando : IRpDataFlashCommandInterfaced;
      out AParams : TRpDataFlashCommandParameterList) : Boolean;
    function GetObject: TObject;
    function GetLifeCycle: TRpDataFlashLifeCycle; virtual;
    function GetResponseFileName: string; virtual;
    function DoGetParametrosSerializados : string; virtual;

    procedure DoSerialize; overload; virtual;
    procedure DoLoad; overload; virtual;
    procedure DoExecutionError(const AErrorMsg : string); overload; virtual;
    procedure DoValidarParametros; virtual;
    procedure DoRegisterParams; overload; virtual;
    procedure DoExecutarPonteInvalida(var AContinue : Boolean); virtual;
    procedure DoExecutarPonteBemSucedida(var AContinue : Boolean); virtual;
    procedure DoExecutarAntesComunicarPonte(var AContinue : Boolean); virtual;
    procedure NovoParametro(const AName : string; const AParamType : TRpDataFlashParamValueType; const ARecarregar : Boolean = True); overload;
    procedure NovoParametro(const AName : string; const ABaseClass : TCustomSerializableObjectClass; const ARecarregar : Boolean = True); overload;
    procedure NovoRetorno(const AName : string; const AParamType : TRpDataFlashParamValueType); overload;
    procedure NovoRetorno(const AName : string; const ABaseClass : TCustomSerializableObjectClass; const ARecarregar : Boolean = True); overload;
    procedure NovoParamInterno(const AName : string; const AParamType : TRpDataFlashParamValueType); overload;
    procedure SetLifeCycle(const ALifeCycle: TRpDataFlashLifeCycle);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetServer(const AServer: TComponent);

    procedure SetConexaoItem(const AConexaoItem: IAutenticationProvider);

    function GetRequestInfo: TIdHTTPRequestInfo;
    procedure SetRequestInfo(const AValue: TIdHTTPRequestInfo);
    function GetResponseInfo: TIdHTTPResponseInfo;
    procedure SetResponseInfo(const AValue: TIdHTTPResponseInfo);

    function SendCallBack: Boolean;
    procedure SetCallBackEvent(const AContext: TIdContext;const ACallBackEvent: TRpDataFlashCallBackEvent);
    function Serializar : string;
    procedure Carregar(const AComando : string);

    function ReturnStatus : Boolean;
    function LastError : string;

    property Command: string read GetCommand;
    property Parametros : TRpDataFlashCommandParameterList read GetParams;
    property Parametro[const AName : string] : TRpDataFlashCommandParameter index tpInput read GetParametro;
    property Retorno[const AName : string] : TRpDataFlashCommandParameter index tpOutput read GetParametro;
    property ProcessType : TRpDataFlashProcessType read GetProcessType;
    property StatusProcessamento : TRpDataFlashProcessingStatus read GetStatusProcessamento write SetStatusProcessamento;
    property Executor : IRpPackageCommandExecutor read GetExecutor write SetExecutor;
    property Server : TComponent read FServer;
    property ServerInstanceController : IServerInstanceController read GetServerInstanceController write SetServerInstanceController;
    property SessionInstanceController : ISessionInstanceController read GetSessionInstanceController write SetSessionInstanceController;

    property LifeCycle : TRpDataFlashLifeCycle read GetLifeCycle write SetLifeCycle;
    property Lock : Boolean read GetLock write SetLock;
    property SerializationFormat : TSerializationFormat read GetSerializationFormat write SetSerializationFormat;
    property RequestInfo  : TIdHTTPRequestInfo read GetRequestInfo write SetRequestInfo;
    property ResponseInfo : TIdHTTPResponseInfo read GetResponseInfo write SetResponseInfo;

    class function CarregarComando(
      const AComando : string;
      out AObjComando : IRpDataFlashCommandInterfaced;
      out AParams : TRpDataFlashCommandParameterList;
      const AServerInstanceController : IServerInstanceController = nil;
      const ASessionInstanceController :ISessionInstanceController = nil;
      const ALoadParams : Boolean = True) : Boolean; overload;

    class function CarregarComando(
      const ACommandClass : string;
      out AObjComando : IRpDataFlashCommandInterfaced;
      out ALifeCycle: TRpDataFlashLifeCycle;
      const AServerInstanceController : IServerInstanceController;
      const ASessionInstanceController : ISessionInstanceController) : Boolean; overload;
  end;

  TLRDataFlashComandoEnvio = class(TRpDataFlashCommand)
  private
    FComando : string;
  protected
    function DoExecutar : Boolean; override;
  public
    function GetCommand: string; override;
    procedure SetComando(const AComando : string); overload;
    procedure SetComando(const AComando : TRpDataFlashCommandClass); overload;
  end;

  TLRDataFlashComandoBrowser = class(TRpDataFlashCommand)
  protected
    function GetBrowserPage : string; virtual;
    function DoGetParametrosSerializados : string; override;
  end;

  TLRDataFlashComandoTexto = class(TRpDataFlashCommand)
  private
    FOnExecutarMensagem: TRpDataFlashOnExecuteMessage;
  protected
    function DoExecutar : Boolean; override;
  public
    property OnExecutarMensagem : TRpDataFlashOnExecuteMessage read FOnExecutarMensagem write FOnExecutarMensagem;
  end;

  IFileProxy = interface
  ['{1F83A425-C29D-486D-9D39-7FAC3FF83EE6}']
    function Save : string;                             // conteudo do arquivo
    function Load(const AFileStream : string) : string; // carrega o conteudo do arquivo

    function SaveToFile(const AFileName :string) : Boolean; overload;   // salva em um arquivo
    function SaveToFile : Boolean; overload;   // salva em um arquivo
    function LoadFromFile(const AFileName : string) : Boolean; // carrega o conteudo de um arquivo

    function Get(const ASupport: IRpDataFlashFileTransferSupport;
      const AFileID : string = ''): Boolean; // busca o arquivo no servidor
    function Put(const ASupport : IRpDataFlashFileTransferSupport) : Boolean; // prepara o arquivo para envio
    function Remove : Boolean; // apaga o arquivo temporario

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
    procedure CopyInfo(const ASource : IFileProxy);
  end;

  TRpFileProxy = class(TInterfacedPersistent, IFileProxy)
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
    procedure CopyInfo(const ASource : IFileProxy);

    property FileName : string read GetFileName write SetFileName;
    property FileID : string read GetFileID write SetFileID;

    property FileStream : TStringStream read FFileStream;
    property FileSize : Int64 read GetFileSize;
    function FileSizeFmt : string;

    property DeleteOnRecive : Boolean read GetDeleteOnRecive write SetDeleteOnRecive;
    property Stream : TStream read GetStream;

    class function GetFileSizeFmt(const ABytes : Int64) : string;
  end;

  TLRDataFlashComandoDataSetProvider = class(TRpDataFlashCommand)
  private
    FProviderClass: string;
    FOperacao: TRpDataFlashDataSetOperations;
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
    function DoExecutar : Boolean; override; final;
    procedure DoRegisterParams; override; final;
  public
    constructor Create; override;
    destructor Destroy; override;

    property ProviderClass : string read FProviderClass;
    property Operacao : TRpDataFlashDataSetOperations read FOperacao;
    property ApplyMaxErrors : Cardinal read FApplyMaxErrors write FApplyMaxErrors;
    property InfoQuery : Boolean read FInfoQuery;
  end;

  TTcpClassRegisterItem = class
  private
    FClass : TRpDataFlashAbstractClass;
    FProxyGroup : string;
    FLifeCycle: TRpDataFlashLifeCycle;
    FPublicItem: Boolean;
    FMnemonico: string;
    function GetProxyGroup: string;
  public
    property ProxyClass : TRpDataFlashAbstractClass read FClass write FClass;
    property ProxyGroup : string read GetProxyGroup write FProxyGroup;
    property LifeCycle : TRpDataFlashLifeCycle read FLifeCycle write FLifeCycle;
    property PublicItem : Boolean read FPublicItem write FPublicItem;
    property Mnemonico : string read FMnemonico write FMnemonico;
  end;

  TTcpClassRegister = class(TObjectList)
  protected
    function GetItem(const Index : Integer) : TTcpClassRegisterItem;
  public
    class var TcpClassRegister: TTcpClassRegister;
    procedure Registrar(const AClass : TRpDataFlashAbstractClass; const AGrupoProxy : string;
      const AMnemonico : string = ''; const APublico : Boolean = False;
      const ALifeCycle : TRpDataFlashLifeCycle = tlfInstance);
    function GetClass(const AClassName : string) : TRpDataFlashAbstractClass; overload;
    function GetClass(const AClassName : string; out ALifeCycle : TRpDataFlashLifeCycle) : TRpDataFlashAbstractClass; overload;
    property Items[const Index: Integer]: TTcpClassRegisterItem read GetItem; default;
  end;

  TCPClassRegistrer = class
  public
    class procedure Destruir;
    class procedure Registrar(const AClass : TRpDataFlashAbstractClass; const AGrupoProxy : string;
      const AMnemonico : string = ''; const APublico : Boolean = False;
      const ALifeCycle : TRpDataFlashLifeCycle = tlfInstance);
    class procedure RegistrarDSProvider(const AClass : TRpDataFlashAbstractClass;
      const ALifeCycle : TRpDataFlashLifeCycle);
    class procedure Registrados(out ARegistrados : TTcpClassRegister; const ASomentePublicos : Boolean = False);
    class function GetClass(const AClassName : string) : TRpDataFlashAbstractClass; overload;
    class function GetClass(const AClassName : string; out ALifeCycle : TRpDataFlashLifeCycle) : TRpDataFlashAbstractClass; overload;
//    class procedure Instanciar(const AClass : TLRDataFlashAbstractClass; const ANomeInstancia : string);
  end;

implementation

{ TLRDataFlashParametroComando }

procedure TRpDataFlashCommandParameter.Assign(const Source: TRpDataFlashCommandParameter);
begin
  if not Source.InheritsFrom(Self.ClassType) then
    raise Exception.Create('Classe de origem n�o permitida');

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

function TRpDataFlashCommandParameter.GetAsObject: TCustomSerializableObject;
var
  lClasseBase: TCustomSerializableObjectClass;
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

function TRpDataFlashCommandParameter.ToJson: string;
var
  lFormatStr: string;
begin
//      "InternalStatusProcessamento": {
//         "Tipo": "2",
//         "TipoValor": "0",
//         "Valor": "4"
//      },

  FValue := StringReplace(FValue, '\', '\\', [rfReplaceAll]);

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
    FValue]);
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

{ TLRDataFlashParametrosComando }

procedure TRpDataFlashCommandParameterList.AddParametro(const AName: string;
  const AValue: Variant; const AParamType: TRpDataFlashParamValueType);
begin
  Novo(AName, AValue, tpInput, AParamType);
end;

procedure TRpDataFlashCommandParameterList.AddRetorno(const AName: string;
  AValue: Variant; const AParamType: TRpDataFlashParamValueType);
begin
  Novo(AName, AValue, tpOutput, AParamType);
end;

procedure TRpDataFlashCommandParameterList.Assign(const Source: TRpDataFlashCommandParameterList);
var
  I: Integer;
  lParam: TRpDataFlashCommandParameter;
  lSourceParam: TRpDataFlashCommandParameter;
begin
  if Source <> Self then
  begin
    for I := 0 to Source.FParametros.Count - 1 do
    begin
      lSourceParam := TRpDataFlashCommandParameter(Source.FParametros[I]);
      lParam       := PorNome(lSourceParam.Name);

      if Assigned(lParam) then
        lParam.Assign(lSourceParam)
      else
      begin
        if (lSourceParam.ValueType = tvpBase) and (lSourceParam.BaseClass <> EmptyStr) then
          Novo(lSourceParam.Name, lSourceParam.ParamType, lSourceParam.BaseClass).AsVariant := lSourceParam.Value
        else
          Novo(lSourceParam.Name, lSourceParam.Value, lSourceParam.ParamType, lSourceParam.ValueType);
      end;
    end;
  end;
end;

procedure TRpDataFlashCommandParameterList.Carregar(const AParams: string);

  procedure InternalLoadXML;
  var
    lXML: IXMLDocument;
    lNodoRoot: IXMLNode;
    lNodoComando: IXMLNode;
    lStream: TStringStream;
    lNodoParametros: IXMLNode;
  begin
    Novo(C_PARAM_INT_FORMAT_TYPE, Ord(sfXML), tpInternal, tvpInteger);

    lStream := TStringStream.Create(AParams);
    try
      try
        lXML := TXMLDocument.Create(nil);

        lXML.LoadFromStream(lStream);

        lNodoRoot := lXML.ChildNodes.FindNode('root');
        lNodoComando := lNodoRoot.ChildNodes.FindNode('TCPComando');

        FComando := lNodoComando.Attributes['Comando'];

        if FComando = '' then
          raise Exception.Create('Carga. Comando n�o pode ser vazio !');

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
      Novo(C_PARAM_INT_FORMAT_TYPE, Ord(sfJSON), tpInternal, tvpInteger);

      lJsonObj := TJSONObject.Create;
      try
        lJsonObj.Parse(BytesOf(AParams), 0);
        lPair := lJsonObj.Get('Comando');
        if lPair <> nil then
          FComando := StringReplace(lPair.FieldValue, '"', '', [rfReplaceAll]);

        if FComando = '' then
          raise Exception.Create('Carga. Comando n�o pode ser vazio !');

        lPair := lJsonObj.Get('Parametros');
        if lPair <> nil then
          DeJson(lPair);
      finally
        FreeAndNil(lJsonObj);
      end;
    except on E:Exception do
      raise Exception.Create('Erro ao carregar mensagem : ' + sLineBreak +
        '(J) : ' +  AParams);
    end;
  end;

begin
  FComando := '';
  if AParams <> EmptyStr then
  begin
    if AParams[1] = '<' then
      InternalLoadXML
    else
      InternalLoadJson
  end
  else
    raise Exception.Create('Nenhum par�metro foi recebido.');
end;

constructor TRpDataFlashCommandParameterList.Create(const AFileTransferSupport : IRpDataFlashFileTransferSupport);
begin
  FParametros := TObjectList.Create;
  FFileTransferSupport := AFileTransferSupport;
  Novo( C_PARAM_INT_STATUS_PROC , EmptyStr, tpInternal, tvpInteger);
end;

procedure TRpDataFlashCommandParameterList.DeJson(const AJsonPair: TJSONPair);
var
  i: Integer;
  lTipo: TRpDataFlashParamType;
  lTipoValor: TRpDataFlashParamValueType;
  lJson : TJSONObject;
  lValues : TJSONObject;
  lPair: TJSONPair;
  lName: string;
  lValueStr: string;

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
      lValueStr := lValues.Get('Valor').FieldValue;
      if (lValueStr <> '') and (lValueStr[1] = '"') then
        Delete(lValueStr, 1, 1);

      if (lValueStr <> '') and (lValueStr[ Length(lValueStr) ] = '"') then
        Delete(lValueStr, Length(lValueStr), 1);

// J� retorna sem a segunda bara
//      lValueStr := StringReplace(lValueStr, '\\', '\', [rfReplaceAll]);

      Novo(lName, lValueStr, lTipo, lTipoValor);
    end;
  except
    raise Exception.Create('Erro carregando par�metros JSON.');
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
      Novo(lNodo.NodeName, lTipo, lClasse);

    Novo(lNodo.NodeName, lNodo.NodeValue, lTipo, lTipoValor);
  end;
end;

destructor TRpDataFlashCommandParameterList.Destroy;
begin
  FreeAndNil(FParametros);
  inherited;
end;

function TRpDataFlashCommandParameterList.GetCount: Integer;
begin
  if Assigned(FParametros) then
    Result := FParametros.Count
  else
    Result := -1;
end;

function TRpDataFlashCommandParameterList.GetParametro(const AName: string;
  const AParamType : TRpDataFlashParamType): TRpDataFlashCommandParameter;
var
  lParametro: TRpDataFlashCommandParameter;
begin
  lParametro := PorNome(AName, AParamType);

  if lParametro = nil then
    raise ERpDataFlashParamNotFound.Create('Par�metro ''' + AName + ''' n�o encontrado!');

  Result := lParametro;
end;

function TRpDataFlashCommandParameterList.GetParametroIdx(const Index: Integer): TRpDataFlashCommandParameter;
begin
  try
    Result := TRpDataFlashCommandParameter(FParametros[Index]);
  except
    Result := nil;
  end;
end;

function TRpDataFlashCommandParameterList.GetStatusProcessamento: TRpDataFlashProcessingStatus;
begin
  Result := TRpDataFlashProcessingStatus(ParametroInterno[C_PARAM_INT_STATUS_PROC].AsInteger);
end;

function TRpDataFlashCommandParameterList.Novo(const AName: string;
  const AParamType: TRpDataFlashParamType;
  const AClasseBase: string): TRpDataFlashCommandParameter;
begin
  Result := TRpDataFlashCommandParameter.Create(Self, AName, '', AParamType, tvpBase, AClasseBase);
  Novo(Result);
end;

procedure TRpDataFlashCommandParameterList.Novo(const AParametro: TRpDataFlashCommandParameter);
begin
  if not Assigned(PorNome(AParametro.Name)) then
    FParametros.Add(AParametro);
end;

function TRpDataFlashCommandParameterList.Novo(const AName: string; AValue: Variant; const AParamType : TRpDataFlashParamType;
  const AValueType : TRpDataFlashParamValueType) : TRpDataFlashCommandParameter;
begin
  Result := PorNome(AName, AParamType);
  if not Assigned(Result) then
  begin
    Result := TRpDataFlashCommandParameter.Create(Self, AName, AValue, AParamType, AValueType);
    Novo(Result);
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

function TRpDataFlashCommandParameterList.ToJson: string;
var
  I: Integer;
  lParametros: string;
begin
  lParametros := '';
  for I := 0 to FParametros.Count - 1 do
  begin
    if lParametros <> '' then
      lParametros := lParametros + ',';

    lParametros := lParametros + TRpDataFlashCommandParameter(FParametros[I]).ToJson;
  end;

  Result := '"Parametros": {' + lParametros + '}';
end;

procedure TRpDataFlashCommandParameterList.ToNode(const ANode: IXMLNode);
var
  I: Integer;
begin
  for I := 0 to FParametros.Count - 1 do
    TRpDataFlashCommandParameter(FParametros[I]).ToNode(ANode);
end;

function TRpDataFlashCommandParameterList.PorNome(const AName: string;
  const AParamType: TRpDataFlashParamType): TRpDataFlashCommandParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FParametros.Count - 1 do
  begin
    if ((TRpDataFlashCommandParameter(FParametros[I]).ParamType in [AParamType, tpInternal, tpInputNoReload]))
    and (TRpDataFlashCommandParameter(FParametros[I]).Name = AName) then
    begin
      Result := TRpDataFlashCommandParameter(FParametros[I]);
      Exit;
    end;
  end;
end;

function TRpDataFlashCommandParameterList.Serializar: string;
var
  lXML: IXMLDocument;
  lXmlNode: IXMLNode;
  lStream: TStringStream;
begin
  if FComando = '' then
    raise Exception.Create('Serializa��o. Comando n�o pode ser vazio!');

  if SerializationFormat = sfUnknown then
    SerializationFormat := TSerializationFormat(Parametro[C_PARAM_INT_FORMAT_TYPE].AsInteger);

  // mantem como padrao o JSON
  if SerializationFormat in [sfUnknown, sfJSON] then
  begin
    Result := '{"Comando":"' + FComando + '",';
    Result := Result
            + ToJson
            + '}';
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

        lXmlNode.Attributes['Comando'] := FComando;
        lXmlNode := lXmlNode.AddChild('Parametros');

        ToNode(lXmlNode);
        Result := lXML.XML.Text;
        {$IFDEF UNICODE}
        Result := StringReplace(Result, 'UTF-16', 'iso-8859-1', []);
        {$ENDIF}
      end
      else
        raise Exception.Create('Serializa��o. N�o foi poss�vel ler o XML do comando.');
    finally
      FreeAndNil( lStream );
    end;
  end
end;

procedure TRpDataFlashCommandParameterList.SetStatusProcessamento(const Value: TRpDataFlashProcessingStatus);
begin
  Parametro[C_PARAM_INT_STATUS_PROC].AsInteger := Ord(Value);
end;

function TRpDataFlashCommandParameterList.PorNome(const AName: string): TRpDataFlashCommandParameter;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FParametros.Count - 1 do
  begin
    if TRpDataFlashCommandParameter(FParametros[I]).Name = AName then
    begin
      Result := TRpDataFlashCommandParameter(FParametros[I]);
      Exit;
    end;
  end;
end;

{ TLRDataFlashComando }

procedure TRpDataFlashCommand.Carregar(const AComando: string);
begin
  FParametros.Carregar(AComando);
  DoLoad;
end;

class function TRpDataFlashCommand.CarregarComando(
  const AComando : string;
  out AObjComando : IRpDataFlashCommandInterfaced;
  out AParams : TRpDataFlashCommandParameterList;
  const AServerInstanceController : IServerInstanceController;
  const ASessionInstanceController :ISessionInstanceController;
  const ALoadParams : Boolean): Boolean;
var
  lLifeCycle: TRpDataFlashLifeCycle;
begin
  AParams := TRpDataFlashCommandParameterList.Create(nil);
  AParams.Carregar(AComando);

  AObjComando := nil;

  TRpDataFlashCommand.CarregarComando(
    AParams.Command,
    AObjComando,
    lLifeCycle,
    AServerInstanceController,
    ASessionInstanceController);

  if ALoadParams and Assigned(AObjComando) then
    AObjComando.DoLoad(loSend, AParams);

  Result := AObjComando <> nil;
end;

class function TRpDataFlashCommand.CarregarComando(const ACommandClass: string;
  out AObjComando: IRpDataFlashCommandInterfaced;
  out ALifeCycle: TRpDataFlashLifeCycle;
  const AServerInstanceController: IServerInstanceController;
  const ASessionInstanceController: ISessionInstanceController): Boolean;
var
  lComandoClass: TRpDataFlashAbstractClass;
begin
  lComandoClass := TCPClassRegistrer.GetClass(ACommandClass, ALifeCycle);
  if (lComandoClass <> nil) then
  begin
    if Supports(lComandoClass, IRpDataFlashCommandInterfaced) then
    begin
      if ALifeCycle = tlfSession then
        AObjComando := ASessionInstanceController.FindInstance(ACommandClass)
      else if ALifeCycle = tlfServer then
        AObjComando := AServerInstanceController.FindInstance(ACommandClass);

      if AObjComando = nil then
      begin
        if lComandoClass.InheritsFrom(TComponent) then
          AObjComando := (TComponentClass(lComandoClass).Create(nil) as IRpDataFlashCommandInterfaced)
        else
          if lComandoClass.InheritsFrom(TRpDataFlashCommand) then
            AObjComando := (TRpDataFlashCommandClass(lComandoClass).Create as IRpDataFlashCommandInterfaced)
          else
            AObjComando  := (TRpDataFlashCommand(lComandoClass.Create) as IRpDataFlashCommandInterfaced);
        AObjComando.LifeCycle := ALifeCycle;

        AObjComando.ServerInstanceController := AServerInstanceController;
        AObjComando.SessionInstanceController := ASessionInstanceController;
      end;

      if ALifeCycle = tlfSession then
        ASessionInstanceController.AddInstance(AObjComando)
      else
        if ALifeCycle = tlfServer then
          AServerInstanceController.AddInstance(AObjComando);
    end;
  end;
  Result := AObjComando <> nil;
end;

constructor TRpDataFlashCommand.Create;
begin
  // inicializados na primeira execucao (e atualziados a cada execucao)
  FServer := nil;
  FConexaoItem := nil;

  FLock := False;
  FParametros := TRpDataFlashCommandParameterList.Create(nil);
  FParametros.Command := GetCommand;
  FSerealizationFormat := sfJSON;
  //FStatusProcessamento := tspServidor;
  DoRegisterParams;

  FParametros.StatusProcessamento := psNone;
end;

destructor TRpDataFlashCommand.Destroy;
begin
  FreeAndNil(FParametros);
  FConexaoItem := nil;
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
  FParametros.Assign(AParams);
  DoExecutionError(EmptyStr);
end;

procedure TRpDataFlashCommand.DoExecutionError(const AErrorMsg : string);
begin
  Retorno[C_PARAM_INT_EXCEPTION].AsString := AErrorMsg;
end;

procedure TRpDataFlashCommand.DoExecutarAntesComunicarPonte(var AContinue: Boolean);
begin
//dummy
end;

procedure TRpDataFlashCommand.DoExecutarPonteBemSucedida(var AContinue: Boolean);
begin
//dummy
end;

procedure TRpDataFlashCommand.DoExecutarPonteInvalida(var AContinue : Boolean);
begin
//dummy
end;

function TRpDataFlashCommand.DoGetDescricao: string;
begin
  Result := R_DATAFLASH_CMD_NO_DESCRIPTION;
end;

function TRpDataFlashCommand.DoGetParametrosSerializados: string;
begin
  Result := FParametros.Serializar;
end;

procedure TRpDataFlashCommand.DoRegisterParams;
begin
  NovoParamInterno(C_PARAM_INT_FORMAT_TYPE, tvpInteger);
  NovoParamInterno(C_PARAM_INT_STATUS_RET, tvpBoolean);
  NovoParamInterno(C_PARAM_INT_EXCEPTION, tvpString);
end;

procedure TRpDataFlashCommand.DoRegisterParams(
  const AParams: TRpDataFlashCommandParameterList);
begin
  FParametros.Assign(AParams);
  DoRegisterParams;
end;

procedure TRpDataFlashCommand.DoLoad(const ALoadType : TRpDataFlashLoadType;
  const AParams: TRpDataFlashCommandParameterList);
begin
//  FParametros := AParametros;
  FParametros.Assign(AParams);
  DoLoad;
end;

procedure TRpDataFlashCommand.DoSerialize(const AParams: TRpDataFlashCommandParameterList);
begin
//  FParametros := AParametros;
  FParametros.SerializationFormat := Self.SerializationFormat;
  FParametros.Assign(AParams);
  DoSerialize;
end;

procedure TRpDataFlashCommand.DoSerialize;
begin
//dummy
end;

procedure TRpDataFlashCommand.DoValidarParametros;
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
      Result := FOnCallBackEvent(FContext, lParams.Serializar);
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
    FParametros.Assign(AParams);
    FExecutor := AExecutor;
    lRetornoPositivo := False;
    try
      DoValidarParametros;
      DoLoad;

      case AParamType of
      etExecution:
        lRetornoPositivo := DoExecutar;
      etBridgeInvalid:
        begin
          DoExecutarPonteInvalida(AContinue);
          lRetornoPositivo := AContinue;
        end;
      etBridgeDone:
        begin
          DoExecutarPonteBemSucedida(AContinue);
          lRetornoPositivo := AContinue;
        end;
      etBeforeExecBridge:
        begin
          DoExecutarAntesComunicarPonte(AContinue);
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
    Retorno[C_PARAM_INT_STATUS_RET].AsBoolean := lRetornoPositivo;
    Result := Serializar;
  finally
    Lock := False;
  end;
end;

function TRpDataFlashCommand.ExecuteBeforeRemoteConnection(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBeforeExecBridge, AContinue);
end;

function TRpDataFlashCommand.ExecuteRemoteSuccessfully(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue: Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBridgeDone, AContinue);
end;

function TRpDataFlashCommand.ExecuteRemoteError(
  const AParams: TRpDataFlashCommandParameterList;
  AExecutor: IRpPackageCommandExecutor; var AContinue : Boolean): string;
begin
  Result := Execute(AParams, AExecutor, etBridgeInvalid, AContinue);
end;

function TRpDataFlashCommand.FindParametro(const AName: string): TRpDataFlashCommandParameter;
begin
  Result := FParametros.PorNome(AName, tpInput);
end;

function TRpDataFlashCommand.FindRetorno(const AName: string): TRpDataFlashCommandParameter;
begin
  Result := FParametros.PorNome(AName, tpOutput);
end;

function TRpDataFlashCommand.GetCommand: string;
begin
  Result := Self.ClassName;
end;

function TRpDataFlashCommand.GetDescription: string;
begin
  Result := DoGetDescricao;
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

function TRpDataFlashCommand.GetParametro(const AName: string;
  const AParamType: TRpDataFlashParamType): TRpDataFlashCommandParameter;
begin
  Result := FParametros.GetParametro(AName, AParamType);
end;

function TRpDataFlashCommand.GetParams: TRpDataFlashCommandParameterList;
begin
  Result := FParametros;
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

function TRpDataFlashCommand.GetStatusProcessamento: TRpDataFlashProcessingStatus;
begin
//  Result := FStatusProcessamento;
  Result := FParametros.StatusProcessamento;
end;

function TRpDataFlashCommand.GetProcessType: TRpDataFlashProcessType;
begin
  Result := prtRemote;
end;

function TRpDataFlashCommand.InternalCarregarComando(
  const AComando: TRpDataFlashCommandClass;
  out AObjComando: IRpDataFlashCommandInterfaced;
  out AParams: TRpDataFlashCommandParameterList): Boolean;
var
  lComando: string;
begin
  // ver como fica esta parte, o comando para carregar n�o � o "nome" do comando
  // mas sim o XML que � recebido pelo server com todos os parametros
  if FSerealizationFormat = sfJSON then
  begin
    lComando := '{"Comando":"' + AComando.ClassName + '","Parametros":{"InternalStatusProcessamento":{"Tipo":2,"TipoValor":0,"Valor":"4"},'
              + '"internal_FormatType":{"Tipo":2,"TipoValor":0,"Valor":"2"},"exec_StatusRetorno":{"Tipo":2,"TipoValor":2,"Valor":"true"},'
              + '"exec_Exception":{"Tipo":2,"TipoValor":1,"Valor":""}}}';
  end
  else
  begin
    lComando := '<root><TCPComando Comando="' + AComando.ClassName + '"><Parametros>'
              + '<InternalStatusProcessamento Tipo="2" TipoValor="0">4</InternalStatusProcessamento>'
              + '<exec_StatusRetorno Tipo="2" TipoValor="2"></exec_StatusRetorno>'
              + '<exec_Exception Tipo="2" TipoValor="1"></exec_Exception></Parametros></TCPComando></root>';
  end;

  Result := CarregarComando(lComando, AObjComando, AParams,
    FServerInstanceController, FSessionInstanceController, False);

  if Assigned(AObjComando) then
  begin
    while AObjComando.Lock do
      Sleep(100);
  end;
end;

function TRpDataFlashCommand.LastError: string;
begin
  Result := FParametros.Retorno[C_PARAM_INT_EXCEPTION].AsString;
end;

procedure TRpDataFlashCommand.NovoParametro(const AName: string;
  const AParamType: TRpDataFlashParamValueType; const ARecarregar : Boolean);
begin
  if not ARecarregar then
    FParametros.Novo(AName, EmptyStr, tpInputNoReload, AParamType)
  else
    FParametros.Novo(AName, EmptyStr, tpInput, AParamType);
end;

procedure TRpDataFlashCommand.NovoRetorno(const AName: string;
  const AParamType: TRpDataFlashParamValueType);
begin
  FParametros.Novo(AName, EmptyStr, tpOutput, AParamType);
end;

function TRpDataFlashCommand.Serializar: string;
begin
  DoSerialize;
  Result := DoGetParametrosSerializados;
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

procedure TRpDataFlashCommand.SetConexaoItem(const AConexaoItem: IAutenticationProvider);
begin
  FConexaoItem := AConexaoItem;
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

procedure TRpDataFlashCommand.SetStatusProcessamento(const Value: TRpDataFlashProcessingStatus);
begin
  FParametros.StatusProcessamento := Value;
end;

function TRpDataFlashCommand.ReturnStatus: Boolean;
begin
  Result := FParametros.Retorno[C_PARAM_INT_STATUS_RET].AsBoolean;
end;

{ TLRDataFlashComandoEnvio }

function TLRDataFlashComandoEnvio.DoExecutar: Boolean;
begin
  Result := True;
end;

procedure TLRDataFlashComandoEnvio.SetComando(const AComando: string);
begin
  FComando := AComando;
  if Trim(FParametros.Command) = EmptyStr then
    FParametros.Command := Trim(FComando);
//  FParametros.Comando := AComando;
//  FComando := AComando;
end;

function TLRDataFlashComandoEnvio.GetCommand: string;
begin
  Result := FComando;
end;

procedure TLRDataFlashComandoEnvio.SetComando(const AComando: TRpDataFlashCommandClass);
begin
  SetComando(AComando.ClassName);
end;

procedure TRpDataFlashCommand.NovoParametro(const AName: string;
  const ABaseClass: TCustomSerializableObjectClass; const ARecarregar: Boolean);
begin
  if not ARecarregar then
    FParametros.Novo(AName, tpInputNoReload, ABaseClass.ClassName)
  else
    FParametros.Novo(AName, tpInput, ABaseClass.ClassName);
end;

procedure TRpDataFlashCommand.NovoParamInterno(const AName: string;
  const AParamType: TRpDataFlashParamValueType);
begin
  FParametros.Novo(AName, EmptyStr, tpInternal, AParamType);
end;

procedure TRpDataFlashCommand.NovoRetorno(const AName: string;
  const ABaseClass: TCustomSerializableObjectClass; const ARecarregar: Boolean);
begin
  FParametros.Novo(AName, tpOutput, ABaseClass.ClassName);
end;

{ TCPClassRegistrer }

class procedure TCPClassRegistrer.Destruir;
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

class procedure TCPClassRegistrer.Registrados(out ARegistrados: TTcpClassRegister;
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
        lItem.Mnemonico  := TTcpClassRegister.TcpClassRegister.Items[I].Mnemonico;
        ARegistrados.Add( lItem );
      end;
    end;
end;

class procedure TCPClassRegistrer.Registrar(const AClass : TRpDataFlashAbstractClass;
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

  TTcpClassRegister.TcpClassRegister.Registrar(AClass, lGrupoProxy, AMnemonico, APublico, ALifeCycle);
end;

class procedure TCPClassRegistrer.RegistrarDSProvider(const AClass: TRpDataFlashAbstractClass;
  const ALifeCycle : TRpDataFlashLifeCycle);
begin
  if TTcpClassRegister.TcpClassRegister = nil then
    TTcpClassRegister.TcpClassRegister := TTcpClassRegister.Create;
  TTcpClassRegister.TcpClassRegister.Registrar(AClass, C_GROUP_DATASET, '', False, ALifeCycle);
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
    if TRpDataFlashAbstractClass(Items[I].ProxyClass).ClassNameIs(AClassName) or (Items[I].Mnemonico = AClassName) then
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

procedure TTcpClassRegister.Registrar(const AClass : TRpDataFlashAbstractClass;
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
  lItem.Mnemonico  := AClass.ClassName;
  if AMnemonico <> '' then
    lItem.Mnemonico  := AMnemonico;
  Add(lItem);
end;

{ TLRDataFlashValorParametroInteger }

constructor TLRDataFlashValorParametroInteger.Create;
begin
  inherited;
  FValueType := tvpInteger;
end;

function TLRDataFlashValorParametroInteger.GetValue: Integer;
begin
  try
    Result := StrToIntDef(VarToStrDef(FValue, '0'), 0);
  except
    Result := 0;
  end;
end;

procedure TLRDataFlashValorParametroInteger.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

{ TLRDataFlashValorParametroString }

constructor TLRDataFlashValorParametroString.Create;
begin
  inherited;
  FValueType := tvpString;
end;

function TLRDataFlashValorParametroString.GetValue: String;
begin
  Result := VarToStrDef(FValue, EmptyStr);
end;

procedure TLRDataFlashValorParametroString.SetValue(const Value: String);
begin
  FValue := Value;
end;

{ TLRDataFlashValorParametroBoolean }

constructor TLRDataFlashValorParametroBoolean.Create;
begin
  inherited;
  FValueType := tvpBoolean;
end;

function TLRDataFlashValorParametroBoolean.GetValue: Boolean;
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

procedure TLRDataFlashValorParametroBoolean.SetValue(const Value: Boolean);
begin
  FValue := Value;
end;

function TRpDataFlashCommandParameterList.Novo(const AName: string;
  const AParamType : TRpDataFlashParamType;
  const AValueType : TLRDataFlashValorParametroClass): TLRDataFlashValorParametro;
begin
  Result := AValueType.Create;
  Result.FParamType := AParamType;
  Result.FName := AName;
  Novo(Result);
end;

{ TTcpClassRegisterItem }

function TTcpClassRegisterItem.GetProxyGroup: string;
begin
  Result := Trim( FProxyGroup );
  if Result = EmptyStr then
    Result := C_WITHOUT_GROUP;
end;

{ TLRDataFlashCustomDataSetProvider }

function TLRDataFlashComandoDataSetProvider.ApplyUpdates: Boolean;
var
  lStatements : TStringList;
  lTotal: Integer;
  i: Integer;
  lSQL: string;
begin
  lStatements := TStringList.Create;
  lStatements.Text := Parametro['SQLInstruct'].AsBase64;
  lTotal := StrToIntDef(Trim(lStatements[0]), 0);
  Result := False;
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
      raise Exception.Create('N�o existem valores para aplicar.');
  finally
    FreeAndNil( lStatements );
//    if Result then
//      DoCommit
//    else
//      DoRollback; //-> Client must finalize transaction
  end;
end;

constructor TLRDataFlashComandoDataSetProvider.Create;
begin
  inherited Create;
  FProviderClass := EmptyStr;
  FInfoQuery := False;
//  FParams := TDataSetParams.Create;
//  FSQLs := TLRDataFlashCustomProvider.Create;
end;

destructor TLRDataFlashComandoDataSetProvider.Destroy;
begin
//  FreeAndNil(FSQLs);
//  FreeAndNil(FParams);
  inherited;
end;

function TLRDataFlashComandoDataSetProvider.DoExecutar: Boolean;
var
  lXmlData: string;
begin
  FProviderClass := Parametro['ProviderClass'].AsString;
  FOperacao := TRpDataFlashDataSetOperations( Parametro['Operacao'].AsInteger );
  FApplyMaxErrors := Parametro['MaxErrors'].AsInteger;
  FInfoQuery := Parametro['csDesigning'].AsBoolean;

  // zera os retornos
  Retorno['ApplyErrCount'].AsInteger := 0;
  Retorno['XMLData'].AsBase64 := EmptyStr;

  // executa a operacao informada
  if FOperacao = opdsSelect then
  begin
    Result := Select(Parametro['SQLInstruct'].AsBase64, lXmlData);
    if Result then
      Retorno['XMLData'].AsBase64 := lXmlData;
  end
  else
    case FOperacao of
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

function TLRDataFlashComandoDataSetProvider.DoPrepareClient: Boolean;
var
  lSQL : TRpDataFlashCustomProvider;
begin
  lSQL := TRpDataFlashCustomProvider.Create;

  lSQL.SelectSQL.Text := GetSelectSQL;
  lSQL.InsertSQL.Text := GetInsertSQL;
  lSQL.UpdateSQL.Text := GetUpdateSQL;
  lSQL.DeleteSQL.Text := GetDeleteSQL;

  Retorno['XMLData'].AsBase64 := lSQL.GetAsString;
  Result := True;
  FreeAndNil(lSQL);
end;

procedure TLRDataFlashComandoDataSetProvider.DoRegisterParams;
begin
  inherited;
  NovoParametro('Operacao', tvpInteger); // INS / UPD / DEL / SEL
  NovoParametro('SQLInstruct', tvpBase64, False);
  NovoParametro('MaxErrors', tvpInteger);
  NovoParametro('Params', tvpBase64);
  NovoParametro('csDesigning', tvpBoolean);

  // ou a classe registrada no servidor
  NovoParametro('ProviderClass', tvpString);

  NovoRetorno('XMLData', tvpBase64);
  NovoRetorno('ApplyErrCount', tvpInteger);
end;

{ TSPITCPComandoTexto }

function TLRDataFlashComandoTexto.DoExecutar: Boolean;
begin
  Result := False;
  if Assigned(FOnExecutarMensagem) then
    Result := FOnExecutarMensagem(Self);
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

procedure TRpFileProxy.CopyInfo(const ASource: IFileProxy);
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

{ TLRDataFlashComandoBrowser }

function TLRDataFlashComandoBrowser.DoGetParametrosSerializados: string;
var
  lBrowserPage: string;
begin
  lBrowserPage := GetBrowserPage;
  if lBrowserPage = '' then
    Result := inherited DoGetParametrosSerializados
  else
    Result := lBrowserPage;
end;

function TLRDataFlashComandoBrowser.GetBrowserPage: string;
begin
  Result := '';
end;

initialization

finalization
  TCPClassRegistrer.Destruir;

end.
