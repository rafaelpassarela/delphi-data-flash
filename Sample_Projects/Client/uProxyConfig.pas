unit uProxyConfig;

interface

uses
  System.Classes, uRpDataFlash.Types, System.SysUtils,
  // don't do that, it's just for demonstrate ;)
  uMainClient, uRpSerialization;

type
  TProxyConfigDemo = class(TInterfacedObject, IRpDataFlashConfig)
  public
    function GetServerName: string;
    function GetServerPort: Integer;
    function GetRestPort: Integer;
    function GetFTPPort: Integer;
    function GetCommunicationType: TRpDataFlashCommunicationType;
    function GetEncryptionType: TRpDataFlashEncryptionType;
    function GetLocalHostToIP: Boolean;
    function GetPassword: string;
    function GetUserName: string;
    function GetSerializationFormat: TSerializationFormat;
  end;

implementation

{ TProxyConfigDemo }

function TProxyConfigDemo.GetCommunicationType: TRpDataFlashCommunicationType;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.CommunicationType;
end;

function TProxyConfigDemo.GetEncryptionType: TRpDataFlashEncryptionType;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.EncryptionType;
end;

function TProxyConfigDemo.GetFTPPort: Integer;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.FileTransfer.Port;
end;

function TProxyConfigDemo.GetLocalHostToIP: Boolean;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.ConvertLocalHostToIP;
end;

function TProxyConfigDemo.GetPassword: string;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.Password;
end;

function TProxyConfigDemo.GetRestPort: Integer;
begin
  Result := 0;
end;

function TProxyConfigDemo.GetSerializationFormat: TSerializationFormat;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.SerializationFormat;
end;

function TProxyConfigDemo.GetServerName: string;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.Server;
end;

function TProxyConfigDemo.GetServerPort: Integer;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.Port;
end;

function TProxyConfigDemo.GetUserName: string;
begin
  Result := FormMainClient.RpDataFlashClientConnectionTeste.UserName;
end;

end.
