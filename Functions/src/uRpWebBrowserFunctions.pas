unit uRpWebBrowserFunctions;

interface

uses
  SHDocVw, Winapi.ActiveX, System.Classes, System.SysUtils, Winapi.WinInet, jpeg,
  System.Types, Winapi.Windows;

type
  TWebBrowserHelper = class helper for TWebBrowser
  public
    function GetHTML : string;
  end;

  TRpWebFunctions = class
  public
    class function SaveImageURLToFile(const AImageUrl : string; const AFileName : string) : Boolean;
    class function SaveImageURLToStream(const AImageUrl : string; const AStream : TStream) : Boolean;
  end;

implementation

{ TWebBrowserHelper }

function TWebBrowserHelper.GetHTML: string;
var
  lStreamIntf : IPersistStreamInit;
  lStringStream : TStringStream;
  lStream: IStream;
begin
  Result := '';
  if Assigned(Self.Document) then
  begin
    Self.Document.QueryInterface(IPersistStreamInit, lStreamIntf);
    if lStreamIntf <> nil then
    begin
      lStringStream := TStringStream.Create;
      try
        lStream := TStreamAdapter.Create(lStringStream, soReference) as IStream;
        lStreamIntf.Save(lStream, True);

        Result := lStringStream.DataString;
      finally
        if Assigned(lStringStream) then
          FreeAndNil(lStringStream);
        lStreamIntf := nil;
        lStream := nil;
      end;
    end;
  end;
end;

{ TRpWebFunctions }

class function TRpWebFunctions.SaveImageURLToFile(const AImageUrl, AFileName: string): Boolean;
var
  lBufferSize: DWORD;
  lCacheEntry: PInternetCacheEntryInfo;
begin
  Result := False;

  GetMem(lCacheEntry, lBufferSize);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    if RetrieveUrlCacheEntryFile(PChar(AImageUrl), lCacheEntry^, lBufferSize, 0) then
    try
      Win32Check(CopyFile(lCacheEntry.lpszLocalFileName, PChar(AFileName), False));
      Result := True;
    finally
      Win32Check(UnlockUrlCacheEntryFile(PChar(AImageUrl), 0));
    end;
//    else
//      RaiseLastOSError;
    {$WARN SYMBOL_PLATFORM ON}
  finally
    FreeMem(lCacheEntry);
  end;
end;

class function TRpWebFunctions.SaveImageURLToStream(const AImageUrl: string; const AStream: TStream): Boolean;
var
  lBufferSize: DWORD;
  lCacheEntry: PInternetCacheEntryInfo;
  lMemStream : TMemoryStream;
begin
  Result := False;

  GetMem(lCacheEntry, lBufferSize);
  try
    {$WARN SYMBOL_PLATFORM OFF}
    if RetrieveUrlCacheEntryFile(PChar(AImageUrl), lCacheEntry^, lBufferSize, 0) then
    try
      lMemStream := TMemoryStream.Create;
      try
        lMemStream.LoadFromFile(lCacheEntry.lpszLocalFileName);
        lMemStream.Position := 0;

        AStream.CopyFrom(lMemStream, lMemStream.Size);
        Result := True;
      finally
        if Assigned(lMemStream) then
          FreeAndNil(lMemStream);
      end;
    finally
      Win32Check(UnlockUrlCacheEntryFile(PChar(AImageUrl), 0));
    end;
//    else
//      RaiseLastOSError;
    {$WARN SYMBOL_PLATFORM ON}
  finally
    FreeMem(lCacheEntry);
  end;
end;

end.
