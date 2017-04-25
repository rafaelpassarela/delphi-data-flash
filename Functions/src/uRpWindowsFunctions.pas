unit uRpWindowsFunctions;

{$I ..\Common\src\RpInc.inc}
//{$I C:\Rafael\RPack\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.SysUtils, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, Vcl.Dialogs, Winapi.WinSock, jpeg, System.Classes, Vcl.StdCtrls,
  Vcl.ExtCtrls, Xml.XMLIntf, Winapi.WinInet,
  {$ELSE}
  SysUtils, Windows, Messages, Controls, Graphics, Forms, Dialogs, Classes,
  StdCtrls, ExtCtrls, WinSock, jpeg, XMLIntf, WinInet,
  {$ENDIF}
  uRpStringFunctions, uRpTypes, IdHTTP;

const
  cUtilWindowExClass: TWndClass = (
    style: 0;
    lpfnWndProc: nil;
    cbClsExtra: 0;
    cbWndExtra: SizeOf(TMethod);
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TPUtilWindowEx');

type
  TIconType = (itError, itWarning, itInformation, itQuestion, itApplication, itNone);

  TScreenPosition = (spCenter, spBottonRight, spBottonLeft,
                     spTopRight, spTopLeft);

  TDimensions = packed record
    Width, Height: Integer;
    procedure FromRect(const ARect : TRect);
    procedure Reset;
  end;

  TIpArrayList = array of string;

  TRpWindows = class
  private
    class var ShellVersion: Integer;
  public
    class function GetShellVersion: Cardinal;
    class function GetWindowsVersionName : string;
    class function GetAnimation: Boolean;
    class function GetUserName : string;
    class function GetComputerName : string;
    class function GetIpList(const AHostName : string) : TIpArrayList; overload;
    class function GetIpAddress(const AHostName : string) : string;
    class procedure GetIpList(const AHostName : string; const AIPList : TStrings); overload;

    class function StdWndProc(Window: THandle; Message, WParam: WPARAM;
      LParam: LPARAM): LRESULT; stdcall;
    class function AllocateHWndEx(Method: TWndMethod;
      const AClassName: string = ''): THandle;

    class function IconeWindows(const AType : TIconType; const AIconImage : TIcon) : Boolean;
    class function DlgButtonToModalResul(const AButtonType: TMsgDlgBtn) : TModalResult;
    class function FileSize(const AFileName : string) : Int64;
    class function FileDate(const AFileName : string) : TDateTime;
    class function CheckInternetConnection(const ARaiseOnFalse : Boolean = False) : Boolean;

    class function GetWebPageContent(const AUrl : string; var AOutStream : TMemoryStream) : Boolean; overload;
    class function GetWebPageContent(const AUrl : string; var AOutStream : TStringStream) : Boolean; overload;
    class function GetWebPageContent(const AUrl : string; var AOutXmlDoc : IXMLDocument) : Boolean; overload;

    class procedure PrintScreen(const AFileName : TFileName; const AInsertCursor : Boolean;
      const AJPEGQuality : SmallInt = 50);
    class procedure SetAnimation(Value: Boolean);
    class procedure DeallocateHWndEx(Wnd: THandle);
    class procedure ShowWinNoAnimate(Handle: THandle; CmdShow: Integer);

    class function GetWinDir: string;
    class function GetSystemDir: string;
    class function GetTempDir: string;

    class function GetVersion(const AFileName: String; const AVersionFormat : String = '%d.%d.%d.%d'): String;
  end;

implementation

{ TRpWindows }

class function TRpWindows.AllocateHWndEx(Method: TWndMethod;
  const AClassName: string): THandle;
var
  TempClass: TWndClass;
  UtilWindowExClass: TWndClass;
  ClassRegistered: Boolean;
begin
  UtilWindowExClass := cUtilWindowExClass;
  UtilWindowExClass.hInstance := HInstance;
  UtilWindowExClass.lpfnWndProc := @DefWindowProc;
  if AClassName <> '' then
    UtilWindowExClass.lpszClassName := PChar(AClassName);

  ClassRegistered := {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.GetClassInfo(HInstance, UtilWindowExClass.lpszClassName, TempClass);

  if not ClassRegistered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    if ClassRegistered then
      {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.UnregisterClass(UtilWindowExClass.lpszClassName, HInstance);
    {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.RegisterClass(UtilWindowExClass);
  end;

  Result := {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.CreateWindowEx(
    {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.WS_EX_TOOLWINDOW,
    UtilWindowExClass.lpszClassName,
    '',
    {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.WS_POPUP,
    0, 0, 0, 0, 0, 0,
    HInstance,
    nil);

  if Assigned(Method) then
  begin
    SetWindowLongPtr(Result, 0, LONG_PTR(TMethod(Method).Code));
    SetWindowLongPtr(Result, SizeOf(TMethod(Method).Code), LONG_PTR(TMethod(Method).Data));
    SetWindowLongPtr(Result, GWL_WNDPROC, LONG_PTR(@TRpWindows.StdWndProc));
  end;
end;

class function TRpWindows.CheckInternetConnection(const ARaiseOnFalse : Boolean): Boolean;
begin
  Result := InternetCheckConnection('http://www.google.com/', 1, 0);
  if (not Result) and ARaiseOnFalse then
    raise ERpNoInternetConnection.Create('Não foi possível localizar uma conexão com a internet.');
end;

class procedure TRpWindows.DeallocateHWndEx(Wnd: THandle);
begin
  {$IFDEF XE3UP}Winapi.{$ENDIF}Windows.DestroyWindow(Wnd);
end;

class function TRpWindows.DlgButtonToModalResul(
  const AButtonType: TMsgDlgBtn): TModalResult;
begin
  case AButtonType of
    mbYes:     Result := mrYes;
    mbNo:      Result := mrNo;
    mbOK:      Result := mrOk;
    mbCancel:  Result := mrCancel;
    mbAbort:   Result := mrAbort;
    mbRetry:   Result := mrRetry;
    mbIgnore:  Result := mrIgnore;
    mbAll:     Result := mrAll;
    mbNoToAll: Result := mrNoToAll;
    mbYesToAll:Result := mrYesToAll ;
    mbClose:   Result := mrClose;
  else
    Result := mrNone; // mbHelp
  end;
end;

class function TRpWindows.FileDate(const AFileName: string): TDateTime;
var
  lFileAtr: TWin32FileAttributeData;
  SystemTime, LocalTime: TSystemTime;
begin
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @lFileAtr) 
  and FileTimeToSystemTime(lFileAtr.ftCreationTime, SystemTime) and SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime) then
    Result := SystemTimeToDateTime(LocalTime)
  else
    Result := 0;  
end;

class function TRpWindows.FileSize(const AFileName: string): Int64;
var
  lInfo: TWin32FileAttributeData;
begin
  Result := -1;

  if not GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @lInfo) then
    Exit;

  Result := lInfo.nFileSizeLow or (lInfo.nFileSizeHigh shl 32);
end;

class function TRpWindows.GetAnimation: Boolean;
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(Info);
  if SystemParametersInfo(SPI_GETANIMATION, Info.cbSize, @Info, 0) then
    Result := Info.iMinAnimate <> 0
  else
    Result := False;
end;

class function TRpWindows.GetComputerName: string;
var
  Count: DWORD;
begin
  Count := MAX_COMPUTERNAME_LENGTH + 1;
  // set buffer size to MAX_COMPUTERNAME_LENGTH + 2 characters for safety
  { TODO : Win2k solution }
  SetLength(Result, Count);

  if {$IFDEF XE3UP}Winapi.Windows.{$ELSE}Windows.{$ENDIF}GetComputerName(PChar(Result), Count) then
    TRpStrings.ResetLength(Result)
  else
    Result := '';
end;

class function TRpWindows.GetIpAddress(const AHostName: string): string;
var
  lList : TIpArrayList;
  i: Integer;
begin
  Result := '';

  lList := TRpWindows.GetIpList(AHostName);
  for i := 0 to High(lList) do
  begin
    if lList[i] <> '127.0.0.1' then
    begin
      Result := lList[i];
      Break;
    end;
  end;
end;

class procedure TRpWindows.GetIpList(const AHostName: string;
  const AIPList: TStrings);
var
  lList : TIpArrayList;
  i: Integer;
begin
  lList := TRpWindows.GetIpList(AHostName);

  for i := 0 to High(lList) do
    AIPList.Add(lList[i]);
end;

class function TRpWindows.GetIpList(const AHostName: string): TIpArrayList;
type
  TaPInAddr = array[0..10] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe: PHostEnt;
  pptr: PaPInAddr;
  Buffer: array[0..255] of AnsiChar; //array[0..63] of char;
  I: Integer;
  GInitData: TWSADATA;
begin
  WSAStartup($101, GInitData);
//  GetHostName(Buffer, SizeOf(Buffer));
  for i := 0 to High(Buffer) do
    Buffer[i] := #0;

  for i := 1 to Length(AHostName) do
    Buffer[i - 1] := AnsiChar(AHostName[i]);

  phe := GetHostByName( Buffer );
  if phe = nil then
    Exit;
  pptr := PaPInAddr(Phe^.h_addr_list);
  I := 0;
  while pptr^[I] <> nil do
  begin
    SetLength(Result, i + 1);
    Result[i] := String( StrPas(inet_ntoa(pptr^[I]^)) );
    Inc(I);
  end;
  WSACleanup;
end;

class function TRpWindows.GetShellVersion: Cardinal;
begin
  if TRpWindows.ShellVersion = 0 then
    TRpWindows.ShellVersion := GetFileVersion('shell32.dll');
  Result := TRpWindows.ShellVersion;
end;

class function TRpWindows.GetSystemDir: string;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetSystemDirectory(dir, MAX_PATH);
  Result := IncludeTrailingPathDelimiter( StrPas(dir) );
end;

class function TRpWindows.GetTempDir: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(SizeOf(Buffer) - 1, Buffer);
  Result := StrPas(Buffer);
end;

class function TRpWindows.GetUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  { TODO : Win2k solution }
  SetLength(Result, Count);
  if {$IFDEF XE3UP}Winapi.Windows.{$ELSE}Windows.{$ENDIF}GetUserName(PChar(Result), Count) then
    TRpStrings.ResetLength(Result)
  else
    Result := '';
end;

class function TRpWindows.GetVersion(const AFileName, AVersionFormat: String): String;
////////////////////////////////////////////////////////////////
// this function reads the file ressource of "FileName" and returns the version
// number as formatted text. in "Fmt" you can use at most four integer values.
// if "Fmt" is invalid, the function may raise an EConvertError exception.
// examples for "Fmt" with versioninfo 4.3.128.0:
// '%d.%d.%d.%d' => '4.13.128.0'
// '%.2d-%.2d-%.2d' => '04-13-128'
//
var iBufferSize, iDummy : DWORD;
    pBuffer, pFileInfo  : Pointer;
    iVer : Array[1..4] of Word;
begin
  Result := '';
  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(AFileName), iDummy);
  if (iBufferSize > 0) then
  begin
    GetMem(pBuffer, iBufferSize);
    try
      // get fixed file info
      GetFileVersionInfo(PChar(AFileName), 0, iBufferSize, pBuffer);
      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
      // read version blocks
      iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
      iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
      iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
    finally
      FreeMem(pBuffer);
    end;
    // format result string
    Result := Format(AVersionFormat, [iVer[1], iVer[2], iVer[3], iVer[4]]);
  end;
end;

class function TRpWindows.GetWebPageContent(const AUrl: string;
  var AOutXmlDoc: IXMLDocument): Boolean;
var
  lStrStream : TStringStream;
begin
  try
    lStrStream := TStringStream.Create;
    Result := TRpWindows.GetWebPageContent(AUrl, lStrStream);
    if Result then
    begin
      AOutXmlDoc.LoadFromXML( lStrStream.DataString );
      AOutXmlDoc.Encoding := 'iso-8859-1';
    end;
  finally
    FreeAndNil( lStrStream );
  end;
end;

class function TRpWindows.GetWinDir: string;
var
  dir: array [0..MAX_PATH] of Char;
begin
  GetWindowsDirectory(dir, MAX_PATH);
  Result := IncludeTrailingPathDelimiter( StrPas(dir) );
end;

class function TRpWindows.GetWindowsVersionName: string;
var
  lVersion : TOSVersionInfo;
begin
  lVersion.dwOSVersionInfoSize := SizeOf(lVersion);
  GetVersionEx(lVersion);
  Result := '';
  case lVersion.dwPlatformId of
    1:
      case lVersion.dwMinorVersion of
         0: Result := 'Windows 95';
        10: Result := 'Windows 98';
        90: Result := 'Windows Me';
      end;
    2:
      case lVersion.dwMajorVersion of
        3: Result := 'Windows NT 3.51';
        4: Result := 'Windows NT 4.0';
        5: case lVersion.dwMinorVersion of
             0: Result := 'Windows 2000';
             1: Result := 'Windows XP';
             2: Result := 'Windows Server 2003';
           end;
        6: case lVersion.dwMinorVersion of
             0 : Result := 'Windows Vista';
             1 : Result := 'Windows 7';
             2 : Result := 'Windows 8';
             3 : Result := 'Windows 8.1';
           end;
        10: Result := 'Windows 10';
        else
          Result := 'Windows 10 ou Superior';
      end;
  end;

  if (Result = '') then
    Result := 'Sistema operacional desconhecido.'
  else
    Result := Result + ' ' + Trim(lVersion.szCSDVersion);
end;

class function TRpWindows.GetWebPageContent(const AUrl: string;
  var AOutStream: TMemoryStream): Boolean;
var
  lHTTP: TIdHTTP;
begin
  Result := False;

  if TRpWindows.CheckInternetConnection(True) then
  begin
    lHTTP := TIdHTTP.Create;
    try
      lHTTP.Request.ContentEncoding := 'ISO-8859-1';
      lHTTP.Request.CharSet := 'ISO-8859-1';
      lHTTP.Request.UserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV2';
      try
        lHTTP.Get(AUrl, AOutStream);
        AOutStream.Position := 0;
        Result := True;
      except
        on E: Exception do
        begin
          Result := False;
          raise Exception.Create('Não foi possível realizar a consulta. ' + E.Message);
        end;
      end;
    finally
      FreeAndNil(lHTTP);
    end;
  end;
end;

class function TRpWindows.GetWebPageContent(const AUrl: string;
  var AOutStream: TStringStream): Boolean;
var
  lMemStream : TMemoryStream;
begin
  Result := False;
  lMemStream := TMemoryStream.Create;
  try
    if TRpWindows.GetWebPageContent(AUrl, lMemStream) then
    begin
      AOutStream.CopyFrom(lMemStream, lMemStream.Size);
      Result := True;
    end;
  finally
    FreeAndNil( lMemStream );
  end;
end;

class function TRpWindows.IconeWindows(const AType: TIconType;
  const AIconImage: TIcon): Boolean;
var
  Icone : PChar;
begin
  if Assigned(AIconImage) then
  begin
    try
      case AType of
        itError:       Icone := IDI_ERROR;
        itWarning:     Icone := IDI_WARNING;
        itInformation: Icone := IDI_INFORMATION;
        itQuestion:    Icone := IDI_QUESTION;
      else
        Icone := '';
      end;
      if AType = itApplication then
        AIconImage.Handle := Application.Icon.Handle
      else
        AIconImage.Handle := LoadIcon(0, Icone);
      Result := True;
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

class procedure TRpWindows.PrintScreen(const AFileName: TFileName;
  const AInsertCursor: Boolean; const AJPEGQuality : SmallInt);
var
//  lIni : TIniFile;
  lTaxaJpg : Integer;
  lJPG : TJPEGImage;
  R : TRect;
  DC : HDc;
  Canv : TCanvas;
  MyFormat : Word;
  AData : {$IFDEF XE3UP} NativeUInt {$ELSE} Cardinal {$ENDIF};
  APalette : HPALETTE;
//  lNomeImg: String;

  { adiciona o cursor no paint screen no momento do erro }
  procedure DrawCursor(ScreenShotBitmap : TBitmap);
  var
    r: TRect;
    CI: TCursorInfo;
    Icon: TIcon;
    II: TIconInfo;
  begin
    r := ScreenShotBitmap.Canvas.ClipRect;
    Icon := TIcon.Create;
    try
      CI.cbSize := SizeOf(CI);
      if GetCursorInfo(CI) then
        if CI.Flags = CURSOR_SHOWING then
        begin
          Icon.Handle := CopyIcon(CI.hCursor);
          if GetIconInfo(Icon.Handle, II) then
          begin
            ScreenShotBitmap.Canvas.Draw(
                  ci.ptScreenPos.x - Integer(II.xHotspot) - r.Left,
                  ci.ptScreenPos.y - Integer(II.yHotspot) - r.Top,
                  Icon);
          end;
        end;
    finally
      Icon.Free;
    end;
  end;

begin
  { taxa de compactacao do jpg }
  lTaxaJpg := 100 - AJPEGQuality;
  if (lTaxaJpg <= 0) or (lTaxaJpg > 100) then
    lTaxaJpg := 50;
      
  { Tira PaintScreen }
  keybd_event(vk_snapshot,0, 0, 0); // tecla do paint screen

  with TImage.Create(Application) do
  try
    lJPG := TJPEGImage.Create;
    Width  := Screen.Width;
    Height := Screen.Height;

    R := Rect( 0, 0, Screen.Width, Screen.Height );
    DC := GetWindowDC( GetDeskTopWindow );
    Canv := TCanvas.Create;
    Canv.Handle := DC;
    Canvas.CopyRect( R, Canv, R );
    ReleaseDC( GetDeskTopWindow, DC );
    DrawCursor(Picture.Bitmap);
    Picture.SaveToClipboardFormat(MyFormat, AData, APalette);
    lJPG.CompressionQuality := lTaxaJpg;
    lJPG.Assign(Picture.Bitmap);
    lJPG.Compress;
    lJPG.SaveToFile( AFileName );
  finally
    FreeAndNil(lJPG);
    Free;
  end;
end;

class procedure TRpWindows.SetAnimation(Value: Boolean);
var
  Info: TAnimationInfo;
begin
  Info.cbSize := SizeOf(Info);
  Info.iMinAnimate := Ord(Value);
  SystemParametersInfo(SPI_SETANIMATION, Info.cbSize, @Info, 0);
end;

class procedure TRpWindows.ShowWinNoAnimate(Handle: THandle; CmdShow: Integer);
var
  Animation: Boolean;
begin
  Animation := TRpWindows.GetAnimation;
  if Animation then
    SetAnimation(False);

  ShowWindow(Handle, CmdShow);

  if Animation then
    SetAnimation(True);
end;

class function TRpWindows.StdWndProc(Window: THandle; Message, WParam: WPARAM;
  LParam: LPARAM): LRESULT;
var
  Msg: {$IFDEF XE3UP}Winapi.{$ENDIF}Messages.TMessage;
  WndProc: TWndMethod;
begin
  TMethod(WndProc).Code := Pointer(GetWindowLongPtr(Window, 0));
  TMethod(WndProc).Data := Pointer(GetWindowLongPtr(Window, SizeOf(Pointer)));
  if Assigned(WndProc) then
  begin
    Msg.Msg := Message;
    Msg.WParam := WParam;
    Msg.LParam := LParam;
    Msg.Result := 0;
    WndProc(Msg);
    Result := Msg.Result;
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end;

{ TDimensions }

procedure TDimensions.FromRect(const ARect: TRect);
begin
  Self.Width := ARect.Right;
  Self.Height := ARect.Bottom;
end;

procedure TDimensions.Reset;
begin
  Width := 0;
  Height := 0;
end;

initialization
  TRpWindows.ShellVersion := 0;

finalization // dummy

end.
