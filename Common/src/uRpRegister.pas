unit uRpRegister;

{$I RpInc.inc}

interface

uses
  {$IFDEF XE3UP} Vcl.Graphics, Winapi.Windows, {$ELSE} Graphics, Windows, {$ENDIF}
  ToolsAPI, DesignIntf, SysUtils;

implementation

{$R RpBdsSplash.res}

const
  ICON_SPLASH = 'RAFA_BDS_SPLASH';
  ICON_ABOUT = 'ABOUTICON';

var
  AboutBoxServices: IOTAAboutBoxServices = nil;
  AboutBoxIndex: Integer = 0;

resourcestring
  resPackageName = 'RafaPack 2015';
  resLicense = 'rafaelpassarela@gmail.com';
  resAboutCopyright = 'Rafael Passarela';
  resAboutTitle = 'Delphi Components - RafaPack';
  resAboutDescription = 'www.rafaelpassarela.com.br ;)';

procedure RegisterSplashScreen;
var
  bmp: {$IFDEF XE3UP} Vcl.Graphics.TBitmap {$ELSE} Graphics.TBitmap {$ENDIF};
begin
  ForceDemandLoadState(dlDisable);
  bmp := {$IFDEF XE3UP} Vcl.Graphics.TBitmap.Create {$ELSE} Graphics.TBitmap.Create {$ENDIF};
  try
    bmp.LoadFromResourceName(HInstance, ICON_SPLASH);
    SplashScreenServices.AddPluginBitmap(resPackageName, bmp.Handle, False, resLicense);
  finally
    bmp.Free;
  end;
end;

procedure RegisterAboutBox;
var
  ProductImage: HBITMAP;
begin
  Supports(BorlandIDEServices,IOTAAboutBoxServices, AboutBoxServices);
  ProductImage := LoadBitmap(FindResourceHInstance(HInstance), ICON_ABOUT);
  AboutBoxIndex := AboutBoxServices.AddPluginInfo(
    resPackageName,
    resAboutCopyright + #13#10#13#10 + resAboutDescription,
    ProductImage, False, resLicense);
end;

procedure UnregisterAboutBox;
begin
  if (AboutBoxIndex <> 0) and Assigned(AboutBoxServices) then
  begin
    AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
    AboutBoxIndex := 0;
    AboutBoxServices := nil;
  end;
end;

initialization
  RegisterSplashScreen;
  RegisterAboutBox;

finalization
  UnRegisterAboutBox;

(*
// have a 24x24 icon
{$R RpBdsSplash.res}

function BitmapFromResource(const ABitmapName: string): TBitmap;
begin
  Result := Vcl.Graphics.TBitmap.Create;
  Result.LoadFromResourceName(hInstance, ABitmapName);
end;


procedure Register;
begin

end;

initialization
  // Force the IDE to load this package if it has made the decision to demand-load it.
  // You may want to put you splash screen icon in its own package to avoid the overhead
  // of loading all your components on start-up.
  ForceDemandLoadState(dlDisable); // In DesignIntf

  SplashScreenServices.AddProductBitmap(
    'RafaPack - XE3',
    BitmapFromResource('RAFA_BDS_SPLASH').Handle,
    False,
    'FreeForUse',
    'rafaelpassarela@gmail.com');

  // Use the ToolsAPI to add the icon and caption. Read the comments for
  // IOTASplashScreenServices in ToolsAPI for information on BDS personalities.
//  SplashScreenServices.AddProductBitmap('Your own library in the BDS splash form',
//    BitmapFromResource('BDSSPLASHDEMO').Handle); // UPPERCASE resource name

  // Add another one to demo the optional parameters
//  SplashScreenServices.AddProductBitmap('Unregistered Library!',
//    BitmapFromResource('BDSSPLASHDEMO').Handle, true, 'Some licencing info',
//    '[Text appended after caption]');
  *)
end.
