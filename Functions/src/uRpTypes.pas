unit uRpTypes;

{$I ..\..\Common\src\RpInc.inc}

interface

uses
  {$IFDEF XE3UP}
  System.SysUtils,
  {$ELSE}
  SysUtils,
  {$ENDIF}
  StdCtrls, XMLIntf, uRpFileHelper;

type
  ERpNoInternetConnection = class(Exception);

  TRpCharCase = (
    ccNormal = Ord(ecNormal),
    ccUpperCase = Ord(ecUpperCase),
    ccLowerCase = Ord(ecLowerCase),
    ccFirstUpper);

  IRpFileSupport = uRpFileHelper.IRpFileSupport;

implementation

end.

